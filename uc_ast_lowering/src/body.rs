use std::str::FromStr;

use uc_def::{ArgFlags, FuncFlags, Op};
use uc_files::Span;
use uc_middle::{
    body::{
        Block, BlockId, Body, DynArrayOpKind, Expr, ExprId, ExprKind, ExprTy, Literal,
        LoopDesugaring, PlaceExprKind, Receiver, Statement, StatementKind, StmtId, ValueExprKind,
    },
    ty::{self, Ty},
    ClassKind, ConstVal, DefId, FuncSig, ScopeWalkKind,
};
use uc_name::Identifier;

use crate::LoweringContext;

struct FuncLowerer<'hir, 'a: 'hir> {
    ctx: &'a LoweringContext<'hir>,
    body: Body,
    body_scope: DefId,
    ret_ty: Option<Ty>,
    class_did: DefId,
    class_ty: Ty,
    self_ty: Option<Ty>,
}

#[derive(Debug)]
pub struct BodyError {
    pub kind: BodyErrorKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
enum TypeExpectation {
    /// lower_expr will check that the expression type
    /// is explicitly convertible to `Ty`, but not affect
    /// type inference in the expression itself, and emit
    /// a cast if necessary.
    CoerceToTy(Ty),
    /// lower_expr will affect inner type inference, check
    /// for an implicit conversion and apply it if necessary.
    RequiredTy(Ty),
    /// lower_expr will not affect inner type inference
    /// except for nudging the leftmost expression towards
    /// a different literal kind.
    HintTy(Ty),
    None,
}

#[derive(Debug)]
pub enum BodyErrorKind {
    /// An expression evaluating to no type at all was found where a type was expected.
    VoidType { expected: Option<Ty> },
    /// Indexing into not-an-array.
    NonArrayType { found: Ty },
    /// New-ing or iterating without class type
    NonClassType { found: Ty },
    /// An incompatible type was found.
    TyMismatch { expected: Ty, found: Ty },
    /// A compatible type that requires an explicit cast was found.
    MissingCast { to: Ty, from: Ty },
    /// An unneeded cast was found
    UnnecessaryCast { to: Ty, from: Ty },
    /// An invalid cast was found
    InvalidCast { to: Ty, from: Ty },
    /// Invalid number of arguments
    ArgCountError { expected: u32, got: u32 },
    /// Function not found
    FuncNotFound { name: Identifier },
    /// Freestanding symbol not found
    SymNotFound { name: Identifier },
    /// Self not allowed
    InvalidSelfAccess,
    /// Non-optional arg missing
    MissingNonOptional,
    /// An out/by-ref argument was not a place
    ByRefArgNotPlace,
    /// class argument to coerce function wasn't class
    BadCoerceArg,
    /// Multiple matching ops
    MultipleMatchingOps,
    /// No matching op
    NoMatchingOps,
    /// Before a default/static/const
    MissingClassLit,
    /// We're not sure about delegates
    DubiousDelegateBinding,
    /// Not a supported access context
    BadContext,
    /// Accessing op as func?
    AccessOpAsFunc,
    /// TODO
    NotYetImplemented(&'static str),
}

#[derive(Debug)]
enum AccessContext {
    Static(DefId),
    Const(DefId),
    Default(DefId),
    Expr(ExprId),
}

#[derive(Debug)]
enum NativeIteratorKind {
    Array(ExprId),
    Func(Receiver, DefId),
}

impl<'hir> LoweringContext<'hir> {
    pub fn lower_body(
        &self,
        body_scope: DefId,
        statements: &[uc_ast::Statement],
    ) -> Result<Body, BodyError> {
        let class_did = self.defs.get_item_class(body_scope);
        let class_ty = self.defs.get_class(class_did).self_ty;
        let (self_ty, ret_ty) = match &self.defs.get_def(body_scope).kind {
            uc_middle::DefKind::Operator(o) => {
                assert!(o.flags.contains(FuncFlags::STATIC));
                (None, o.sig.as_ref().unwrap().ret_ty)
            }
            uc_middle::DefKind::Function(f) => {
                let self_ty = if f.flags.contains(FuncFlags::STATIC) {
                    None
                } else {
                    Some(class_ty)
                };
                (self_ty, f.sig.as_ref().unwrap().ret_ty)
            }
            uc_middle::DefKind::State(_) => todo!(),
            _ => unreachable!(),
        };

        let lowerer = FuncLowerer {
            ctx: self,
            body: Body::new(),
            body_scope,
            ret_ty,
            class_ty,
            class_did,
            self_ty,
        };
        lowerer.lower_body(statements)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TyConversionCost {
    Same,
    Generalization(u16),
    Expansion,
    IntToFloat,
    Truncation,
    Disallowed,
}

impl<'hir, 'a> FuncLowerer<'hir, 'a> {
    fn lower_body(mut self, statements: &'hir [uc_ast::Statement]) -> Result<Body, BodyError> {
        let mut lowered_stmts = Vec::with_capacity(statements.len());
        for stmt in statements {
            lowered_stmts.push(self.lower_statement(stmt)?);
        }

        let entry = self.body.add_block(Block {
            stmts: lowered_stmts.into_boxed_slice(),
            span: None,
        });
        self.body.set_entry(entry);
        Ok(self.body)
    }

    fn lower_block(&mut self, block: &'hir uc_ast::Block) -> Result<BlockId, BodyError> {
        let stmts = self.lower_statements(&block.stmts)?.into_boxed_slice();
        Ok(self.body.add_block(Block { stmts, span: None })) // TODO
    }

    fn lower_statements(
        &mut self,
        stmts: &'hir [uc_ast::Statement],
    ) -> Result<Vec<StmtId>, BodyError> {
        stmts
            .iter()
            .map(|stmt| self.lower_statement(stmt))
            .collect::<Result<Vec<_>, _>>()
    }

    fn lower_statement(&mut self, stmt: &'hir uc_ast::Statement) -> Result<StmtId, BodyError> {
        let kind = match &stmt.kind {
            uc_ast::StatementKind::IfStatement {
                cond,
                then,
                or_else,
            } => {
                let e = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL), false)?;
                let then = self.lower_block(then)?;
                let or_else = or_else.as_ref().map(|b| self.lower_block(b)).transpose()?;
                StatementKind::If(e, then, or_else)
            }
            uc_ast::StatementKind::ForStatement {
                init,
                cond,
                retry,
                run,
            } => {
                let i = self.lower_statement(init)?;
                let c = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL), false)?;
                let r = self.lower_statement(retry)?;
                let mut inner_stmts = self.lower_statements(&run.stmts)?;
                let break_stmt = self.body.add_stmt(Statement {
                    kind: StatementKind::Break,
                    span: Some(cond.span),
                });
                let break_block = self.body.add_block(Block {
                    stmts: Box::new([break_stmt]),
                    span: Some(cond.span),
                });
                let exit = self.body.add_stmt(Statement {
                    kind: StatementKind::If(c, break_block, None),
                    span: Some(cond.span),
                });
                inner_stmts.insert(0, exit);

                let whole_block = self.body.add_block(Block {
                    stmts: inner_stmts.into_boxed_slice(),
                    span: Some(stmt.span),
                });

                StatementKind::Loop(
                    Some(i),
                    Some(r),
                    whole_block,
                    LoopDesugaring::For {
                        init: i,
                        cond: c,
                        retry: r,
                    },
                )
            }
            uc_ast::StatementKind::ForeachStatement { source, run } => {
                self.lower_foreach(source, run)?
            }
            uc_ast::StatementKind::WhileStatement { cond, run } => {
                let c = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL), false)?;
                let mut inner_stmts = self.lower_statements(&run.stmts)?;
                let break_stmt = self.body.add_stmt(Statement {
                    kind: StatementKind::Break,
                    span: Some(cond.span),
                });
                let break_block = self.body.add_block(Block {
                    stmts: Box::new([break_stmt]),
                    span: Some(cond.span),
                });
                let exit = self.body.add_stmt(Statement {
                    kind: StatementKind::If(c, break_block, None),
                    span: Some(cond.span),
                });
                inner_stmts.insert(0, exit);

                let whole_block = self.body.add_block(Block {
                    stmts: inner_stmts.into_boxed_slice(),
                    span: Some(stmt.span),
                });

                StatementKind::Loop(None, None, whole_block, LoopDesugaring::While { cond: c })
            }
            uc_ast::StatementKind::DoStatement { cond, run } => {
                let c = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL), false)?;
                let mut inner_stmts = self.lower_statements(&run.stmts)?;
                let break_stmt = self.body.add_stmt(Statement {
                    kind: StatementKind::Break,
                    span: Some(cond.span),
                });
                let break_block = self.body.add_block(Block {
                    stmts: Box::new([break_stmt]),
                    span: Some(cond.span),
                });
                let exit = self.body.add_stmt(Statement {
                    kind: StatementKind::If(c, break_block, None),
                    span: Some(cond.span),
                });
                inner_stmts.push(exit);

                let whole_block = self.body.add_block(Block {
                    stmts: inner_stmts.into_boxed_slice(),
                    span: Some(stmt.span),
                });

                StatementKind::Loop(None, None, whole_block, LoopDesugaring::Do { cond: c })
            }
            uc_ast::StatementKind::SwitchStatement { scrutinee, cases } => {
                let scrut = self.lower_expr(scrutinee, TypeExpectation::None, false)?;
                let scrut_ty = self.body.get_expr_ty(scrut).ty_or(BodyError {
                    kind: BodyErrorKind::VoidType { expected: None },
                    span: scrutinee.span,
                })?;
                let mut default = None;
                let mut clauses = vec![];
                let mut stmts = vec![];
                for c in cases {
                    match &c.case {
                        uc_ast::Case::Case(c) => {
                            let expr =
                                self.lower_expr(c, TypeExpectation::RequiredTy(scrut_ty), false)?;
                            clauses.push((expr, stmts.len().try_into().unwrap()))
                        }
                        uc_ast::Case::Default => {
                            assert!(default.is_none());
                            default = Some(stmts.len().try_into().unwrap())
                        }
                    }
                    for s in &c.stmts {
                        stmts.push(self.lower_statement(s)?);
                    }
                }

                let block = self.body.add_block(Block {
                    stmts: stmts.into_boxed_slice(),
                    span: None,
                }); // TODO

                StatementKind::Switch(scrut, clauses.into_boxed_slice(), default, block)
            }
            uc_ast::StatementKind::BreakStatement => StatementKind::Break,
            uc_ast::StatementKind::ContinueStatement => StatementKind::Continue,
            uc_ast::StatementKind::ReturnStatement { expr } => {
                let ret_ty = match self.ret_ty {
                    Some(t) => TypeExpectation::RequiredTy(t),
                    None => TypeExpectation::None,
                };
                let e = expr
                    .as_ref()
                    .map(|e| self.lower_expr(e, ret_ty, false))
                    .transpose()?;
                StatementKind::Return(e)
            }
            uc_ast::StatementKind::Label { name } => {
                return Err(BodyError {
                    kind: BodyErrorKind::NotYetImplemented("label"),
                    span: stmt.span,
                })
            }
            uc_ast::StatementKind::Assignment { lhs, rhs } => {
                let l = self.lower_expr(lhs, TypeExpectation::None, true)?;
                let l_ty = self.body.get_expr(l).ty.ty_or(BodyError {
                    kind: BodyErrorKind::VoidType { expected: None },
                    span: lhs.span,
                })?;
                assert!(!l_ty.is_stat_array());
                let r = self.lower_expr(rhs, TypeExpectation::RequiredTy(l_ty), false)?;
                StatementKind::Assign(l, r)
            }
            uc_ast::StatementKind::Expr { expr } => {
                StatementKind::Expr(self.lower_expr(expr, TypeExpectation::None, false)?)
            }
        };
        Ok(self.body.add_stmt(Statement {
            kind,
            span: Some(stmt.span),
        }))
    }

    fn try_bare_var_access(
        &mut self,
        name: &Identifier,
        span: Span,
    ) -> Result<Option<(ExprKind, ExprTy)>, BodyError> {
        if let Ok(var) = self.ctx.resolver.get_scoped_var(
            self.body_scope,
            self.ctx.defs,
            ScopeWalkKind::Access,
            name,
        ) {
            let def = self.ctx.defs.get_def(var);
            match &def.kind {
                uc_middle::DefKind::Var(var) => {
                    let s = self.body.add_expr(Expr {
                        ty: ExprTy::Ty(self.self_ty.ok_or(BodyError {
                            kind: BodyErrorKind::InvalidSelfAccess,
                            span,
                        })?),
                        kind: ExprKind::Place(PlaceExprKind::SelfAccess),
                        span: None,
                    });
                    Ok(Some((
                        ExprKind::Place(PlaceExprKind::Field(Receiver::Expr(s), def.id)),
                        ExprTy::Ty(var.ty.unwrap()),
                    )))
                }
                uc_middle::DefKind::FuncArg(arg) => Ok(Some((
                    ExprKind::Place(PlaceExprKind::Arg(def.id)),
                    ExprTy::Ty(arg.ty),
                ))),
                uc_middle::DefKind::Local(local) => Ok(Some((
                    ExprKind::Place(PlaceExprKind::Local(def.id)),
                    ExprTy::Ty(local.ty),
                ))),
                _ => panic!("unexpected def {:?} for sym {:?}", def, name),
            }
        } else {
            Ok(None)
        }
    }

    fn try_bare_func_access(
        &mut self,
        name: &Identifier,
        span: Span,
        allow_binding_to_static: bool,
        allow_iterator: bool,
    ) -> Result<Option<(Receiver, DefId)>, BodyError> {
        if let Ok(func) = self.ctx.resolver.get_scoped_func(
            self.body_scope,
            self.ctx.defs,
            ScopeWalkKind::Access,
            name,
        ) {
            let def = self.ctx.defs.get_func(func);
            assert!(!def.flags.intersects(
                FuncFlags::OPERATOR | FuncFlags::PREOPERATOR | FuncFlags::POSTOPERATOR
            ));
            assert!(allow_iterator || !def.flags.contains(FuncFlags::ITERATOR));
            let receiver = if def.flags.contains(FuncFlags::STATIC) {
                Receiver::Cdo(self.class_did)
            } else {
                match (self.self_ty, allow_binding_to_static) {
                    (None, false) => {
                        return Err(BodyError {
                            kind: BodyErrorKind::InvalidSelfAccess,
                            span,
                        })
                    }
                    (None, true) => Receiver::Cdo(self.class_did),
                    (Some(d), _) => Receiver::Expr(self.body.add_expr(Expr {
                        ty: ExprTy::Ty(self.class_ty),
                        kind: ExprKind::Place(PlaceExprKind::SelfAccess),
                        span: None,
                    })),
                }
            };
            Ok(Some((receiver, func)))
        } else {
            Ok(None)
        }
    }

    fn lower_foreach(
        &mut self,
        source: &'hir uc_ast::Expr,
        run: &uc_ast::Block,
    ) -> Result<StatementKind, BodyError> {
        let (rhs, name, args) = match &source.kind {
            uc_ast::ExprKind::FuncCallExpr { lhs, name, args } => (lhs, name, args),
            _ => panic!("invalid syntax"),
        };
        let func = match rhs {
            Some(rhs) => {
                let access_context = self.get_access_context(rhs)?;
                match access_context {
                    AccessContext::Static(c) => {
                        let func = self
                            .ctx
                            .resolver
                            .get_scoped_func(c, self.ctx.defs, ScopeWalkKind::Access, name)
                            .map_err(|_| BodyError {
                                kind: BodyErrorKind::FuncNotFound { name: name.clone() },
                                span: source.span,
                            })?;
                        NativeIteratorKind::Func(Receiver::Cdo(c), func)
                    }
                    AccessContext::Const(_) => {
                        return Err(BodyError {
                            kind: BodyErrorKind::NotYetImplemented("cannot iterate over consts"),
                            span: rhs.span,
                        })
                    }
                    AccessContext::Default(c) => {
                        let prop = self
                            .ctx
                            .resolver
                            .get_scoped_var(c, self.ctx.defs, ScopeWalkKind::Access, name)
                            .map_err(|_| BodyError {
                                kind: BodyErrorKind::SymNotFound { name: name.clone() },
                                span: source.span,
                            })?;
                        let ty = self.ctx.defs.get_var(prop).ty.unwrap();
                        assert!(ty.is_dyn_array());
                        let expr = self.body.add_expr(Expr {
                            kind: ExprKind::Place(PlaceExprKind::Field(Receiver::Cdo(c), prop)),
                            ty: ExprTy::Ty(ty),
                            span: Some(source.span),
                        });
                        NativeIteratorKind::Array(expr)
                    }
                    AccessContext::Expr(expr) => NativeIteratorKind::Array(expr),
                }
            }
            None => {
                if let Some((kind, ty)) = self.try_bare_var_access(name, source.span)? {
                    let expr = self.body.add_expr(Expr {
                        kind,
                        ty,
                        span: Some(source.span),
                    });
                    NativeIteratorKind::Array(expr)
                } else if let Some((receiver, func)) =
                    self.try_bare_func_access(name, source.span, false, true)?
                {
                    NativeIteratorKind::Func(receiver, func)
                } else {
                    panic!("unknown iterator")
                }
            }
        };

        Err(BodyError {
            kind: BodyErrorKind::NotYetImplemented("iterator"),
            span: source.span,
        })
    }

    fn get_access_context(&mut self, lhs: &'hir uc_ast::Expr) -> Result<AccessContext, BodyError> {
        let context_ctor: Option<fn(DefId) -> AccessContext> = match &lhs.kind {
            uc_ast::ExprKind::FieldExpr { rhs, .. } | uc_ast::ExprKind::SymExpr { sym: rhs } => {
                if rhs == "static" {
                    Some(AccessContext::Static)
                } else if rhs == "const" {
                    Some(AccessContext::Const)
                } else if rhs == "default" {
                    Some(AccessContext::Default)
                } else {
                    None
                }
            }
            _ => None,
        };

        match context_ctor {
            Some(c) => match &lhs.kind {
                uc_ast::ExprKind::FieldExpr { lhs, .. } => match &lhs.kind {
                    uc_ast::ExprKind::LiteralExpr { lit } => {
                        match self.ast_to_middle_lit(lit, TypeExpectation::None, lhs.span)? {
                            (Literal::Class(did), _) => Ok(c(did)),
                            _ => Err(BodyError {
                                kind: BodyErrorKind::MissingClassLit,
                                span: lhs.span,
                            }),
                        }
                    }
                    _ => Err(BodyError {
                        kind: BodyErrorKind::MissingClassLit,
                        span: lhs.span,
                    }),
                },
                uc_ast::ExprKind::SymExpr { .. } => Ok(c(self.class_did)),
                _ => unreachable!(),
            },
            None => Ok(AccessContext::Expr(self.lower_expr(
                lhs,
                TypeExpectation::None,
                false,
            )?)),
        }
    }

    fn lower_dyn_array_call(
        &mut self,
        receiver: ExprId,
        name: &Identifier,
        args: &'hir [Option<uc_ast::Expr>],
        span: Span,
    ) -> Result<(ExprKind, ExprTy), BodyError> {
        let receiver_ty = self.body.get_expr_ty(receiver).expect_ty("array expr");
        let inner_ty = receiver_ty.drop_array();
        if name == "Find" {
            match args {
                [Some(val)] => {
                    let inner =
                        self.lower_expr(val, TypeExpectation::RequiredTy(inner_ty), false)?;
                    Ok((
                        ExprKind::Value(ValueExprKind::DynArrayIntrinsic(
                            receiver,
                            DynArrayOpKind::FindElem(inner),
                        )),
                        ExprTy::Ty(Ty::INT),
                    ))
                }
                [Some(field), Some(val)] => {
                    let field_id = match &field.kind {
                        uc_ast::ExprKind::LiteralExpr {
                            lit: uc_ast::Literal::Name(n),
                        } => {
                            assert!(inner_ty.is_struct());
                            let struct_def = inner_ty.get_def().unwrap();
                            self.ctx
                                .resolver
                                .get_scoped_var(struct_def, self.ctx.defs, ScopeWalkKind::Access, n)
                                .map_err(|_| BodyError {
                                    kind: BodyErrorKind::SymNotFound { name: n.clone() },
                                    span: field.span,
                                })?
                        }
                        _ => panic!("bad find call"),
                    };
                    let field_ty = self.ctx.defs.get_var(field_id).ty.unwrap();
                    let inner =
                        self.lower_expr(val, TypeExpectation::RequiredTy(field_ty), false)?;
                    Ok((
                        ExprKind::Value(ValueExprKind::DynArrayIntrinsic(
                            receiver,
                            DynArrayOpKind::FindField(field_id, inner),
                        )),
                        ExprTy::Ty(Ty::INT),
                    ))
                }
                _ => panic!("bad find call"),
            }
        } else if name == "Add" {
            match args {
                [Some(val)] => {
                    let amt = self.lower_expr(val, TypeExpectation::RequiredTy(Ty::INT), false)?;
                    Ok((
                        ExprKind::Value(ValueExprKind::DynArrayIntrinsic(
                            receiver,
                            DynArrayOpKind::Add(amt),
                        )),
                        ExprTy::Ty(Ty::INT),
                    ))
                }
                _ => panic!("bad add call"),
            }
        } else if name == "AddItem" {
            match args {
                [Some(val)] => {
                    let inner =
                        self.lower_expr(val, TypeExpectation::RequiredTy(inner_ty), false)?;
                    Ok((
                        ExprKind::Value(ValueExprKind::DynArrayIntrinsic(
                            receiver,
                            DynArrayOpKind::AddItem(inner),
                        )),
                        ExprTy::Ty(Ty::INT),
                    ))
                }
                _ => panic!("bad additem call"),
            }
        } else if name == "Insert" {
            match args {
                [Some(at), Some(num)] => {
                    let at = self.lower_expr(at, TypeExpectation::RequiredTy(Ty::INT), false)?;
                    let num = self.lower_expr(num, TypeExpectation::RequiredTy(Ty::INT), false)?;
                    Ok((
                        ExprKind::Value(ValueExprKind::DynArrayIntrinsic(
                            receiver,
                            DynArrayOpKind::Insert(at, num),
                        )),
                        ExprTy::Void,
                    ))
                }
                _ => panic!("bad insert call"),
            }
        } else if name == "InsertItem" {
            match args {
                [Some(at), Some(item)] => {
                    let at = self.lower_expr(at, TypeExpectation::RequiredTy(Ty::INT), false)?;
                    let item =
                        self.lower_expr(item, TypeExpectation::RequiredTy(inner_ty), false)?;
                    Ok((
                        ExprKind::Value(ValueExprKind::DynArrayIntrinsic(
                            receiver,
                            DynArrayOpKind::InsertItem(at, item),
                        )),
                        ExprTy::Ty(Ty::INT),
                    ))
                }
                _ => panic!("bad insertitem call"),
            }
        } else if name == "Remove" {
            match args {
                [Some(at), Some(num)] => {
                    let at = self.lower_expr(at, TypeExpectation::RequiredTy(Ty::INT), false)?;
                    let num = self.lower_expr(num, TypeExpectation::RequiredTy(Ty::INT), false)?;
                    Ok((
                        ExprKind::Value(ValueExprKind::DynArrayIntrinsic(
                            receiver,
                            DynArrayOpKind::Remove(at, num),
                        )),
                        ExprTy::Void,
                    ))
                }
                _ => panic!("bad remove call"),
            }
        } else if name == "RemoveItem" {
            match args {
                [Some(item)] => {
                    let item =
                        self.lower_expr(item, TypeExpectation::RequiredTy(inner_ty), false)?;
                    Ok((
                        ExprKind::Value(ValueExprKind::DynArrayIntrinsic(
                            receiver,
                            DynArrayOpKind::RemoveItem(item),
                        )),
                        ExprTy::Ty(Ty::INT),
                    ))
                }
                _ => panic!("bad removeitem call"),
            }
        } else if name == "Sort" {
            Err(BodyError {
                kind: BodyErrorKind::NotYetImplemented("dyn array Sort"),
                span,
            })
        } else if name == "RandomizeOrder" {
            assert!(args.is_empty());
            Ok((
                ExprKind::Value(ValueExprKind::DynArrayIntrinsic(
                    receiver,
                    DynArrayOpKind::RandomizeOrder,
                )),
                ExprTy::Void,
            ))
        } else {
            panic!("unknown array op {}", name)
        }
    }

    fn lower_cast(
        &mut self,
        inner_expr: &'hir uc_ast::Expr,
        to_type: Ty,
    ) -> Result<ExprKind, BodyError> {
        let expr = self.lower_expr(inner_expr, TypeExpectation::None, false)?;
        let expr_ty = self.body.get_expr_ty(expr).ty_or(BodyError {
            kind: BodyErrorKind::VoidType { expected: None },
            span: inner_expr.span,
        })?;
        if self.ty_match(expr_ty, to_type).is_some() {
            // We cast to a subtype. Note the reversed arguments
        } else {
            let cc = self.conversion_cost(to_type, expr_ty, true);
            match cc {
                TyConversionCost::Same | TyConversionCost::Generalization(_) => {
                    return Err(BodyError {
                        kind: BodyErrorKind::UnnecessaryCast {
                            to: to_type,
                            from: expr_ty,
                        },
                        span: inner_expr.span,
                    })
                }
                TyConversionCost::Disallowed => {
                    return Err(BodyError {
                        kind: BodyErrorKind::InvalidCast {
                            to: to_type,
                            from: expr_ty,
                        },
                        span: inner_expr.span,
                    })
                }
                TyConversionCost::Expansion
                | TyConversionCost::IntToFloat
                | TyConversionCost::Truncation => {
                    // Cast is allowed
                }
            }
        }
        Ok(ExprKind::Value(ValueExprKind::CastExpr(
            to_type, expr, true,
        )))
    }

    fn lower_call_sig(
        &mut self,
        sig: &'hir FuncSig,
        args: &'hir [Option<uc_ast::Expr>],
        span: Span,
        func_flags: FuncFlags,
    ) -> Result<(ExprTy, Vec<Option<ExprId>>), BodyError> {
        if func_flags.contains(FuncFlags::ITERATOR) {
            Err(BodyError {
                kind: BodyErrorKind::NotYetImplemented("iterator call sig"),
                span,
            })
        } else {
            if args.len() > sig.args.len() {
                return Err(BodyError {
                    kind: BodyErrorKind::ArgCountError {
                        expected: sig.args.len() as u32,
                        got: args.len() as u32,
                    },
                    span,
                });
            }
            let mut arg_exprs = vec![];
            for (&arg_def, arg_expr) in sig
                .args
                .iter()
                .zip(args.iter().chain(std::iter::repeat(&None)))
            {
                let arg = self.ctx.defs.get_arg(arg_def);
                if !arg.flags.contains(ArgFlags::OPTIONAL) && arg_expr.is_none() {
                    return Err(BodyError {
                        kind: BodyErrorKind::MissingNonOptional,
                        span,
                    });
                }
                if let Some(arg_expr) = arg_expr {
                    let arg_expr_ty = if arg.flags.contains(ArgFlags::COERCE) {
                        TypeExpectation::CoerceToTy(arg.ty)
                    } else {
                        TypeExpectation::RequiredTy(arg.ty)
                    };
                    let is_out = arg
                        .flags
                        .contains(ArgFlags::OUT | ArgFlags::OUTONLY | ArgFlags::REF);
                    let exp_id = self.lower_expr(arg_expr, arg_expr_ty, is_out)?;
                    let expr = self.body.get_expr(exp_id);
                    if is_out && !matches!(expr.kind, ExprKind::Place(_)) {
                        return Err(BodyError {
                            kind: BodyErrorKind::ByRefArgNotPlace,
                            span,
                        });
                    }
                    arg_exprs.push(Some(exp_id));
                } else {
                    arg_exprs.push(None);
                }
            }

            let mut ret_ty = match sig.ret_ty {
                Some(t) => ExprTy::Ty(t),
                None => ExprTy::Void,
            };

            if func_flags.contains(FuncFlags::COERCE) {
                let ty = ret_ty.expect_ty("coerce function");
                let arg = arg_exprs[0].unwrap();
                let expr = self.body.get_expr(arg);
                match &expr.ty {
                    ExprTy::Ty(t) if t.is_class() => {
                        let new_ret = t.instanciate_class();
                        assert!(self.ty_match(ty, new_ret).is_some());
                        ret_ty = ExprTy::Ty(new_ret);
                    }
                    _ => {
                        return Err(BodyError {
                            kind: BodyErrorKind::BadCoerceArg,
                            span: expr.span.unwrap(),
                        })
                    }
                }
            }

            Ok((ret_ty, arg_exprs))
        }
    }

    fn lower_unary_op<F: Fn(DefId) -> bool>(
        &mut self,
        rhs: &'hir uc_ast::Expr,
        op: Op,
        filter: F,
    ) -> Result<(ExprKind, ExprTy), BodyError> {
        let r = self.lower_expr(rhs, TypeExpectation::None, false)?;
        let r_ty = self.body.get_expr_ty(r).expect_ty("unary operator");
        let mut candidate_ops = self.ctx.resolver.collect_scoped_ops(
            self.body_scope,
            self.ctx.defs,
            ScopeWalkKind::Access,
            op,
        );
        candidate_ops.sort_unstable();
        candidate_ops.dedup();
        // Preoperators never have argument coercion going on, so simply collect the best op
        let mut best = candidate_ops
            .iter()
            .filter(|&op| filter(*op))
            .filter_map(|&op| {
                let op_def = self.ctx.defs.get_op(op);
                let arg = op_def.sig.as_ref().unwrap().args[0];
                let arg_def = self.ctx.defs.get_arg(arg);
                self.ty_match(arg_def.ty, r_ty).map(|p| (p, op))
            })
            .collect::<Vec<_>>();
        best.sort_unstable_by_key(|(p, _)| *p);
        let best = match &*best {
            [] => {
                return Err(BodyError {
                    kind: BodyErrorKind::NoMatchingOps,
                    span: rhs.span,
                })
            }
            [(_, def)] => *def,
            [(p_a, def_a), (p_b, _), ..] => {
                if p_a == p_b {
                    return Err(BodyError {
                        kind: BodyErrorKind::MultipleMatchingOps,
                        span: rhs.span,
                    });
                } else {
                    *def_a
                }
            }
        };
        let ret_ty = self
            .ctx
            .defs
            .get_op(best)
            .sig
            .as_ref()
            .unwrap()
            .ret_ty
            .unwrap();
        Ok((
            ExprKind::Value(ValueExprKind::OpCall(best, r, None)),
            ExprTy::Ty(ret_ty),
        ))
    }

    fn lower_bin_op(
        &mut self,
        span: Span,
        lhs: &'hir uc_ast::Expr,
        rhs: &'hir uc_ast::Expr,
        op: Op,
        lhs_ty_hint: TypeExpectation,
    ) -> Result<(ExprKind, ExprTy), BodyError> {
        let mut l = self.lower_expr(lhs, lhs_ty_hint, false)?;
        let mut r = self.lower_expr(rhs, TypeExpectation::None, false)?;
        let l_ty = self.body.get_expr_ty(l).ty_or(BodyError {
            kind: BodyErrorKind::VoidType { expected: None },
            span: lhs.span,
        })?;
        let r_ty = self.body.get_expr_ty(r).ty_or(BodyError {
            kind: BodyErrorKind::VoidType { expected: None },
            span: rhs.span,
        })?;

        let mut candidate_ops = self.ctx.resolver.collect_scoped_ops(
            self.body_scope,
            self.ctx.defs,
            ScopeWalkKind::Access,
            op,
        );
        candidate_ops.sort_unstable();
        candidate_ops.dedup();

        // Select a matching binary op
        let mut best = candidate_ops
            .iter()
            .filter(|&op| {
                self.ctx
                    .defs
                    .get_op(*op)
                    .flags
                    .contains(FuncFlags::OPERATOR)
            })
            .filter_map(|&op| {
                let op_def = self.ctx.defs.get_op(op);
                let l_arg = self.ctx.defs.get_arg(op_def.sig.as_ref().unwrap().args[0]);
                let r_arg = self.ctx.defs.get_arg(op_def.sig.as_ref().unwrap().args[1]);
                let l_cc =
                    self.conversion_cost(l_arg.ty, l_ty, l_arg.flags.contains(ArgFlags::COERCE));
                let r_cc =
                    self.conversion_cost(r_arg.ty, r_ty, r_arg.flags.contains(ArgFlags::COERCE));
                let cc = std::cmp::max(l_cc, r_cc);
                match cc {
                    TyConversionCost::Disallowed => None,
                    _ => Some((cc, op)),
                }
            })
            .collect::<Vec<_>>();
        best.sort_unstable_by_key(|(p, _)| *p);
        let best = match &*best {
            [] => {
                if op == Op::EqEq || op == Op::BangEq {
                    if l_ty.is_struct() && r_ty.is_struct() && l_ty.get_def() == r_ty.get_def() {
                        return Ok((
                            ExprKind::Value(ValueExprKind::StructComparison(l, r, op == Op::EqEq)),
                            ExprTy::Ty(Ty::BOOL),
                        ));
                    } else if (l_ty.is_delegate() || r_ty.is_delegate())
                        && (self.ty_match(l_ty, r_ty).is_some()
                            || self.ty_match(r_ty, l_ty).is_some())
                    {
                        return Ok((
                            ExprKind::Value(ValueExprKind::DelegateComparison(
                                l,
                                r,
                                op == Op::EqEq,
                            )),
                            ExprTy::Ty(Ty::BOOL),
                        ));
                    }
                }
                return Err(BodyError {
                    kind: BodyErrorKind::NoMatchingOps,
                    span,
                });
            }
            [(_, def)] => *def,
            [(p_a, def_a), (p_b, _), ..] => {
                if p_a == p_b {
                    return Err(BodyError {
                        kind: BodyErrorKind::MultipleMatchingOps,
                        span,
                    });
                } else {
                    *def_a
                }
            }
        };
        let op_def = self.ctx.defs.get_op(best);
        let l_arg = self.ctx.defs.get_arg(op_def.sig.as_ref().unwrap().args[0]);
        let r_arg = self.ctx.defs.get_arg(op_def.sig.as_ref().unwrap().args[1]);

        // Check again without a conversion to see if we need to insert a coercion
        let l_cc = self.conversion_cost(l_arg.ty, l_ty, false);
        let r_cc = self.conversion_cost(r_arg.ty, r_ty, false);

        if l_cc == TyConversionCost::Disallowed {
            l = self.body.add_expr(Expr {
                ty: ExprTy::Ty(l_arg.ty),
                kind: ExprKind::Value(ValueExprKind::CastExpr(l_arg.ty, l, false)),
                span: self.body.get_expr(l).span,
            });
        }
        if r_cc == TyConversionCost::Disallowed {
            r = self.body.add_expr(Expr {
                ty: ExprTy::Ty(r_arg.ty),
                kind: ExprKind::Value(ValueExprKind::CastExpr(r_arg.ty, r, false)),
                span: self.body.get_expr(r).span,
            });
        }

        Ok((
            ExprKind::Value(ValueExprKind::OpCall(best, l, Some(r))),
            ExprTy::Ty(op_def.sig.as_ref().unwrap().ret_ty.unwrap()),
        ))
    }

    fn lower_lhsful_func_call(
        &mut self,
        lhs: &'hir uc_ast::Expr,
        name: &Identifier,
        args: &'hir [Option<uc_ast::Expr>],
        span: Span,
    ) -> Result<(ExprKind, ExprTy), BodyError> {
        let access_context = self.get_access_context(lhs)?;
        let (receiver_ty, receiver) = match access_context {
            AccessContext::Expr(e)
                if self
                    .body
                    .get_expr_ty(e)
                    .expect_ty("func call")
                    .is_dyn_array() =>
            {
                return self.lower_dyn_array_call(e, name, args, span);
            }
            AccessContext::Static(d) => (Ty::class_from(d), Receiver::Cdo(d)),
            AccessContext::Expr(e) => {
                let expr_ty = self.body.get_expr_ty(e).ty_or(BodyError {
                    kind: BodyErrorKind::VoidType { expected: None },
                    span: lhs.span,
                })?;
                (
                    if expr_ty.is_class() {
                        // downgrade classes to objects
                        Ty::object_from(self.ctx.special_items.object_id.unwrap())
                    } else {
                        expr_ty
                    },
                    Receiver::Expr(e),
                )
            }
            _ => {
                return Err(BodyError {
                    kind: BodyErrorKind::BadContext,
                    span,
                })
            }
        };

        let func = self
            .ctx
            .resolver
            .get_scoped_func(
                receiver_ty.get_def().unwrap(),
                self.ctx.defs,
                ScopeWalkKind::Access,
                name,
            )
            .map_err(|_| BodyError {
                kind: BodyErrorKind::FuncNotFound { name: name.clone() },
                span: lhs.span,
            })?;
        let def = self.ctx.defs.get_func(func);
        assert!(!def.flags.intersects(
            FuncFlags::OPERATOR
                | FuncFlags::PREOPERATOR
                | FuncFlags::POSTOPERATOR
                | FuncFlags::ITERATOR
        ));

        let (ret_ty, arg_exprs) =
            self.lower_call_sig(def.sig.as_ref().unwrap(), args, span, def.flags)?;
        Ok((
            ExprKind::Value(ValueExprKind::FuncCall(
                func,
                receiver,
                arg_exprs.into_boxed_slice(),
            )),
            ret_ty,
        ))
    }

    fn lower_expr(
        &mut self,
        expr: &'hir uc_ast::Expr,
        ty_expec: TypeExpectation,
        prefer_delegate_prop: bool,
    ) -> Result<ExprId, BodyError> {
        // If we coerce our type anyway, it'd be bad to eagerly expect something different
        let (lhs_ty_hint, passdown_ty_expec) = match ty_expec {
            TypeExpectation::RequiredTy(t) => {
                (TypeExpectation::HintTy(t), TypeExpectation::RequiredTy(t))
            }
            TypeExpectation::HintTy(t) => (TypeExpectation::HintTy(t), TypeExpectation::None),
            TypeExpectation::CoerceToTy(_) | TypeExpectation::None => {
                (TypeExpectation::None, TypeExpectation::None)
            }
        };
        let (kind, ty) = match &expr.kind {
            uc_ast::ExprKind::IndexExpr { base, idx } => {
                let idx = self.lower_expr(idx, TypeExpectation::RequiredTy(Ty::INT), false)?;
                let base_id = self.lower_expr(base, TypeExpectation::None, false)?;
                let arr_ty = self.body.get_expr_ty(base_id).ty_or(BodyError {
                    kind: BodyErrorKind::VoidType { expected: None },
                    span: base.span,
                })?;
                if !arr_ty.is_dyn_array() && !arr_ty.is_stat_array() {
                    return Err(BodyError {
                        kind: BodyErrorKind::NonArrayType { found: arr_ty },
                        span: base.span,
                    });
                }
                let inner_ty = arr_ty.drop_array();
                (
                    ExprKind::Place(PlaceExprKind::Index(base_id, idx)),
                    ExprTy::Ty(inner_ty),
                )
            }
            uc_ast::ExprKind::FieldExpr { lhs, rhs } => {
                let access_context = self.get_access_context(lhs)?;
                match access_context {
                    AccessContext::Static(c) => {
                        let func_def = self
                            .ctx
                            .resolver
                            .get_scoped_func(c, self.ctx.defs, ScopeWalkKind::Access, rhs)
                            .map_err(|_| BodyError {
                                kind: BodyErrorKind::FuncNotFound { name: rhs.clone() },
                                span: expr.span,
                            })?;

                        (
                            ExprKind::Value(ValueExprKind::DelegateCreation(
                                Receiver::Cdo(c),
                                func_def,
                            )),
                            ExprTy::Ty(Ty::delegate_from(func_def)),
                        )
                    }
                    AccessContext::Const(c) => {
                        let konst = self
                            .ctx
                            .resolver
                            .get_scoped_const(c, self.ctx.defs, ScopeWalkKind::Access, rhs)
                            .map_err(|_| BodyError {
                                kind: BodyErrorKind::SymNotFound { name: rhs.clone() },
                                span: expr.span,
                            })?;

                        let ty = self.const_ty(konst, ty_expec)?;

                        (ExprKind::Value(ValueExprKind::Const(konst)), ExprTy::Ty(ty))
                    }
                    AccessContext::Default(c) => {
                        let prop = self
                            .ctx
                            .resolver
                            .get_scoped_var(c, self.ctx.defs, ScopeWalkKind::Access, rhs)
                            .map_err(|_| BodyError {
                                kind: BodyErrorKind::FuncNotFound { name: rhs.clone() },
                                span: expr.span,
                            })?;

                        let prop_ty = self.ctx.defs.get_var(prop);

                        (
                            ExprKind::Place(PlaceExprKind::Field(Receiver::Cdo(c), prop)),
                            ExprTy::Ty(prop_ty.ty.unwrap()),
                        )
                    }
                    AccessContext::Expr(l) => {
                        let ty = self.body.get_expr_ty(l).ty_or(BodyError {
                            kind: BodyErrorKind::VoidType { expected: None },
                            span: lhs.span,
                        })?;
                        if ty.is_dyn_array() {
                            assert!(rhs == "Length");
                            (
                                ExprKind::Place(PlaceExprKind::DynArrayLen(l)),
                                ExprTy::Ty(Ty::INT),
                            )
                        } else if ty.is_object()
                            || ty.is_struct()
                            || ty.is_class()
                            || ty.is_interface()
                        {
                            let scope = if ty.is_class() {
                                self.ctx.special_items.object_id.unwrap()
                            } else {
                                ty.get_def().unwrap()
                            };
                            if let Ok(prop) = self.ctx.resolver.get_scoped_var(
                                scope,
                                self.ctx.defs,
                                ScopeWalkKind::Access,
                                rhs,
                            ) {
                                let var = self.ctx.defs.get_var(prop);
                                (
                                    ExprKind::Place(PlaceExprKind::Field(Receiver::Expr(l), prop)),
                                    ExprTy::Ty(var.ty.unwrap()),
                                )
                            } else if let Ok(konst) = self.ctx.resolver.get_scoped_const(
                                scope,
                                self.ctx.defs,
                                ScopeWalkKind::Access,
                                rhs,
                            ) {
                                (
                                    ExprKind::Value(ValueExprKind::Const(konst)),
                                    ExprTy::Ty(self.const_ty(konst, ty_expec)?),
                                )
                            } else if let Ok(func) = self.ctx.resolver.get_scoped_func(
                                scope,
                                self.ctx.defs,
                                ScopeWalkKind::Access,
                                rhs,
                            ) {
                                (
                                    ExprKind::Value(ValueExprKind::DelegateCreation(
                                        Receiver::Expr(l),
                                        func,
                                    )),
                                    ExprTy::Ty(Ty::delegate_from(func)),
                                )
                            } else {
                                return Err(BodyError {
                                    kind: BodyErrorKind::SymNotFound { name: rhs.clone() },
                                    span: expr.span,
                                });
                            }
                        } else {
                            todo!("invalid field access: {:?}, {:?}", lhs, rhs)
                        }
                    }
                }
            }
            uc_ast::ExprKind::FuncCallExpr { lhs, name, args } => {
                match lhs {
                    Some(lhs) => self.lower_lhsful_func_call(lhs, name, args, expr.span)?,
                    None => {
                        let cast_to_type = if args.len() == 1 {
                            self.ctx.decode_simple_ty(name, self.body_scope)
                        } else {
                            None
                        };
                        // A freestanding function call could be a cast. Check if name is a type
                        match cast_to_type {
                            Some(cast_ty) => {
                                // We have a cast. Forget everything about expected types...
                                match &**args {
                                    [Some(arg)] => {
                                        (self.lower_cast(arg, cast_ty)?, ExprTy::Ty(cast_ty))
                                    }
                                    x => {
                                        return Err(BodyError {
                                            kind: BodyErrorKind::ArgCountError {
                                                expected: 1,
                                                got: x.len() as u32,
                                            },
                                            span: expr.span,
                                        })
                                    }
                                }
                            }
                            None => match self
                                .try_bare_func_access(name, expr.span, false, false)?
                            {
                                Some((receiver, func)) => {
                                    let def = self.ctx.defs.get_func(func);
                                    let (ret_ty, arg_exprs) = self.lower_call_sig(
                                        def.sig.as_ref().unwrap(),
                                        args,
                                        expr.span,
                                        def.flags,
                                    )?;
                                    (
                                        ExprKind::Value(ValueExprKind::FuncCall(
                                            func,
                                            receiver,
                                            arg_exprs.into_boxed_slice(),
                                        )),
                                        ret_ty,
                                    )
                                }
                                None => {
                                    return Err(BodyError {
                                        kind: BodyErrorKind::FuncNotFound { name: name.clone() },
                                        span: expr.span,
                                    })
                                }
                            },
                        }
                    }
                }
            }
            uc_ast::ExprKind::ClassMetaCastExpr { ty, expr } => {
                let cast_ty = self.ctx.decode_ast_ty(ty, self.body_scope).unwrap();
                (self.lower_cast(expr, cast_ty)?, ExprTy::Ty(cast_ty))
            }
            uc_ast::ExprKind::NewExpr { args, cls, arch } => {
                // There is an ambiguity here...
                let outer_name = match &**args {
                    [] => (None, None),
                    [outer] => (
                        outer
                            .as_ref()
                            .map(|o| self.lower_expr(o, TypeExpectation::None, false))
                            .transpose()?,
                        None,
                    ),
                    [outer, name] => (
                        outer
                            .as_ref()
                            .map(|o| self.lower_expr(o, TypeExpectation::None, false))
                            .transpose()?,
                        name.as_ref()
                            .map(|n| {
                                self.lower_expr(n, TypeExpectation::RequiredTy(Ty::STRING), false)
                            })
                            .transpose()?,
                    ),
                    x => {
                        return Err(BodyError {
                            kind: BodyErrorKind::ArgCountError {
                                expected: 2,
                                got: x.len() as u32,
                            },
                            span: expr.span,
                        })
                    }
                };
                let cls_expr = self.lower_expr(cls, TypeExpectation::None, false)?;
                let cls_ty = self.body.get_expr_ty(cls_expr).ty_or(BodyError {
                    kind: BodyErrorKind::VoidType { expected: None },
                    span: cls.span,
                })?;
                let inst_ty = if cls_ty.is_class() {
                    cls_ty.instanciate_class()
                } else {
                    return Err(BodyError {
                        kind: BodyErrorKind::NonClassType { found: cls_ty },
                        span: cls.span,
                    });
                };
                let arch = arch
                    .as_ref()
                    .map(|a| self.lower_expr(a, TypeExpectation::RequiredTy(inst_ty), false))
                    .transpose()?;

                (
                    ExprKind::Value(ValueExprKind::NewExpr(
                        outer_name.0,
                        outer_name.1,
                        cls_expr,
                        arch,
                    )),
                    ExprTy::Ty(inst_ty),
                )
            }
            uc_ast::ExprKind::PreOpExpr { op, rhs } => self.lower_unary_op(rhs, *op, |def| {
                self.ctx
                    .defs
                    .get_op(def)
                    .flags
                    .contains(FuncFlags::PREOPERATOR)
            })?,
            uc_ast::ExprKind::PostOpExpr { lhs, op } => self.lower_unary_op(lhs, *op, |def| {
                self.ctx
                    .defs
                    .get_op(def)
                    .flags
                    .contains(FuncFlags::POSTOPERATOR)
            })?,
            uc_ast::ExprKind::BinOpExpr { lhs, op, rhs } => {
                self.lower_bin_op(expr.span, lhs, rhs, *op, lhs_ty_hint)?
            }
            uc_ast::ExprKind::TernExpr { cond, then, alt } => {
                let e = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL), false)?;
                let then = self.lower_expr(then, passdown_ty_expec, false)?;
                let alt = self.lower_expr(alt, passdown_ty_expec, false)?;
                let unification = self
                    .bidi_unify_tys(self.body.get_expr_ty(then), self.body.get_expr_ty(alt))
                    .ok_or(BodyError {
                        kind: BodyErrorKind::NotYetImplemented("ternary unification"),
                        span: expr.span,
                    })?;
                (
                    ExprKind::Value(ValueExprKind::TernaryOp(e, then, alt)),
                    unification,
                )
            }
            uc_ast::ExprKind::SymExpr { sym } => {
                // This could be 1. a constant in scope, 2. an enum value, 3. a local or arg 4. a class variable or function 5. a self 6. delegate function
                // TODO: Order?
                if let Some(res) = self.try_bare_var_access(sym, expr.span)? {
                    res
                } else if let Some((receiver, func)) =
                    self.try_bare_func_access(sym, expr.span, true, false)?
                {
                    let func_def = self.ctx.defs.get_func(func);
                    if prefer_delegate_prop {
                        let delegate_prop = func_def.delegate_prop.unwrap();
                        (
                            ExprKind::Place(PlaceExprKind::Field(receiver, delegate_prop)),
                            ExprTy::Ty(Ty::delegate_from(func)),
                        )
                    } else {
                        (
                            ExprKind::Value(ValueExprKind::DelegateCreation(receiver, func)),
                            ExprTy::Ty(Ty::delegate_from(func)),
                        )
                    }
                } else if sym == "self" {
                    (
                        ExprKind::Place(PlaceExprKind::SelfAccess),
                        ExprTy::Ty(self.self_ty.expect("access to self var in static func")),
                    )
                } else if sym == "super" {
                    return Err(BodyError {
                        kind: BodyErrorKind::NotYetImplemented("super call"),
                        span: expr.span,
                    });
                } else if let Ok(konst) = self.ctx.resolver.get_scoped_const(
                    self.body_scope,
                    self.ctx.defs,
                    ScopeWalkKind::Access,
                    sym,
                ) {
                    let const_ty = self.const_ty(konst, passdown_ty_expec)?;
                    (
                        ExprKind::Value(ValueExprKind::Const(konst)),
                        ExprTy::Ty(const_ty),
                    )
                } else if let Ok(en) =
                    self.ctx
                        .resolver
                        .get_global_value(self.body_scope, self.ctx.defs, sym)
                {
                    let var = self.ctx.defs.get_variant(en);
                    let parent_enum = var.owning_enum;
                    (
                        ExprKind::Value(ValueExprKind::Lit(Literal::Byte(var.idx))),
                        ExprTy::Ty(Ty::enum_from(parent_enum)),
                    )
                } else {
                    return Err(BodyError {
                        kind: BodyErrorKind::SymNotFound { name: sym.clone() },
                        span: expr.span,
                    });
                }
            }
            uc_ast::ExprKind::LiteralExpr { lit } => {
                let (lit, ty) = self.ast_to_middle_lit(lit, passdown_ty_expec, expr.span)?;
                (ExprKind::Value(ValueExprKind::Lit(lit)), ExprTy::Ty(ty))
            }
        };

        if let TypeExpectation::RequiredTy(expected_ty) | TypeExpectation::CoerceToTy(expected_ty) =
            ty_expec
        {
            let got_non_void = ty.ty_or(BodyError {
                kind: BodyErrorKind::VoidType {
                    expected: Some(expected_ty),
                },
                span: expr.span,
            })?;
            if self.ty_match(expected_ty, got_non_void).is_some() {
                Ok(self.body.add_expr(Expr {
                    ty,
                    kind,
                    span: Some(expr.span),
                }))
            } else {
                match ty::classify_conversion(got_non_void, expected_ty) {
                    ty::ConversionClassification::Forbidden => Err(BodyError {
                        kind: BodyErrorKind::TyMismatch {
                            expected: expected_ty,
                            found: got_non_void,
                        },
                        span: expr.span,
                    }),
                    ty::ConversionClassification::Allowed { auto, truncation } => {
                        if auto || matches!(ty_expec, TypeExpectation::CoerceToTy(_)) {
                            let inner_expr = self.body.add_expr(Expr {
                                ty,
                                kind,
                                span: Some(expr.span),
                            });
                            Ok(self.body.add_expr(Expr {
                                ty: ExprTy::Ty(expected_ty),
                                kind: ExprKind::Value(ValueExprKind::CastExpr(
                                    expected_ty,
                                    inner_expr,
                                    false,
                                )),
                                span: Some(expr.span),
                            }))
                        } else {
                            Err(BodyError {
                                kind: BodyErrorKind::MissingCast {
                                    to: expected_ty,
                                    from: got_non_void,
                                },
                                span: expr.span,
                            })
                        }
                    }
                }
            }
        } else {
            Ok(self.body.add_expr(Expr {
                ty,
                kind,
                span: Some(expr.span),
            }))
        }
    }

    /// For the ternary operator, determine the resulting type
    fn bidi_unify_tys(&self, left: ExprTy, right: ExprTy) -> Option<ExprTy> {
        if let (ExprTy::Ty(l), ExprTy::Ty(r)) = (left, right) {
            let left_to_right = self.ty_match(l, r);
            let right_to_left = self.ty_match(r, l);
            match (left_to_right, right_to_left) {
                (None, None) => {
                    return None;
                    // TODO: get lowest common ancestor for classes
                }
                // L is subtype of R
                (None, Some(_)) => Some(right),
                (Some(_), None) => Some(left),
                (Some(_), Some(_)) => Some(left),
            }
        } else {
            Some(ExprTy::Void)
        }
    }

    fn ast_to_middle_lit(
        &self,
        lit: &uc_ast::Literal,
        expected_type: TypeExpectation,
        span: Span,
    ) -> Result<(Literal, Ty), BodyError> {
        match lit {
            uc_ast::Literal::None => Ok((Literal::None, Ty::NONE)),
            uc_ast::Literal::ObjReference(a, b) => {
                if a == "class" {
                    let parts = b
                        .as_ref()
                        .split('.')
                        .map(|s| Identifier::from_str(s.trim()).unwrap())
                        .collect::<Box<_>>();

                    let referenced_class = match &*parts {
                        [] => panic!("empty class"),
                        [name] => self
                            .ctx
                            .resolver
                            .get_ty(self.body_scope, self.ctx.defs, name)
                            .map_err(|e| BodyError {
                                kind: BodyErrorKind::SymNotFound { name: name.clone() },
                                span,
                            })?,
                        [package, name] => self
                            .ctx
                            .resolver
                            .get_ty_in(Some(self.body_scope), self.ctx.defs, package, name)
                            .map_err(|e| BodyError {
                                kind: BodyErrorKind::SymNotFound { name: name.clone() },
                                span,
                            })?,
                        [..] => panic!("too many name parts"),
                    };

                    Ok((
                        Literal::Class(referenced_class),
                        Ty::class_from(referenced_class),
                    ))
                } else {
                    let ty = self
                        .ctx
                        .resolver
                        .get_ty(self.body_scope, self.ctx.defs, a)
                        .map_err(|e| BodyError {
                            kind: BodyErrorKind::SymNotFound { name: a.clone() },
                            span,
                        })?;
                    Ok((Literal::Object(ty), Ty::object_from(ty)))
                }
            }
            uc_ast::Literal::Bool(b) => Ok((Literal::Bool(*b), Ty::BOOL)),
            uc_ast::Literal::Name(_) => Ok((Literal::Name, Ty::NAME)),
            uc_ast::Literal::String(_) => Ok((Literal::String, Ty::STRING)),

            uc_ast::Literal::Int(i) => Ok(self.adjust_int(*i, expected_type)),
            uc_ast::Literal::Float(f) => Ok((Literal::Float(*f), Ty::FLOAT)),
        }
    }

    fn adjust_int(&self, i: i32, ty_expec: TypeExpectation) -> (Literal, Ty) {
        let interp_ty = match ty_expec {
            TypeExpectation::RequiredTy(t) | TypeExpectation::HintTy(t) => Some(t),
            _ => None,
        };

        if interp_ty.map(|t| t.is_float()).unwrap_or(false) {
            (Literal::Float(i as f32), Ty::FLOAT)
        } else if interp_ty.map(|t| t.is_byte()).unwrap_or(false) {
            (Literal::Byte(i as u8), Ty::BYTE)
        } else {
            (Literal::Int(i), Ty::INT)
        }
    }

    fn const_ty(&self, konst: DefId, ty_expec: TypeExpectation) -> Result<Ty, BodyError> {
        let val = self.ctx.defs.get_const(konst);
        match &val.val {
            ConstVal::Literal(l) => match l {
                Literal::Bool(_) => Ok(Ty::FLOAT),
                Literal::Int(i) => Ok(self.adjust_int(*i, ty_expec).1),
                Literal::Float(f) => Ok(Ty::FLOAT),
                Literal::Name => Ok(Ty::NAME),
                Literal::String => Ok(Ty::STRING),
                _ => unreachable!(),
            },
            ConstVal::Redirect(i) => {
                let did = self
                    .ctx
                    .resolver
                    .get_scoped_const(konst, self.ctx.defs, ScopeWalkKind::Access, i)
                    .map_err(|_| BodyError {
                        kind: BodyErrorKind::SymNotFound { name: i.clone() },
                        span: self.ctx.defs.get_def(konst).span.unwrap(),
                    })?;
                // Exciting potential for a stack overflow here
                self.const_ty(did, ty_expec)
            }
        }
    }

    // This is mostly cribbed from UCC without really understanding the implications.
    // UCC needs to be more strict here with lvalues/rvalues, const, and VM limitations
    // that don't really hurt us if we're failing to consider some.
    fn conversion_cost(&self, dest: Ty, src: Ty, coerce: bool) -> TyConversionCost {
        // check for subtyping
        if let Some(dist) = self.ty_match(dest, src) {
            return if dist == 0 {
                TyConversionCost::Same
            } else {
                TyConversionCost::Generalization(dist)
            };
        }

        match ty::classify_conversion(src, dest) {
            ty::ConversionClassification::Forbidden => TyConversionCost::Disallowed,
            ty::ConversionClassification::Allowed { auto, truncation } => {
                if !auto && !coerce {
                    TyConversionCost::Disallowed
                } else if (src.is_byte() || src.is_int()) && dest.is_float() {
                    TyConversionCost::IntToFloat
                } else if truncation {
                    TyConversionCost::Truncation
                } else {
                    TyConversionCost::Expansion
                }
            }
        }
    }

    fn inheritance_distance(&self, general: DefId, mut specific: DefId) -> Option<u16> {
        let mut dist = 0;
        loop {
            if general == specific {
                return Some(dist);
            } else {
                let def = self.ctx.defs.get_def(specific);
                match &def.kind {
                    uc_middle::DefKind::Class(c) => match c.kind.as_ref().unwrap() {
                        ClassKind::Class { extends, .. } | ClassKind::Interface { extends } => {
                            match *extends {
                                Some(e) => {
                                    specific = e;
                                    dist += 1;
                                }
                                None => return None,
                            }
                        }
                    },
                    _ => unreachable!(),
                }
            }
        }
    }

    fn signature_match(&self, general: DefId, specific: DefId) -> bool {
        if general == specific {
            return true;
        }
        let general_func_sig = self.ctx.defs.get_func(general).sig.as_ref().unwrap();
        let specific_func_sig = self.ctx.defs.get_func(specific).sig.as_ref().unwrap();

        if general_func_sig.args.len() != specific_func_sig.args.len() {
            return false;
        }

        // TODO: How about a byte comparison of `Ty`?
        let cmp_tys = |ty1, ty2| {
            if self.ty_match(ty1, ty2) == Some(0) {
                assert!(self.ty_match(ty2, ty1) == Some(0));
                true
            } else {
                false
            }
        };

        match (general_func_sig.ret_ty, specific_func_sig.ret_ty) {
            (None, None) => {}
            (Some(ty1), Some(ty2)) => {
                if !cmp_tys(ty1, ty2) {
                    return false;
                }
            }
            _ => return false,
        }

        for (&arg1, &arg2) in general_func_sig
            .args
            .iter()
            .zip(specific_func_sig.args.iter())
        {
            let general_arg = self.ctx.defs.get_arg(arg1);
            let specific_arg = self.ctx.defs.get_arg(arg2);
            if !cmp_tys(general_arg.ty, specific_arg.ty) {
                return false;
            }
        }

        true
    }

    /// Returns whether src is a subtype of dest
    fn ty_match(&self, dest: Ty, src: Ty) -> Option<u16> {
        ty::is_subtype(
            dest,
            src,
            self.ctx.special_items.object_id.unwrap(),
            &|general, specific| self.inheritance_distance(general, specific),
            &|general, specific| self.signature_match(general, specific),
        )
    }
}
