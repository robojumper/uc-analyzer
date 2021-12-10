use std::str::FromStr;

use uc_ast as ast;
use uc_def::{ArgFlags, FuncFlags, Op};
use uc_files::Span;
use uc_middle::{
    body::{
        Block, BlockId, Body, DynArrayOpKind, Expr, ExprId, ExprKind, ExprTy, ForeachOpKind,
        Literal, LoopDesugaring, PlaceExprKind, Receiver, Statement, StatementKind, StmtId,
        ValueExprKind,
    },
    ty::{self, Ty},
    ClassKind, ConstVal, DefId, DefKind, FuncSig, ScopeWalkKind,
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
    /// Doesn't affect type inference, applies otherwise
    /// explicit coercion if necessary.
    CoerceToTy(Ty),
    /// Affects type inference, applies implicit coercion
    /// if necessary.
    RequiredTy(Ty),
    /// Doesn't affect type inference, expression must be a place.
    /// If bool is set, must allow mutating.
    PlaceTy(Option<Ty>, bool),
    /// Doesn't affect type inference, only interprets leftmost
    /// expression (if literal) as different type.
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
    /// .staticing on .defaulting on non-object type
    NonObjectType { found: Ty },
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
    /// 5 = 7;
    LhsNotPlace,
    /// TODO
    NotYetImplemented(&'static str),
}

#[derive(Debug)]
enum NativeIteratorKind {
    Array(ExprId),
    Func(Receiver, DefId),
}

#[derive(Debug)]
enum ContextResolution {
    /// LHS had an object or struct type, so var or function access is legitimate
    Item(Receiver, DefId),
    // LHS had a dyn array type, so access must be a builtin
    Array(ExprId),
}

impl<'hir> LoweringContext<'hir> {
    pub fn lower_body(
        &self,
        body_scope: DefId,
        statements: &[ast::Statement],
    ) -> Result<Body, BodyError> {
        let class_did = self.defs.get_item_class(body_scope);
        let class_ty = self.defs.get_class(class_did).self_ty;
        let (self_ty, ret_ty) = match &self.defs.get_def(body_scope).kind {
            DefKind::Operator(o) => {
                assert!(o.flags.contains(FuncFlags::STATIC));
                (None, o.sig.ret_ty)
            }
            DefKind::Function(f) => {
                let self_ty =
                    if f.flags.contains(FuncFlags::STATIC) { None } else { Some(class_ty) };
                (self_ty, f.sig.ret_ty)
            }
            DefKind::State(_) => todo!(),
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
    fn lower_body(mut self, statements: &'hir [ast::Statement]) -> Result<Body, BodyError> {
        let mut lowered_stmts = Vec::with_capacity(statements.len());
        for stmt in statements {
            lowered_stmts.push(self.lower_statement(stmt)?);
        }

        let entry =
            self.body.add_block(Block { stmts: lowered_stmts.into_boxed_slice(), span: None });
        self.body.set_entry(entry);
        Ok(self.body)
    }

    fn lower_block(&mut self, block: &'hir ast::Block) -> Result<BlockId, BodyError> {
        let stmts = self.lower_statements(&block.stmts)?.into_boxed_slice();
        Ok(self.body.add_block(Block { stmts, span: None })) // TODO
    }

    fn lower_statements(
        &mut self,
        stmts: &'hir [ast::Statement],
    ) -> Result<Vec<StmtId>, BodyError> {
        stmts.iter().map(|stmt| self.lower_statement(stmt)).collect::<Result<Vec<_>, _>>()
    }

    fn lower_statement(&mut self, stmt: &'hir ast::Statement) -> Result<StmtId, BodyError> {
        let kind = match &stmt.kind {
            ast::StatementKind::IfStatement { cond, then, or_else } => {
                let e = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL))?;
                let then = self.lower_block(then)?;
                let or_else = or_else.as_ref().map(|b| self.lower_block(b)).transpose()?;
                StatementKind::If(e, then, or_else)
            }
            ast::StatementKind::ForStatement { init, cond, retry, run } => {
                let i = self.lower_statement(init)?;
                let c = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL))?;
                let r = self.lower_statement(retry)?;
                let mut inner_stmts = self.lower_statements(&run.stmts)?;
                let break_stmt = self
                    .body
                    .add_stmt(Statement { kind: StatementKind::Break, span: Some(cond.span) });
                let break_block = self
                    .body
                    .add_block(Block { stmts: Box::new([break_stmt]), span: Some(cond.span) });
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
                    LoopDesugaring::For { init: i, cond: c, retry: r },
                )
            }
            ast::StatementKind::ForeachStatement { ctx, name, args, run } => {
                self.lower_foreach(ctx, name, args, run, stmt.span)?
            }
            ast::StatementKind::WhileStatement { cond, run } => {
                let c = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL))?;
                let mut inner_stmts = self.lower_statements(&run.stmts)?;
                let break_stmt = self
                    .body
                    .add_stmt(Statement { kind: StatementKind::Break, span: Some(cond.span) });
                let break_block = self
                    .body
                    .add_block(Block { stmts: Box::new([break_stmt]), span: Some(cond.span) });
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
            ast::StatementKind::DoStatement { cond, run } => {
                let c = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL))?;
                let mut inner_stmts = self.lower_statements(&run.stmts)?;
                let break_stmt = self
                    .body
                    .add_stmt(Statement { kind: StatementKind::Break, span: Some(cond.span) });
                let break_block = self
                    .body
                    .add_block(Block { stmts: Box::new([break_stmt]), span: Some(cond.span) });
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
            ast::StatementKind::SwitchStatement { scrutinee, cases } => {
                let scrut = self.lower_expr(scrutinee, TypeExpectation::None)?;
                let scrut_ty = self.body.get_expr_ty(scrut).ty_or(BodyError {
                    kind: BodyErrorKind::VoidType { expected: None },
                    span: scrutinee.span,
                })?;
                let mut default = None;
                let mut clauses = vec![];
                let mut stmts = vec![];
                for c in cases {
                    match &c.case {
                        ast::Case::Case(c) => {
                            let expr = self.lower_expr(c, TypeExpectation::RequiredTy(scrut_ty))?;
                            clauses.push((expr, stmts.len().try_into().unwrap()))
                        }
                        ast::Case::Default => {
                            assert!(default.is_none());
                            default = Some(stmts.len().try_into().unwrap())
                        }
                    }
                    for s in &c.stmts {
                        stmts.push(self.lower_statement(s)?);
                    }
                }

                let block =
                    self.body.add_block(Block { stmts: stmts.into_boxed_slice(), span: None }); // TODO

                StatementKind::Switch(scrut, clauses.into_boxed_slice(), default, block)
            }
            ast::StatementKind::BreakStatement => StatementKind::Break,
            ast::StatementKind::ContinueStatement => StatementKind::Continue,
            ast::StatementKind::ReturnStatement { expr } => {
                let ret_ty = match self.ret_ty {
                    Some(t) => TypeExpectation::RequiredTy(t),
                    None => TypeExpectation::None,
                };
                let e = expr.as_ref().map(|e| self.lower_expr(e, ret_ty)).transpose()?;
                StatementKind::Return(e)
            }
            ast::StatementKind::Label { name } => {
                return Err(BodyError {
                    kind: BodyErrorKind::NotYetImplemented("label"),
                    span: stmt.span,
                });
            }
            ast::StatementKind::Assignment { lhs, rhs } => {
                let l = self.lower_expr(lhs, TypeExpectation::PlaceTy(None, true))?;
                let l_ty = self.body.get_expr(l).ty.ty_or(BodyError {
                    kind: BodyErrorKind::VoidType { expected: None },
                    span: lhs.span,
                })?;
                assert!(!l_ty.is_stat_array());
                let r = self.lower_expr(rhs, TypeExpectation::RequiredTy(l_ty))?;
                StatementKind::Assign(l, r)
            }
            ast::StatementKind::Expr { expr } => {
                StatementKind::Expr(self.lower_expr(expr, TypeExpectation::None)?)
            }
        };
        Ok(self.body.add_stmt(Statement { kind, span: Some(stmt.span) }))
    }

    fn lower_foreach(
        &mut self,
        ctx: &'hir ast::Context,
        name: &'hir Identifier,
        args: &'hir [Option<ast::Expr>],
        run: &'hir ast::Block,
        span: Span,
    ) -> Result<StatementKind, BodyError> {
        let it_kind = if let Some((kind, ty)) = self.try_local_or_arg(ctx, name) {
            assert!(ty.is_dyn_array());
            let expr = self.body.add_expr(Expr { kind, span: Some(span), ty: ExprTy::Ty(ty) });
            NativeIteratorKind::Array(expr)
        } else {
            let resolution = self.translate_context(ctx, name, span)?;
            let (recv, item) = match resolution {
                ContextResolution::Item(recv, target) => (recv, target),
                ContextResolution::Array(_) => todo!("error: iterating over dyn array?"),
            };

            let def = self.ctx.defs.get_def(item);
            match &def.kind {
                DefKind::Var(v) => {
                    assert!(v.ty.unwrap().is_dyn_array());
                    let expr = self.body.add_expr(Expr {
                        kind: ExprKind::Place(PlaceExprKind::Field(recv, item)),
                        span: Some(span),
                        ty: ExprTy::Ty(v.ty.unwrap()),
                    });
                    NativeIteratorKind::Array(expr)
                }
                DefKind::Function(f) => {
                    assert!(f.flags.contains(FuncFlags::ITERATOR));
                    NativeIteratorKind::Func(recv, item)
                }
                _ => todo!("error"),
            }
        };

        let iterator_alloc = self.body.create_iterator();
        let (init, get) = match it_kind {
            NativeIteratorKind::Array(array_arg) => {
                let arg_ty = self.body.get_expr_ty(array_arg).expect_ty("foreach array");
                let inner_ty = arg_ty.drop_array();
                let native_iterator_func = self.ctx.special_items.dyn_array_iterator.unwrap();
                let init = ValueExprKind::ForeachIntrinsic(
                    iterator_alloc,
                    ForeachOpKind::Create(
                        Receiver::StaticSelf,
                        native_iterator_func,
                        Box::new([Some(array_arg)]),
                    ),
                );
                let get_call = match &*args {
                    [Some(elem)] => {
                        let elem =
                            self.lower_expr(elem, TypeExpectation::PlaceTy(Some(inner_ty), true))?;
                        ValueExprKind::ForeachIntrinsic(
                            iterator_alloc,
                            ForeachOpKind::Next(Box::new([Some(elem), None])),
                        )
                    }
                    [Some(elem), Some(idx)] => {
                        let elem =
                            self.lower_expr(elem, TypeExpectation::PlaceTy(Some(inner_ty), true))?;
                        let idx =
                            self.lower_expr(idx, TypeExpectation::PlaceTy(Some(Ty::INT), true))?;
                        ValueExprKind::ForeachIntrinsic(
                            iterator_alloc,
                            ForeachOpKind::Next(Box::new([Some(elem), Some(idx)])),
                        )
                    }
                    _ => {
                        return Err(BodyError {
                            kind: BodyErrorKind::ArgCountError {
                                expected: args.len() as u32,
                                got: args.len() as u32,
                            },
                            span,
                        });
                    }
                };
                (init, get_call)
            }
            NativeIteratorKind::Func(receiver, func_id) => {
                let func = self.ctx.defs.get_func(func_id);
                assert!(func.flags.contains(FuncFlags::ITERATOR));

                let mut target_args = func
                    .sig
                    .args
                    .iter()
                    .map(|&a| {
                        let arg = self.ctx.defs.get_arg(a);
                        let optional = arg.flags.contains(ArgFlags::OPTIONAL);
                        let ty = if arg.flags.contains(ArgFlags::OUT) {
                            TypeExpectation::PlaceTy(Some(arg.ty), true)
                        } else {
                            TypeExpectation::RequiredTy(arg.ty)
                        };
                        (ty, optional)
                    })
                    .collect::<Box<_>>();

                let min_required = target_args
                    .iter()
                    .position(|(_, opt)| *opt)
                    .unwrap_or_else(|| target_args.len());
                if args.len() < min_required {
                    return Err(BodyError {
                        kind: BodyErrorKind::ArgCountError {
                            expected: target_args.len() as u32,
                            got: args.len() as u32,
                        },
                        span,
                    });
                }

                let first_arg_expr = if matches!(target_args.get(0..2), Some(&[(TypeExpectation::RequiredTy(c_ty), false), (TypeExpectation::PlaceTy(Some(obj_ty), _), false)]) if c_ty.is_class() && obj_ty.is_object())
                {
                    // First arg is a class, second arg is object. Get specific class type...
                    let expr = self.lower_expr(args[0].as_ref().unwrap(), target_args[0].0)?;
                    let class_ty = self.body.get_expr_ty(expr).expect_ty("iterator class arg");
                    // And set our second arg to be of a more specific object type
                    let obj_ty = class_ty.instanciate_class();
                    target_args[1].0 = TypeExpectation::PlaceTy(Some(obj_ty), false);
                    Some(expr)
                } else {
                    None
                };

                let mut call_args = vec![];
                let mut get_args = vec![];

                let mut rest_args =
                    target_args.iter().zip(args.iter().chain(std::iter::repeat(&None)));
                if let Some(e) = first_arg_expr {
                    call_args.push(Some(e));
                    rest_args.next();
                }
                for ((ty_ex, opt), ast_expr) in rest_args {
                    let expr = match ast_expr {
                        Some(e) => Some(self.lower_expr(e, *ty_ex)?),
                        None => {
                            assert!(*opt);
                            None
                        }
                    };
                    match ty_ex {
                        TypeExpectation::RequiredTy(_) => call_args.push(expr),
                        TypeExpectation::PlaceTy(_, _) => get_args.push(expr),
                        _ => unreachable!(),
                    }
                }

                let init = ValueExprKind::ForeachIntrinsic(
                    iterator_alloc,
                    ForeachOpKind::Create(receiver, func_id, call_args.into_boxed_slice()),
                );

                let get = ValueExprKind::ForeachIntrinsic(
                    iterator_alloc,
                    ForeachOpKind::Next(get_args.into_boxed_slice()),
                );

                (init, get)
            }
        };
        let init_expr = self.body.add_expr(Expr {
            kind: ExprKind::Value(init),
            ty: ExprTy::Void,
            span: Some(span),
        });
        let init_stmt = self
            .body
            .add_stmt(Statement { kind: StatementKind::Expr(init_expr), span: Some(span) });
        let get_expr = self.body.add_expr(Expr {
            kind: ExprKind::Value(get),
            ty: ExprTy::Void,
            span: Some(span),
        });
        let get_stmt =
            self.body.add_stmt(Statement { kind: StatementKind::Expr(get_expr), span: Some(span) });
        let get_block =
            self.body.add_block(Block { stmts: Box::new([get_stmt]), span: Some(span) });

        let check_expr = self.body.add_expr(Expr {
            kind: ExprKind::Value(ValueExprKind::ForeachIntrinsic(
                iterator_alloc,
                ForeachOpKind::HasNext,
            )),
            ty: ExprTy::Ty(Ty::BOOL),
            span: Some(span),
        });
        let break_stmt =
            self.body.add_stmt(Statement { kind: StatementKind::Break, span: Some(span) });
        let break_block =
            self.body.add_block(Block { stmts: Box::new([break_stmt]), span: Some(span) });

        let if_stmt = self.body.add_stmt(Statement {
            kind: StatementKind::If(check_expr, get_block, Some(break_block)),
            span: Some(span),
        });

        let mut run_stmts = self.lower_statements(&run.stmts)?;
        run_stmts.insert(0, if_stmt);

        let full_run_block =
            self.body.add_block(Block { stmts: run_stmts.into_boxed_slice(), span: Some(span) });
        Ok(StatementKind::Loop(
            Some(init_stmt),
            None,
            full_run_block,
            LoopDesugaring::Foreach { init: init_stmt, cond: check_expr, next: get_stmt },
        ))
    }

    fn lower_dyn_array_call(
        &mut self,
        receiver: ExprId,
        name: &Identifier,
        args: &'hir [Option<ast::Expr>],
        span: Span,
    ) -> Result<(ExprKind, ExprTy), BodyError> {
        let receiver_ty = self.body.get_expr_ty(receiver).expect_ty("array expr");
        let inner_ty = receiver_ty.drop_array();
        if name == "Find" {
            match args {
                [Some(val)] => {
                    let inner = self.lower_expr(val, TypeExpectation::RequiredTy(inner_ty))?;
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
                        ast::ExprKind::LiteralExpr { lit: ast::Literal::Name(n) } => {
                            assert!(inner_ty.is_struct());
                            let struct_def = inner_ty.get_def().unwrap();
                            self.ctx.resolve_var(struct_def, n, ScopeWalkKind::Access).ok_or_else(
                                || BodyError {
                                    kind: BodyErrorKind::SymNotFound { name: n.clone() },
                                    span: field.span,
                                },
                            )?
                        }
                        _ => panic!("bad find call"),
                    };
                    let field_ty = self.ctx.defs.get_var(field_id).ty.unwrap();
                    let inner = self.lower_expr(val, TypeExpectation::RequiredTy(field_ty))?;
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
                    let amt = self.lower_expr(val, TypeExpectation::RequiredTy(Ty::INT))?;
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
                    let inner = self.lower_expr(val, TypeExpectation::RequiredTy(inner_ty))?;
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
                    let at = self.lower_expr(at, TypeExpectation::RequiredTy(Ty::INT))?;
                    let num = self.lower_expr(num, TypeExpectation::RequiredTy(Ty::INT))?;
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
                    let at = self.lower_expr(at, TypeExpectation::RequiredTy(Ty::INT))?;
                    let item = self.lower_expr(item, TypeExpectation::RequiredTy(inner_ty))?;
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
                    let at = self.lower_expr(at, TypeExpectation::RequiredTy(Ty::INT))?;
                    let num = self.lower_expr(num, TypeExpectation::RequiredTy(Ty::INT))?;
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
                    let item = self.lower_expr(item, TypeExpectation::RequiredTy(inner_ty))?;
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
            Err(BodyError { kind: BodyErrorKind::NotYetImplemented("dyn array Sort"), span })
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
        inner_expr: &'hir ast::Expr,
        to_type: Ty,
    ) -> Result<ExprKind, BodyError> {
        let expr = self.lower_expr(inner_expr, TypeExpectation::None)?;
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
                        kind: BodyErrorKind::UnnecessaryCast { to: to_type, from: expr_ty },
                        span: inner_expr.span,
                    });
                }
                TyConversionCost::Disallowed => {
                    if expr_ty.is_struct() && to_type.is_struct() {
                        if expr_ty.get_def() == self.ctx.special_items.vector_id
                            && to_type.get_def() == self.ctx.special_items.rotator_id
                            || expr_ty.get_def() == self.ctx.special_items.rotator_id
                                && to_type.get_def() == self.ctx.special_items.vector_id
                        {
                            // vector->rotator or rotator->vector: ok
                        } else {
                            return Err(BodyError {
                                kind: BodyErrorKind::InvalidCast { to: to_type, from: expr_ty },
                                span: inner_expr.span,
                            });
                        }
                    } else {
                        return Err(BodyError {
                            kind: BodyErrorKind::InvalidCast { to: to_type, from: expr_ty },
                            span: inner_expr.span,
                        });
                    }
                }
                TyConversionCost::Expansion
                | TyConversionCost::IntToFloat
                | TyConversionCost::Truncation => {
                    // Cast is allowed
                }
            }
        }
        Ok(ExprKind::Value(ValueExprKind::CastExpr(to_type, expr, true)))
    }

    fn lower_call_sig(
        &mut self,
        sig: &'hir FuncSig,
        args: &'hir [Option<ast::Expr>],
        span: Span,
        func_flags: FuncFlags,
    ) -> Result<(ExprTy, Vec<Option<ExprId>>), BodyError> {
        if func_flags.contains(FuncFlags::ITERATOR) {
            Err(BodyError { kind: BodyErrorKind::NotYetImplemented("iterator call sig"), span })
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
            for (&arg_def, arg_expr) in
                sig.args.iter().zip(args.iter().chain(std::iter::repeat(&None)))
            {
                let arg = self.ctx.defs.get_arg(arg_def);
                if !arg.flags.contains(ArgFlags::OPTIONAL) && arg_expr.is_none() {
                    return Err(BodyError { kind: BodyErrorKind::MissingNonOptional, span });
                }
                if let Some(arg_expr) = arg_expr {
                    let arg_expr_ty = if arg.flags.contains(ArgFlags::COERCE) {
                        TypeExpectation::CoerceToTy(arg.ty)
                    } else {
                        let is_out = arg.flags.contains(ArgFlags::OUT | ArgFlags::REF);
                        let is_const = arg.flags.contains(ArgFlags::CONST);
                        if is_out {
                            TypeExpectation::PlaceTy(Some(arg.ty), !is_const)
                        } else {
                            TypeExpectation::RequiredTy(arg.ty)
                        }
                    };
                    let exp_id = self.lower_expr(arg_expr, arg_expr_ty)?;
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
                        });
                    }
                }
            }

            Ok((ret_ty, arg_exprs))
        }
    }

    fn lower_unary_op<F: Fn(DefId) -> bool>(
        &mut self,
        rhs: &'hir ast::Expr,
        op: Op,
        filter: F,
    ) -> Result<(ExprKind, ExprTy), BodyError> {
        let r = self.lower_expr(rhs, TypeExpectation::None)?;
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
                let arg = op_def.sig.args[0];
                let arg_def = self.ctx.defs.get_arg(arg);
                self.ty_match(arg_def.ty, r_ty).map(|p| (p, op))
            })
            .collect::<Vec<_>>();
        best.sort_unstable_by_key(|(p, _)| *p);
        let best = match &*best {
            [] => return Err(BodyError { kind: BodyErrorKind::NoMatchingOps, span: rhs.span }),
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
        let ret_ty = self.ctx.defs.get_op(best).sig.ret_ty.unwrap();
        Ok((ExprKind::Value(ValueExprKind::OpCall(best, r, None)), ExprTy::Ty(ret_ty)))
    }

    fn lower_bin_op(
        &mut self,
        span: Span,
        lhs: &'hir ast::Expr,
        rhs: &'hir ast::Expr,
        op: Op,
        lhs_ty_hint: TypeExpectation,
    ) -> Result<(ExprKind, ExprTy), BodyError> {
        let mut l = self.lower_expr(lhs, lhs_ty_hint)?;
        // Prefer an LHS of the same type, if possible
        let basis_l_ty = self.body.get_expr_ty(l).expect_ty("lhs bin op");
        let mut r = self.lower_expr(rhs, TypeExpectation::HintTy(basis_l_ty))?;
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
            .filter(|&op| self.ctx.defs.get_op(*op).flags.contains(FuncFlags::OPERATOR))
            .filter_map(|&op| {
                let op_def = self.ctx.defs.get_op(op);
                let l_arg = self.ctx.defs.get_arg(op_def.sig.args[0]);
                let r_arg = self.ctx.defs.get_arg(op_def.sig.args[1]);
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
                return Err(BodyError { kind: BodyErrorKind::NoMatchingOps, span });
            }
            [(_, def)] => *def,
            [(p_a, def_a), (p_b, _), ..] => {
                if p_a == p_b {
                    return Err(BodyError { kind: BodyErrorKind::MultipleMatchingOps, span });
                } else {
                    *def_a
                }
            }
        };
        let op_def = self.ctx.defs.get_op(best);
        let l_arg = self.ctx.defs.get_arg(op_def.sig.args[0]);
        let r_arg = self.ctx.defs.get_arg(op_def.sig.args[1]);

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
            ExprTy::Ty(op_def.sig.ret_ty.unwrap()),
        ))
    }

    fn try_enum_variant(
        &mut self,
        ctx: &ast::Context,
        name: &Identifier,
        ty_expec: TypeExpectation,
    ) -> Option<(ExprKind, Ty)> {
        match ctx {
            ast::Context::Bare => match ty_expec {
                TypeExpectation::RequiredTy(t) | TypeExpectation::HintTy(t)
                    if t.is_int() || t.is_byte() =>
                {
                    if t.is_byte() && t.get_def().is_some() {
                        let ty = t.get_def().unwrap();
                        if let Ok(val) = self.ctx.resolver.get_enum_value(ty, name) {
                            let variant = self.ctx.defs.get_variant(val);
                            Some((
                                ExprKind::Value(ValueExprKind::Lit(Literal::Byte(variant.idx))),
                                t,
                            ))
                        } else {
                            None
                        }
                    } else if let Ok(val) =
                        self.ctx.resolver.get_global_value(self.body_scope, self.ctx.defs, name)
                    {
                        let variant = self.ctx.defs.get_variant(val);
                        let (lit, ty) = self.adjust_int(variant.idx as i32, ty_expec);
                        Some((ExprKind::Value(ValueExprKind::Lit(lit)), ty))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn try_local_or_arg(
        &mut self,
        ctx: &ast::Context,
        name: &Identifier,
    ) -> Option<(ExprKind, Ty)> {
        match ctx {
            ast::Context::Bare => {
                if name == "self" {
                    Some((ExprKind::Place(PlaceExprKind::SelfAccess), self.self_ty.unwrap()))
                } else {
                    match self.ctx.resolver.get_scoped_item(
                        self.body_scope,
                        self.ctx.defs,
                        ScopeWalkKind::Access,
                        name,
                        |d| {
                            matches!(
                                self.ctx.defs.get_def(d).kind,
                                DefKind::FuncArg(_) | DefKind::Local(_)
                            )
                        },
                    ) {
                        Ok(i) => {
                            let def = self.ctx.defs.get_def(i);
                            match &def.kind {
                                DefKind::FuncArg(arg) => {
                                    Some((ExprKind::Place(PlaceExprKind::Arg(def.id)), arg.ty))
                                }
                                DefKind::Local(l) => {
                                    Some((ExprKind::Place(PlaceExprKind::Local(def.id)), l.ty))
                                }
                                _ => unreachable!(),
                            }
                        }
                        Err(_) => None,
                    }
                }
            }
            _ => None,
        }
    }

    fn try_const(
        &mut self,
        ctx: &'hir ast::Context,
        name: &Identifier,
        ty_expec: TypeExpectation,
        span: Span,
    ) -> Result<Option<(ExprKind, Ty)>, BodyError> {
        let lookup_scope = match ctx {
            ast::Context::Bare => self.body_scope,
            ast::Context::Const(None) => self.body_scope,
            ast::Context::Const(Some(e)) => {
                let expr = self.lower_expr(e, TypeExpectation::None)?;
                // TODO: Delete this compiled code
                let mut ty = self.body.get_expr_ty(expr).expect_ty("const access");
                if ty.is_class() {
                    ty = ty.instanciate_class();
                }
                if ty.is_object() { ty.get_def().unwrap() } else { todo!("error") }
            }
            _ => return Ok(None),
        };
        match self.ctx.resolver.get_scoped_item(
            lookup_scope,
            self.ctx.defs,
            ScopeWalkKind::Access,
            name,
            |d| matches!(self.ctx.defs.get_def(d).kind, DefKind::Const(_)),
        ) {
            Ok(c) => {
                let def = self.ctx.defs.get_def(c);
                match def.kind {
                    DefKind::Const(_) => {
                        let (lit, ty) = self.resolve_const(c, ty_expec)?;
                        Ok(Some((ExprKind::Value(ValueExprKind::Lit(lit)), ty)))
                    }
                    _ => unreachable!(),
                }
            }
            Err(_) => Ok(None),
        }
    }

    fn try_cast(
        &mut self,
        ctx: &ast::Context,
        name: &'hir Identifier,
        args: &'hir [Option<ast::Expr>],
        span: Span,
    ) -> Result<Option<(ExprKind, Ty)>, BodyError> {
        if !matches!(ctx, ast::Context::Bare) {
            return Ok(None);
        }
        let cast_to_type =
            if args.len() == 1 { self.ctx.decode_simple_ty(name, self.body_scope) } else { None };
        // A freestanding function call could be a cast. Check if name is a type
        match cast_to_type {
            Some(cast_ty) => {
                // We have a cast. Forget everything about expected types...
                match args {
                    [Some(arg)] => Ok(Some((self.lower_cast(arg, cast_ty)?, cast_ty))),
                    x => Err(BodyError {
                        kind: BodyErrorKind::ArgCountError { expected: 1, got: x.len() as u32 },
                        span,
                    }),
                }
            }
            None => Ok(None),
        }
    }

    fn try_builtin_func(
        &mut self,
        ctx: &ast::Context,
        name: &'hir Identifier,
        args: &'hir [Option<ast::Expr>],
        span: Span,
    ) -> Result<Option<(ExprKind, Ty)>, BodyError> {
        if !matches!(ctx, ast::Context::Bare) {
            return Ok(None);
        }

        if name == "ArrayCount" {
            assert_eq!(args.len(), 1, "ArrayCount");
            // TODO: Cleanup const
            let e = self.lower_expr(args[0].as_ref().unwrap(), TypeExpectation::None)?;
            let ty = self.body.get_expr_ty(e).expect_ty("ArrayCount");
            Ok(Some((
                ExprKind::Value(ValueExprKind::Lit(Literal::Int(ty.get_array_count() as i32))),
                Ty::INT,
            )))
        } else if name == "nameof" {
            assert_eq!(args.len(), 1, "nameof");
            // TODO: Cleanup expr
            let expr = self.lower_expr(args[0].as_ref().unwrap(), TypeExpectation::None)?;
            let _ = match &self.body.get_expr(expr).kind {
                ExprKind::Value(ValueExprKind::DelegateCreation(_, d)) => {
                    &self.ctx.defs.get_func(*d).name
                }
                ExprKind::Place(PlaceExprKind::Field(_, d)) => &self.ctx.defs.get_var(*d).name,
                _ => {
                    return Err(BodyError {
                        kind: BodyErrorKind::NotYetImplemented("bad nameof target"),
                        span,
                    });
                }
            };
            Ok(Some((ExprKind::Value(ValueExprKind::Lit(Literal::Name)), Ty::NAME)))
        } else if name == "vect" {
            assert_eq!(args.len(), 3, "vect");
            let arg_exprs = args
                .iter()
                .map(|a| self.lower_expr(a.as_ref().unwrap(), TypeExpectation::HintTy(Ty::FLOAT)))
                .collect::<Result<Vec<_>, _>>()?;
            arg_exprs.iter().try_for_each(|&expr| {
                match &self.body.get_expr(expr).kind {
                    ExprKind::Value(ValueExprKind::Lit(_)) => Ok(()),
                    ExprKind::Value(ValueExprKind::OpCall(_, _, _)) => Ok(()), // TODO
                    _ => Err(BodyError {
                        kind: BodyErrorKind::NotYetImplemented("vect without literal?"),
                        span,
                    }),
                }
            })?;
            let vec_id = self.ctx.special_items.vector_id.unwrap();
            Ok(Some((
                ExprKind::Value(ValueExprKind::Lit(Literal::Struct(vec_id))),
                Ty::struct_from(vec_id),
            )))
        } else if name == "rot" {
            assert_eq!(args.len(), 3, "rot");
            let arg_exprs = args
                .iter()
                .map(|a| self.lower_expr(a.as_ref().unwrap(), TypeExpectation::HintTy(Ty::FLOAT)))
                .collect::<Result<Vec<_>, _>>()?;
            arg_exprs.iter().try_for_each(|&expr| {
                match &self.body.get_expr(expr).kind {
                    ExprKind::Value(ValueExprKind::Lit(_)) => Ok(()),
                    ExprKind::Value(ValueExprKind::OpCall(_, _, _)) => Ok(()), // TODO
                    _ => Err(BodyError {
                        kind: BodyErrorKind::NotYetImplemented("rot without literal?"),
                        span,
                    }),
                }
            })?;
            let vec_id = self.ctx.special_items.rotator_id.unwrap();
            Ok(Some((
                ExprKind::Value(ValueExprKind::Lit(Literal::Struct(vec_id))),
                Ty::struct_from(vec_id),
            )))
        } else {
            Ok(None)
        }
    }

    /// Turn a context and an item name into a receiver and item def.
    /// This essentially pre-processes the syntactic options to always have
    /// an explicit receiver, and tries to find the item that is being referred.
    /// Of course, some syntactic constructs may behave differently in that they
    /// either don't have an explicit receiver, or don't have an explicit definition.
    /// Some notable examples are consts (where we directly produce a Literal), casts (which don't have a func def),
    /// locals/args (where we don't have a receiver) and array builtins (which don't have a def).
    /// Arrays are handled via the `ContextResolution` value, while consts and locals/args
    /// are filtered above.
    fn translate_context(
        &mut self,
        ctx: &'hir uc_ast::Context,
        name: &Identifier,
        span: Span,
    ) -> Result<ContextResolution, BodyError> {
        // TODO: Check if allowed in self
        match ctx {
            ast::Context::Global => {
                let (func, _) = self
                    .ctx
                    .resolve_func(
                        self.self_ty.unwrap().get_def().unwrap(),
                        name,
                        ScopeWalkKind::Access,
                    )
                    .ok_or_else(|| BodyError {
                        kind: BodyErrorKind::FuncNotFound { name: name.clone() },
                        span,
                    })?;
                Ok(ContextResolution::Item(Receiver::Global, func))
            }
            ast::Context::Super(opt) => {
                let (recv, def_id) = match opt {
                    Some(super_name) => {
                        // TODO: Verify super class
                        let s_class = self
                            .ctx
                            .resolver
                            .get_ty(self.body_scope, self.ctx.defs, super_name)
                            .unwrap();
                        let (func, _) = self
                            .ctx
                            .resolve_func(s_class, name, ScopeWalkKind::Access)
                            .ok_or_else(|| BodyError {
                                kind: BodyErrorKind::FuncNotFound { name: name.clone() },
                                span,
                            })?;
                        (Receiver::Super(s_class), func)
                    }
                    None => {
                        let super_class =
                            match self.ctx.defs.get_class(self.class_did).kind.as_ref().unwrap() {
                                ClassKind::Class { extends, .. } => extends.unwrap(),
                                ClassKind::Interface { .. } => unreachable!(),
                            };
                        let (func, _) = self
                            .ctx
                            .resolve_func(super_class, name, ScopeWalkKind::Access)
                            .ok_or_else(|| BodyError {
                                kind: BodyErrorKind::FuncNotFound { name: name.clone() },
                                span,
                            })?;
                        (Receiver::Super(super_class), func)
                    }
                };

                let (func, _) = self
                    .ctx
                    .resolve_func(def_id, name, ScopeWalkKind::Access)
                    .ok_or_else(|| BodyError {
                        kind: BodyErrorKind::FuncNotFound { name: name.clone() },
                        span,
                    })?;
                Ok(ContextResolution::Item(recv, func))
            }
            ast::Context::Static(opt_expr) => {
                let (recv, def_id) = match opt_expr {
                    Some(expr) => {
                        let expr = self.lower_expr(expr, TypeExpectation::None)?;
                        let mut ty = self.body.get_expr_ty(expr).expect_ty("static access");
                        if ty.is_class() {
                            ty = ty.instanciate_class();
                        }
                        if ty.is_object() {
                            (Receiver::Expr(expr), ty.get_def().unwrap())
                        } else {
                            return Err(BodyError {
                                kind: BodyErrorKind::NonObjectType { found: ty },
                                span,
                            });
                        }
                    }
                    None => {
                        // `static.something` without anything before the static
                        (Receiver::StaticSelf, self.class_did)
                    }
                };

                let (func, _) = self
                    .ctx
                    .resolve_func(def_id, name, ScopeWalkKind::Access)
                    .ok_or_else(|| BodyError {
                        kind: BodyErrorKind::FuncNotFound { name: name.clone() },
                        span,
                    })?;
                Ok(ContextResolution::Item(recv, func))
            }
            ast::Context::Default(opt_expr) => {
                let (recv, def_id) = match opt_expr {
                    Some(expr) => {
                        let expr = self.lower_expr(expr, TypeExpectation::None)?;
                        let mut ty = self.body.get_expr_ty(expr).expect_ty("default access");
                        if ty.is_class() {
                            ty = ty.instanciate_class();
                        }
                        if ty.is_object() {
                            (Receiver::Expr(expr), ty.get_def().unwrap())
                        } else {
                            return Err(BodyError {
                                kind: BodyErrorKind::NonObjectType { found: ty },
                                span,
                            });
                        }
                    }
                    None => {
                        // `default.something` without anything before the default
                        (Receiver::StaticSelf, self.class_did)
                    }
                };

                let item = self
                    .ctx
                    .resolver
                    .get_scoped_item(def_id, self.ctx.defs, ScopeWalkKind::Access, name, |d| {
                        matches!(self.ctx.defs.get_def(d).kind, DefKind::Var(_))
                    })
                    .map_err(|_| BodyError {
                        kind: BodyErrorKind::SymNotFound { name: name.clone() },
                        span,
                    })?;
                match &self.ctx.defs.get_def(item).kind {
                    DefKind::Var(_) => { /* ok */ }
                    _ => todo!("error handling"),
                }
                Ok(ContextResolution::Item(recv, item))
            }
            ast::Context::Expr(e) => {
                let expr = self.lower_expr(e, TypeExpectation::None)?;
                let mut ty = self.body.get_expr_ty(expr).expect_ty("field access");
                if ty.is_class() {
                    ty = ty.instanciate_class();
                }
                if ty.is_object() || ty.is_struct() || ty.is_interface() {
                    let scope_def = ty.get_def().unwrap();
                    let item = self
                        .ctx
                        .resolver
                        .get_scoped_item(
                            scope_def,
                            self.ctx.defs,
                            ScopeWalkKind::Access,
                            name,
                            |d| {
                                matches!(
                                    self.ctx.defs.get_def(d).kind,
                                    DefKind::Const(_) | DefKind::Var(_) | DefKind::Function(_)
                                )
                            },
                        )
                        .map_err(|_| BodyError {
                            kind: BodyErrorKind::SymNotFound { name: name.clone() },
                            span,
                        })?;
                    Ok(ContextResolution::Item(Receiver::Expr(expr), item))
                } else if ty.is_dyn_array() {
                    Ok(ContextResolution::Array(expr))
                } else {
                    Err(BodyError { kind: BodyErrorKind::NonObjectType { found: ty }, span })
                }
            }
            ast::Context::Bare => {
                self.access_bare_item(self.body_scope, name, span)?.ok_or(BodyError {
                    kind: BodyErrorKind::NotYetImplemented("unknown freestanding symbol"),
                    span,
                })
            }
            ast::Context::Const(_) => unreachable!(),
        }
    }

    fn access_bare_item(
        &mut self,
        scope: DefId,
        name: &Identifier,
        span: Span,
    ) -> Result<Option<ContextResolution>, BodyError> {
        match self.ctx.resolver.get_scoped_item(
            scope,
            self.ctx.defs,
            ScopeWalkKind::Access,
            name,
            |d| matches!(self.ctx.defs.get_def(d).kind, DefKind::Function(_) | DefKind::Var(_)),
        ) {
            Ok(i) => {
                let def = self.ctx.defs.get_def(i);
                match &def.kind {
                    DefKind::Var(_) => {
                        let s = self.body.add_expr(Expr {
                            ty: ExprTy::Ty(self.self_ty.ok_or(BodyError {
                                kind: BodyErrorKind::InvalidSelfAccess,
                                span,
                            })?),
                            kind: ExprKind::Place(PlaceExprKind::SelfAccess),
                            span: None,
                        });
                        Ok(Some(ContextResolution::Item(Receiver::Expr(s), i)))
                    }
                    DefKind::Function(f) => {
                        // access to bare function
                        // TODO: This disallows binding non-static functions to the static self in delegate
                        // creation, which code may rely on heavily?
                        let recv = if self.self_ty.is_some() && !f.flags.contains(FuncFlags::STATIC)
                        {
                            let s = self.body.add_expr(Expr {
                                ty: ExprTy::Ty(self.self_ty.ok_or(BodyError {
                                    kind: BodyErrorKind::InvalidSelfAccess,
                                    span,
                                })?),
                                kind: ExprKind::Place(PlaceExprKind::SelfAccess),
                                span: None,
                            });
                            Receiver::Expr(s)
                        } else {
                            Receiver::StaticSelf
                        };
                        Ok(Some(ContextResolution::Item(recv, i)))
                    }
                    _ => unreachable!(),
                }
            }
            Err(_) => {
                let outer = self.get_next_outer(scope);
                if let Some(outer) = outer {
                    // Symbol not found here -- try the outer
                    match self.access_bare_item(outer, name, span)? {
                        Some(res) => {
                            let (receiver, item) = match res {
                                ContextResolution::Item(recv, item) => match recv {
                                    Receiver::StaticSelf => (recv, item),
                                    Receiver::Expr(e) => {
                                        let expr = self.body.get_expr(e);
                                        match &expr.kind {
                                            ExprKind::Place(_) => {
                                                let outer_access = self.body.add_expr(Expr {
                                                    ty: ExprTy::Ty(Ty::object_from(outer)),
                                                    kind: ExprKind::Place(PlaceExprKind::Field(
                                                        Receiver::Expr(e),
                                                        self.ctx.special_items.outer_var.unwrap(),
                                                    )),
                                                    span: None,
                                                });
                                                (Receiver::Expr(outer_access), item)
                                            }
                                            _ => unreachable!(),
                                        }
                                    }
                                    _ => unreachable!(),
                                },
                                ContextResolution::Array(_) => unreachable!("bare access"),
                            };
                            Ok(Some(ContextResolution::Item(receiver, item)))
                        }
                        None => Ok(None),
                    }
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn get_next_outer(&mut self, scope: DefId) -> Option<DefId> {
        let mut look_at_class = self.ctx.defs.get_item_class(scope);
        loop {
            match self.ctx.defs.get_class(look_at_class).kind.as_ref().unwrap() {
                ClassKind::Class { extends, within, .. } => match within {
                    x @ Some(_) => break *x,
                    None => match extends {
                        Some(e) => look_at_class = *e,
                        None => break None,
                    },
                },
                ClassKind::Interface { .. } => break None,
            }
        }
    }

    fn lower_expr(
        &mut self,
        expr: &'hir ast::Expr,
        ty_expec: TypeExpectation,
    ) -> Result<ExprId, BodyError> {
        // If we coerce our type anyway, it'd be bad to eagerly expect something different
        let (lhs_ty_hint, passdown_ty_expec) = match ty_expec {
            TypeExpectation::RequiredTy(t) => {
                (TypeExpectation::HintTy(t), TypeExpectation::RequiredTy(t))
            }
            TypeExpectation::HintTy(t) => (TypeExpectation::HintTy(t), TypeExpectation::None),
            TypeExpectation::PlaceTy(_, _) => (TypeExpectation::None, TypeExpectation::None),
            TypeExpectation::CoerceToTy(_) | TypeExpectation::None => {
                (TypeExpectation::None, TypeExpectation::None)
            }
        };
        let (kind, ty) = match &expr.kind {
            ast::ExprKind::IndexExpr { base, idx } => {
                let idx = self.lower_expr(idx, TypeExpectation::RequiredTy(Ty::INT))?;
                let base_id = self.lower_expr(base, TypeExpectation::None)?;
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
                (ExprKind::Place(PlaceExprKind::Index(base_id, idx)), ExprTy::Ty(inner_ty))
            }
            ast::ExprKind::FieldExpr { lhs, rhs } => {
                if let Some((kind, ty)) = self.try_enum_variant(lhs, rhs, ty_expec) {
                    (kind, ExprTy::Ty(ty))
                } else if let Some((kind, ty)) = self.try_const(lhs, rhs, ty_expec, expr.span)? {
                    (kind, ExprTy::Ty(ty))
                } else if let Some((kind, ty)) = self.try_local_or_arg(lhs, rhs) {
                    (kind, ExprTy::Ty(ty))
                } else {
                    let def_or_array = self.translate_context(lhs, rhs, expr.span)?;
                    match def_or_array {
                        ContextResolution::Item(recv, item) => {
                            let def = self.ctx.defs.get_def(item);
                            match &def.kind {
                                DefKind::Var(var) => {
                                    assert!(
                                        matches!(recv, Receiver::StaticSelf | Receiver::Expr(_)),
                                        "{} = {:?}",
                                        rhs,
                                        recv
                                    );
                                    let ty = if item == self.ctx.special_items.class_var.unwrap() {
                                        match recv {
                                            Receiver::StaticSelf => Ty::class_from(self.class_did),
                                            Receiver::Expr(e) => {
                                                let mut ty = self
                                                    .body
                                                    .get_expr_ty(e)
                                                    .expect_ty("class specialization");
                                                if ty.is_class() {
                                                    ty = ty.instanciate_class()
                                                }
                                                if ty.is_object() {
                                                    Ty::class_from(ty.get_def().unwrap())
                                                } else {
                                                    return Err(BodyError {
                                                        kind: BodyErrorKind::NonObjectType {
                                                            found: ty,
                                                        },
                                                        span: expr.span,
                                                    });
                                                }
                                            }
                                            _ => unreachable!(),
                                        }
                                    } else if item == self.ctx.special_items.outer_var.unwrap() {
                                        match recv {
                                            Receiver::Expr(e) => {
                                                let ty = self
                                                    .body
                                                    .get_expr_ty(e)
                                                    .expect_ty("outer specialization");
                                                if ty.is_object() {
                                                    match self.get_next_outer(ty.get_def().unwrap())
                                                    {
                                                        Some(outer) => Ty::object_from(outer),
                                                        None => Ty::object_from(
                                                            self.ctx
                                                                .special_items
                                                                .object_id
                                                                .unwrap(),
                                                        ),
                                                    }
                                                } else {
                                                    return Err(BodyError {
                                                        kind: BodyErrorKind::NonObjectType {
                                                            found: ty,
                                                        },
                                                        span: expr.span,
                                                    });
                                                }
                                            }
                                            _ => unreachable!(),
                                        }
                                    } else {
                                        var.ty.unwrap()
                                    };
                                    (
                                        ExprKind::Place(PlaceExprKind::Field(recv, item)),
                                        ExprTy::Ty(ty),
                                    )
                                }
                                DefKind::Const(_) => {
                                    match recv {
                                        Receiver::Expr(e) => {
                                            let ty =
                                                self.body.get_expr_ty(e).expect_ty("const access");
                                            if ty.is_object() {
                                                // TODO: Delete compiled code
                                                let (lit, ty) =
                                                    self.resolve_const(item, ty_expec)?;
                                                (
                                                    ExprKind::Value(ValueExprKind::Lit(lit)),
                                                    ExprTy::Ty(ty),
                                                )
                                            } else {
                                                todo!("downgrade?")
                                            }
                                        }
                                        _ => todo!("error?"),
                                    }
                                }
                                DefKind::Function(func) => {
                                    assert!(
                                        matches!(
                                            recv,
                                            Receiver::StaticSelf
                                                | Receiver::Static(_)
                                                | Receiver::Expr(_)
                                        ),
                                        "{} = {:?}",
                                        rhs,
                                        recv
                                    );
                                    if let TypeExpectation::PlaceTy(_, _) = ty_expec {
                                        assert!(func.flags.contains(FuncFlags::DELEGATE));
                                        let delegate_prop = func.delegate_prop.unwrap();
                                        let var = self.ctx.defs.get_var(delegate_prop);
                                        (
                                            ExprKind::Place(PlaceExprKind::Field(
                                                recv,
                                                delegate_prop,
                                            )),
                                            ExprTy::Ty(var.ty.unwrap()),
                                        )
                                    } else {
                                        (
                                            ExprKind::Value(ValueExprKind::DelegateCreation(
                                                recv, item,
                                            )),
                                            ExprTy::Ty(Ty::delegate_from(item)),
                                        )
                                    }
                                }
                                _ => unreachable!("{:?}.{:?}, {:?}", lhs, rhs, def.kind),
                            }
                        }
                        ContextResolution::Array(expr) => {
                            // Receiver is a dynamic array
                            assert!(rhs == "Length");
                            (ExprKind::Place(PlaceExprKind::DynArrayLen(expr)), ExprTy::Ty(Ty::INT))
                        }
                    }
                }
            }
            ast::ExprKind::FuncCallExpr { lhs, name, args } => {
                if let Some((kind, ty)) = self.try_builtin_func(lhs, name, args, expr.span)? {
                    (kind, ExprTy::Ty(ty))
                } else if let Some((kind, ty)) = self.try_cast(lhs, name, args, expr.span)? {
                    (kind, ExprTy::Ty(ty))
                } else if let Some((kind, ty)) = self.try_local_or_arg(lhs, name) {
                    if !ty.is_delegate() {
                        return Err(BodyError {
                            kind: BodyErrorKind::NotYetImplemented(
                                "delegate call through not delegate",
                            ),
                            span: expr.span,
                        });
                    }
                    let func_id = ty.get_def().unwrap();
                    let func = self.ctx.defs.get_func(func_id);
                    let access_expr = self.body.add_expr(Expr {
                        kind,
                        ty: ExprTy::Ty(ty),
                        span: Some(expr.span),
                    });
                    let (ret_ty, lowered_args) =
                        self.lower_call_sig(&func.sig, args, expr.span, func.flags)?;
                    (
                        ExprKind::Value(ValueExprKind::DelegateCall(
                            Receiver::Expr(access_expr),
                            func_id,
                            lowered_args.into_boxed_slice(),
                        )),
                        ret_ty,
                    )
                } else {
                    let def_or_array = self.translate_context(lhs, name, expr.span)?;
                    match def_or_array {
                        ContextResolution::Item(recv, item) => {
                            let def = self.ctx.defs.get_def(item);
                            match &def.kind {
                                DefKind::Var(v) => {
                                    let ty = v.ty.unwrap();
                                    if !ty.is_delegate() {
                                        return Err(BodyError {
                                            kind: BodyErrorKind::NotYetImplemented(
                                                "delegate call through not delegate",
                                            ),
                                            span: expr.span,
                                        });
                                    }
                                    let func_id = ty.get_def().unwrap();
                                    let func = self.ctx.defs.get_func(func_id);
                                    let access_expr = self.body.add_expr(Expr {
                                        kind: ExprKind::Place(PlaceExprKind::Field(recv, item)),
                                        ty: ExprTy::Ty(ty),
                                        span: Some(expr.span),
                                    });
                                    let (ret_ty, lowered_args) = self
                                        .lower_call_sig(&func.sig, args, expr.span, func.flags)?;
                                    (
                                        ExprKind::Value(ValueExprKind::DelegateCall(
                                            Receiver::Expr(access_expr),
                                            func_id,
                                            lowered_args.into_boxed_slice(),
                                        )),
                                        ret_ty,
                                    )
                                }
                                DefKind::Function(func) => {
                                    let (ret_ty, lowered_args) = self
                                        .lower_call_sig(&func.sig, args, expr.span, func.flags)?;
                                    (
                                        ExprKind::Value(ValueExprKind::FuncCall(
                                            recv,
                                            item,
                                            lowered_args.into_boxed_slice(),
                                        )),
                                        ret_ty,
                                    )
                                }
                                _ => unreachable!("{:?}.{:?}, {:?}", lhs, name, def.kind),
                            }
                        }
                        ContextResolution::Array(arr_expr) => {
                            self.lower_dyn_array_call(arr_expr, name, args, expr.span)?
                        }
                    }
                }
            }
            ast::ExprKind::ClassMetaCastExpr { ty, expr } => {
                let cast_ty = self.ctx.decode_ast_ty(ty, self.body_scope).unwrap();
                (self.lower_cast(expr, cast_ty)?, ExprTy::Ty(cast_ty))
            }
            ast::ExprKind::NewExpr { args, cls, arch } => {
                // There is an ambiguity here...
                let outer_name = match &**args {
                    [] => (None, None),
                    [outer] => (
                        outer
                            .as_ref()
                            .map(|o| self.lower_expr(o, TypeExpectation::None))
                            .transpose()?,
                        None,
                    ),
                    [outer, name] => (
                        outer
                            .as_ref()
                            .map(|o| self.lower_expr(o, TypeExpectation::None))
                            .transpose()?,
                        name.as_ref()
                            .map(|n| self.lower_expr(n, TypeExpectation::RequiredTy(Ty::STRING)))
                            .transpose()?,
                    ),
                    x => {
                        return Err(BodyError {
                            kind: BodyErrorKind::ArgCountError { expected: 2, got: x.len() as u32 },
                            span: expr.span,
                        });
                    }
                };
                let cls_expr = self.lower_expr(cls, TypeExpectation::None)?;
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
                    .map(|a| self.lower_expr(a, TypeExpectation::RequiredTy(inst_ty)))
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
            ast::ExprKind::PreOpExpr { op, rhs } => self.lower_unary_op(rhs, *op, |def| {
                self.ctx.defs.get_op(def).flags.contains(FuncFlags::PREOPERATOR)
            })?,
            ast::ExprKind::PostOpExpr { lhs, op } => self.lower_unary_op(lhs, *op, |def| {
                self.ctx.defs.get_op(def).flags.contains(FuncFlags::POSTOPERATOR)
            })?,
            ast::ExprKind::BinOpExpr { lhs, op, rhs } => {
                self.lower_bin_op(expr.span, lhs, rhs, *op, lhs_ty_hint)?
            }
            ast::ExprKind::TernExpr { cond, then, alt } => {
                let e = self.lower_expr(cond, TypeExpectation::RequiredTy(Ty::BOOL))?;
                let then = self.lower_expr(then, passdown_ty_expec)?;
                let alt = self.lower_expr(alt, passdown_ty_expec)?;
                let unification = self
                    .bidi_unify_tys(self.body.get_expr_ty(then), self.body.get_expr_ty(alt))
                    .ok_or(BodyError {
                        kind: BodyErrorKind::NotYetImplemented("ternary unification"),
                        span: expr.span,
                    })?;
                (ExprKind::Value(ValueExprKind::TernaryOp(e, then, alt)), unification)
            }
            ast::ExprKind::LiteralExpr { lit } => {
                let (lit, ty) = self.ast_to_middle_lit(lit, passdown_ty_expec, expr.span)?;
                (ExprKind::Value(ValueExprKind::Lit(lit)), ExprTy::Ty(ty))
            }
        };

        if let TypeExpectation::PlaceTy(expected_ty, mutable) = ty_expec {
            if !matches!(kind, ExprKind::Place(_)) {
                return Err(BodyError { kind: BodyErrorKind::ByRefArgNotPlace, span: expr.span });
            }

            match expected_ty {
                None => Ok(self.body.add_expr(Expr { ty, kind, span: Some(expr.span) })),
                Some(expected_ty) => {
                    let got_non_void = ty.ty_or(BodyError {
                        kind: BodyErrorKind::VoidType { expected: Some(expected_ty) },
                        span: expr.span,
                    })?;

                    let ty_match = self.ty_match(expected_ty, got_non_void);

                    match (ty_match, mutable) {
                        (Some(0), _) | (Some(_), false) => {
                            // Exact match -- mutability doesn't matter.
                            // Inexact match -- only for const out
                            Ok(self.body.add_expr(Expr { ty, kind, span: Some(expr.span) }))
                        }
                        (Some(_), true) | (None, _) => {
                            // Generalization but mutable -- error
                            // Ty mismatch -- error
                            Err(BodyError {
                                kind: BodyErrorKind::TyMismatch {
                                    expected: expected_ty,
                                    found: got_non_void,
                                },
                                span: expr.span,
                            })
                        }
                    }
                }
            }
        } else if let TypeExpectation::RequiredTy(expected_ty)
        | TypeExpectation::CoerceToTy(expected_ty) = ty_expec
        {
            let got_non_void = ty.ty_or(BodyError {
                kind: BodyErrorKind::VoidType { expected: Some(expected_ty) },
                span: expr.span,
            })?;
            if self.ty_match(expected_ty, got_non_void).is_some() {
                Ok(self.body.add_expr(Expr { ty, kind, span: Some(expr.span) }))
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
                            let inner_expr =
                                self.body.add_expr(Expr { ty, kind, span: Some(expr.span) });
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
            Ok(self.body.add_expr(Expr { ty, kind, span: Some(expr.span) }))
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
        lit: &ast::Literal,
        expected_type: TypeExpectation,
        span: Span,
    ) -> Result<(Literal, Ty), BodyError> {
        match lit {
            ast::Literal::None => Ok((Literal::None, Ty::NONE)),
            ast::Literal::ObjReference(a, b) => {
                if a == "class" {
                    let parts = b
                        .as_ref()
                        .split('.')
                        .map(|s| Identifier::from_str(s.trim()).unwrap())
                        .collect::<Box<_>>();

                    let referenced_class = match &*parts {
                        [] => panic!("empty class"),
                        [name] => {
                            self.ctx.resolver.get_ty(self.body_scope, self.ctx.defs, name).map_err(
                                |_| BodyError {
                                    kind: BodyErrorKind::SymNotFound { name: name.clone() },
                                    span,
                                },
                            )?
                        }
                        [package, name] => self
                            .ctx
                            .resolver
                            .get_ty_in(Some(self.body_scope), self.ctx.defs, package, name)
                            .map_err(|_| BodyError {
                                kind: BodyErrorKind::SymNotFound { name: name.clone() },
                                span,
                            })?,
                        [..] => panic!("too many name parts"),
                    };

                    Ok((Literal::Class(referenced_class), Ty::class_from(referenced_class)))
                } else {
                    let ty = self.ctx.resolver.get_ty(self.body_scope, self.ctx.defs, a).map_err(
                        |_| BodyError {
                            kind: BodyErrorKind::SymNotFound { name: a.clone() },
                            span,
                        },
                    )?;
                    Ok((Literal::Object(ty), Ty::object_from(ty)))
                }
            }
            ast::Literal::Bool(b) => Ok((Literal::Bool(*b), Ty::BOOL)),
            ast::Literal::Name(_) => Ok((Literal::Name, Ty::NAME)),
            ast::Literal::String(_) => Ok((Literal::String, Ty::STRING)),

            ast::Literal::Int(i) => Ok(self.adjust_int(*i, expected_type)),
            ast::Literal::Float(f) => Ok((Literal::Float(*f), Ty::FLOAT)),
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
            // TODO: Lint truncation
            (Literal::Byte(i as u8), Ty::BYTE)
        } else {
            (Literal::Int(i), Ty::INT)
        }
    }

    fn resolve_const(
        &self,
        konst: DefId,
        ty_expec: TypeExpectation,
    ) -> Result<(Literal, Ty), BodyError> {
        let val = self.ctx.defs.get_const(konst);
        match &val.val {
            ConstVal::Literal(l) => match l {
                Literal::Bool(_) => Ok((l.clone(), Ty::FLOAT)),
                Literal::Int(i) => Ok((l.clone(), self.adjust_int(*i, ty_expec).1)),
                Literal::Float(_) => Ok((l.clone(), Ty::FLOAT)),
                Literal::Name => Ok((l.clone(), Ty::NAME)),
                Literal::String => Ok((l.clone(), Ty::STRING)),
                _ => unreachable!(),
            },
            ConstVal::Redirect(i) => {
                let (did, _) =
                    self.ctx.resolve_const(konst, i, ScopeWalkKind::Access).ok_or_else(|| {
                        BodyError {
                            kind: BodyErrorKind::SymNotFound { name: i.clone() },
                            span: self.ctx.defs.get_def(konst).span.unwrap(),
                        }
                    })?;
                // Exciting potential for a stack overflow here
                self.resolve_const(did, ty_expec)
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
                    DefKind::Class(c) => match c.kind.as_ref().unwrap() {
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
        let general_func_sig = &self.ctx.defs.get_func(general).sig;
        let specific_func_sig = &self.ctx.defs.get_func(specific).sig;

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

        for (&arg1, &arg2) in general_func_sig.args.iter().zip(specific_func_sig.args.iter()) {
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
