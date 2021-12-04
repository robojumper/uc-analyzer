use std::str::FromStr;

use uc_def::{ArgFlags, FuncFlags};
use uc_files::Span;
use uc_middle::{
    body::{
        Block, BlockId, Body, Expr, ExprId, ExprKind, ExprTy, Literal, LoopDesugaring,
        PlaceExprKind, Statement, StatementKind, StmtId, ValueExprKind,
    },
    ty::{self, Ty},
    ClassKind, DefId, FuncSig, ScopeWalkKind,
};
use uc_name::Identifier;

use crate::LoweringContext;

struct FuncLowerer<'hir, 'a: 'hir> {
    ctx: &'a LoweringContext<'hir>,
    body: Body,
    func_scope: DefId,
    class_did: DefId,
    class_ty: Ty,
    self_ty: Option<Ty>,
}

#[derive(Debug)]
pub struct BodyError {
    pub kind: BodyErrorKind,
    pub span: Span,
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
    /// TODO
    NotYetImplemented,
}

impl<'hir> LoweringContext<'hir> {
    pub fn lower_body(
        &self,
        func_scope: DefId,
        statements: &[uc_ast::Statement],
    ) -> Result<Body, BodyError> {
        let class_did = self.defs.get_item_class(func_scope);
        let class_ty = self.defs.get_class(class_did).self_ty;
        let self_ty = if self
            .defs
            .get_func(func_scope)
            .flags
            .contains(FuncFlags::STATIC)
        {
            None
        } else {
            Some(class_ty)
        };

        let lowerer = FuncLowerer {
            ctx: self,
            body: Body::new(),
            func_scope,
            class_ty,
            class_did,
            self_ty,
        };
        lowerer.lower_body(statements)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
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
                let e = self.lower_expr(cond, Some(Ty::BOOL), false, false)?;
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
                let c = self.lower_expr(cond, Some(Ty::BOOL), false, false)?;
                let r = self.lower_statement(retry)?;
                let mut inner_stmts = self.lower_statements(&run.stmts)?;
                inner_stmts.push(r);
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
                    whole_block,
                    LoopDesugaring::For {
                        init: i,
                        cond: c,
                        retry: r,
                    },
                )
            }
            uc_ast::StatementKind::ForeachStatement { source, run } => {
                return Err(BodyError {
                    kind: BodyErrorKind::NotYetImplemented,
                    span: stmt.span,
                })
            }
            uc_ast::StatementKind::WhileStatement { cond, run } => {
                let c = self.lower_expr(cond, Some(Ty::BOOL), false, false)?;
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

                StatementKind::Loop(None, whole_block, LoopDesugaring::While { cond: c })
            }
            uc_ast::StatementKind::DoStatement { cond, run } => {
                let c = self.lower_expr(cond, Some(Ty::BOOL), false, false)?;
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

                StatementKind::Loop(None, whole_block, LoopDesugaring::Do { cond: c })
            }
            uc_ast::StatementKind::SwitchStatement { scrutinee, cases } => {
                let scrut = self.lower_expr(scrutinee, None, false, false)?;
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
                            let expr = self.lower_expr(c, Some(scrut_ty), false, false)?;
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
                let ret_ty = self
                    .ctx
                    .defs
                    .get_func(self.func_scope)
                    .sig
                    .as_ref()
                    .unwrap()
                    .ret_ty;
                let e = expr
                    .as_ref()
                    .map(|e| self.lower_expr(e, ret_ty, false, false))
                    .transpose()?;
                StatementKind::Return(e)
            }
            uc_ast::StatementKind::Label { name } => {
                return Err(BodyError {
                    kind: BodyErrorKind::NotYetImplemented,
                    span: stmt.span,
                })
            }
            uc_ast::StatementKind::Assignment { lhs, rhs } => {
                let l = self.lower_expr(lhs, None, true, false)?;
                let l_ty = self.body.get_expr(l).ty.ty_or(BodyError {
                    kind: BodyErrorKind::VoidType { expected: None },
                    span: lhs.span,
                })?;
                assert!(!l_ty.is_stat_array());
                let r = self.lower_expr(rhs, Some(l_ty), false, false)?;
                StatementKind::Assign(l, r)
            }
            uc_ast::StatementKind::Expr { expr } => {
                StatementKind::Expr(self.lower_expr(expr, None, false, false)?)
            }
        };
        Ok(self.body.add_stmt(Statement {
            kind,
            span: Some(stmt.span),
        }))
    }

    fn lower_cast(
        &mut self,
        inner_expr: &'hir uc_ast::Expr,
        to_type: Ty,
    ) -> Result<ExprKind, BodyError> {
        let expr = self.lower_expr(inner_expr, None, false, false)?;
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
                kind: BodyErrorKind::NotYetImplemented,
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
                    let exp_id = self.lower_expr(
                        arg_expr,
                        Some(arg.ty),
                        false,
                        arg.flags.contains(ArgFlags::COERCE),
                    )?;
                    let expr = self.body.get_expr(exp_id);
                    if arg
                        .flags
                        .contains(ArgFlags::OUT | ArgFlags::OUTONLY | ArgFlags::REF)
                        && !matches!(expr.kind, ExprKind::Place(_))
                    {
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

    fn lower_expr(
        &mut self,
        expr: &'hir uc_ast::Expr,
        expected_type: Option<Ty>,
        assign: bool,
        coerce: bool,
    ) -> Result<ExprId, BodyError> {
        // If we coerce our type anyway, it'd be bad to eagerly expect something different
        let passdown_expected_type = if coerce { None } else { expected_type };
        let (kind, ty) = match &expr.kind {
            uc_ast::ExprKind::IndexExpr { base, idx } => {
                let idx = self.lower_expr(idx, Some(Ty::INT), false, false)?;
                let base_id = self.lower_expr(base, None, false, false)?;
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
                // TODO: Pseudo-field-exprs like x.?default.field / x.?const.field
                let l = self.lower_expr(lhs, None, false, false)?;
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
                } else if ty.is_object() || ty.is_struct() || ty.is_class() {
                    let scope = if ty.is_class() {
                        self.ctx.special_items.object_id.unwrap()
                    } else {
                        ty.get_def().unwrap()
                    };
                    let prop = self
                        .ctx
                        .resolver
                        .get_scoped_var(scope, self.ctx.defs, ScopeWalkKind::Access, rhs)
                        .map_err(|_| BodyError {
                            kind: BodyErrorKind::SymNotFound { name: rhs.clone() },
                            span: expr.span,
                        })?;
                    let var = self.ctx.defs.get_var(prop);
                    (
                        ExprKind::Place(PlaceExprKind::Field(l, prop)),
                        ExprTy::Ty(var.ty.unwrap()),
                    )
                } else {
                    todo!("invalid field access: {:?}, {:?}", lhs, rhs)
                }
            }
            uc_ast::ExprKind::FuncCallExpr { lhs, name, args } => {
                match lhs {
                    Some(lhs) => {
                        if matches!(&lhs.kind, uc_ast::ExprKind::FieldExpr { rhs, .. } | uc_ast::ExprKind::SymExpr { sym: rhs } if rhs == "static")
                        {
                            let receiver = match &lhs.kind {
                                uc_ast::ExprKind::FieldExpr { lhs, .. } => match &lhs.kind {
                                    uc_ast::ExprKind::LiteralExpr { lit } => {
                                        match self.ast_to_middle_lit(lit, lhs.span)? {
                                            (x @ Literal::Class(_), ty) => {
                                                self.body.add_expr(Expr {
                                                    kind: ExprKind::Value(ValueExprKind::Lit(x)),
                                                    ty: ExprTy::Ty(ty),
                                                    span: Some(lhs.span),
                                                })
                                            }
                                            _ => {
                                                return Err(BodyError {
                                                    kind: BodyErrorKind::NotYetImplemented,
                                                    span: expr.span,
                                                })
                                            }
                                        }
                                    }
                                    _ => {
                                        return Err(BodyError {
                                            kind: BodyErrorKind::NotYetImplemented,
                                            span: expr.span,
                                        })
                                    }
                                },
                                uc_ast::ExprKind::SymExpr { .. } => self.body.add_expr(Expr {
                                    kind: ExprKind::Value(ValueExprKind::Lit(Literal::Class(
                                        self.class_did,
                                    ))),
                                    ty: ExprTy::Ty(Ty::class_from(self.class_did)),
                                    span: Some(lhs.span),
                                }),
                                _ => unreachable!(),
                            };

                            let receiver_def = self.body.get_expr_ty(receiver);

                            let func = self
                                .ctx
                                .resolver
                                .get_scoped_func(
                                    receiver_def
                                        .expect_ty("just created class")
                                        .get_def()
                                        .unwrap(),
                                    self.ctx.defs,
                                    ScopeWalkKind::Access,
                                    name,
                                )
                                .map_err(|_| BodyError {
                                    kind: BodyErrorKind::FuncNotFound { name: name.clone() },
                                    span: expr.span,
                                })?;
                            let def = self.ctx.defs.get_func(func);
                            assert!(!def.flags.intersects(
                                FuncFlags::OPERATOR
                                    | FuncFlags::PREOPERATOR
                                    | FuncFlags::POSTOPERATOR
                                    | FuncFlags::ITERATOR
                            ));

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
                        } else {
                            let receiver = self.lower_expr(lhs, None, false, false)?;
                            let mut receiver_ty =
                                self.body.get_expr_ty(receiver).ty_or(BodyError {
                                    kind: BodyErrorKind::VoidType { expected: None },
                                    span: lhs.span,
                                })?;
                            if receiver_ty.is_class() {
                                // downgrade classes to objects
                                receiver_ty = Ty::object_from(receiver_ty.get_def().unwrap());
                            }

                            if receiver_ty.is_dyn_array() {
                                return Err(BodyError {
                                    kind: BodyErrorKind::NotYetImplemented,
                                    span: expr.span,
                                });
                            } else if receiver_ty.is_interface() || receiver_ty.is_object() {
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
                            } else {
                                todo!("call on unknown type {:?}", receiver_ty)
                            }
                        }
                    }
                    None => {
                        // A freestanding function call could be a cast. Check if name is a type
                        match self.ctx.decode_simple_ty(name, self.func_scope) {
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
                            None => {
                                match self.ctx.resolver.get_scoped_func(
                                    self.func_scope,
                                    self.ctx.defs,
                                    ScopeWalkKind::Access,
                                    name,
                                ) {
                                    Ok(func) => {
                                        let def = self.ctx.defs.get_func(func);
                                        assert!(!def.flags.intersects(
                                            FuncFlags::OPERATOR
                                                | FuncFlags::PREOPERATOR
                                                | FuncFlags::POSTOPERATOR
                                                | FuncFlags::ITERATOR
                                        ));
                                        let receiver = if def.flags.contains(FuncFlags::STATIC) {
                                            self.body.add_expr(Expr {
                                                kind: ExprKind::Value(ValueExprKind::Lit(
                                                    Literal::Class(self.class_did),
                                                )),
                                                ty: ExprTy::Ty(self.class_ty),
                                                span: None,
                                            })
                                        } else {
                                            self.body.add_expr(Expr {
                                                ty: ExprTy::Ty(self.self_ty.ok_or(BodyError {
                                                    kind: BodyErrorKind::InvalidSelfAccess,
                                                    span: expr.span,
                                                })?),
                                                kind: ExprKind::Place(PlaceExprKind::SelfAccess),
                                                span: None,
                                            })
                                        };
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
                                    Err(_) => {
                                        return Err(BodyError {
                                            kind: BodyErrorKind::FuncNotFound {
                                                name: name.clone(),
                                            },
                                            span: expr.span,
                                        })
                                    }
                                }
                            }
                        }
                    }
                }
            }
            uc_ast::ExprKind::DelegateCallExpr { lhs, args } => {
                return Err(BodyError {
                    kind: BodyErrorKind::NotYetImplemented,
                    span: expr.span,
                })
            }
            uc_ast::ExprKind::ClassMetaCastExpr { ty, expr } => {
                let cast_ty = self.ctx.decode_ast_ty(ty, self.func_scope).unwrap();
                (self.lower_cast(expr, cast_ty)?, ExprTy::Ty(cast_ty))
            }
            uc_ast::ExprKind::NewExpr { args, cls, arch } => {
                // There is an ambiguity here...
                let outer_name = match &**args {
                    [] => (None, None),
                    [outer, name] => (
                        outer
                            .as_ref()
                            .map(|o| self.lower_expr(o, None, false, false))
                            .transpose()?,
                        name.as_ref()
                            .map(|n| self.lower_expr(n, Some(Ty::STRING), false, false))
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
                let cls_expr = self.lower_expr(cls, None, false, false)?;
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
                    .map(|a| self.lower_expr(a, Some(inst_ty), false, false))
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
            uc_ast::ExprKind::PreOpExpr { op, rhs } => {
                return Err(BodyError {
                    kind: BodyErrorKind::NotYetImplemented,
                    span: expr.span,
                })
            }
            uc_ast::ExprKind::PostOpExpr { lhs, op } => {
                return Err(BodyError {
                    kind: BodyErrorKind::NotYetImplemented,
                    span: expr.span,
                })
            }
            uc_ast::ExprKind::BinOpExpr { lhs, op, rhs } => {
                return Err(BodyError {
                    kind: BodyErrorKind::NotYetImplemented,
                    span: expr.span,
                })
            }
            uc_ast::ExprKind::TernExpr { cond, then, alt } => {
                let e = self.lower_expr(cond, Some(Ty::BOOL), false, false)?;
                let then = self.lower_expr(then, passdown_expected_type, false, false)?;
                let alt = self.lower_expr(alt, passdown_expected_type, false, false)?;
                (
                    ExprKind::Value(ValueExprKind::TernaryOp(e, then, alt)),
                    self.bidi_unify_tys(self.body.get_expr_ty(then), self.body.get_expr_ty(alt)),
                )
            }
            uc_ast::ExprKind::SymExpr { sym } => {
                // This could be 1. a constant in scope, 2. an enum value, 3. a local or arg 4. a class variable or function 5. a self 6. delegate function
                // TODO: Order?
                if let Ok(var) = self.ctx.resolver.get_scoped_var(
                    self.func_scope,
                    self.ctx.defs,
                    ScopeWalkKind::Access,
                    sym,
                ) {
                    let def = self.ctx.defs.get_def(var);
                    match &def.kind {
                        uc_middle::DefKind::Var(var) => {
                            let s = self.body.add_expr(Expr {
                                ty: ExprTy::Ty(self.self_ty.ok_or(BodyError {
                                    kind: BodyErrorKind::InvalidSelfAccess,
                                    span: expr.span,
                                })?),
                                kind: ExprKind::Place(PlaceExprKind::SelfAccess),
                                span: None,
                            });
                            (
                                ExprKind::Place(PlaceExprKind::Field(s, def.id)),
                                ExprTy::Ty(var.ty.unwrap()),
                            )
                        }
                        uc_middle::DefKind::FuncArg(arg) => (
                            ExprKind::Place(PlaceExprKind::Arg(def.id)),
                            ExprTy::Ty(arg.ty),
                        ),
                        uc_middle::DefKind::Local(local) => (
                            ExprKind::Place(PlaceExprKind::Arg(def.id)),
                            ExprTy::Ty(local.ty),
                        ),
                        _ => panic!("unexpected def {:?} for sym {:?}", def, sym),
                    }
                } else if sym == "self" {
                    (
                        ExprKind::Place(PlaceExprKind::SelfAccess),
                        ExprTy::Ty(self.self_ty.expect("access to self var in static func")),
                    )
                } else if sym == "super" {
                    return Err(BodyError {
                        kind: BodyErrorKind::NotYetImplemented,
                        span: expr.span,
                    });
                } else if let Ok(konst) = self.ctx.resolver.get_scoped_const(
                    self.func_scope,
                    self.ctx.defs,
                    ScopeWalkKind::Access,
                    sym,
                ) {
                    return Err(BodyError {
                        kind: BodyErrorKind::NotYetImplemented,
                        span: expr.span,
                    });
                    (
                        ExprKind::Value(ValueExprKind::Const(konst)),
                        ExprTy::Ty(self.konst_ty(konst)),
                    )
                } else if let Ok(en) =
                    self.ctx
                        .resolver
                        .get_global_value(self.func_scope, self.ctx.defs, sym)
                {
                    let parent_enum = self.ctx.defs.get_variant(en).owning_enum;
                    (
                        ExprKind::Value(ValueExprKind::Lit(Literal::Byte)),
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
                let (lit, ty) = self.ast_to_middle_lit(lit, expr.span)?;
                (ExprKind::Value(ValueExprKind::Lit(lit)), ExprTy::Ty(ty))
            }
        };

        if let Some(expected_ty) = expected_type {
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
                        if auto || coerce {
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
    fn bidi_unify_tys(&self, left: ExprTy, right: ExprTy) -> ExprTy {
        if let (ExprTy::Ty(l), ExprTy::Ty(r)) = (left, right) {
            let left_to_right = self.ty_match(l, r);
            let right_to_left = self.ty_match(r, l);
            match (left_to_right, right_to_left) {
                (None, None) => {
                    panic!("question mark: failed to unify {:?} with {:?}", l, r);
                    // TODO: get lowest common ancestor for classes
                }
                // L is subtype of R
                (None, Some(_)) => right,
                (Some(_), None) => left,
                (Some(_), Some(_)) => {
                    l.assert_literally_same(r);
                    left
                }
            }
        } else {
            ExprTy::Void
        }
    }

    fn ast_to_middle_lit(
        &self,
        lit: &uc_ast::Literal,
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
                            .get_ty(self.func_scope, self.ctx.defs, name)
                            .map_err(|e| BodyError {
                                kind: BodyErrorKind::SymNotFound { name: name.clone() },
                                span,
                            })?,
                        [package, name] => self
                            .ctx
                            .resolver
                            .get_ty_in(Some(self.func_scope), self.ctx.defs, package, name)
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
                        .get_ty(self.func_scope, self.ctx.defs, a)
                        .map_err(|e| BodyError {
                            kind: BodyErrorKind::SymNotFound { name: a.clone() },
                            span,
                        })?;
                    Ok((Literal::Object(ty), Ty::object_from(ty)))
                }
            }
            uc_ast::Literal::Number => Ok((Literal::Float, Ty::FLOAT)),
            uc_ast::Literal::Bool => Ok((Literal::Bool, Ty::BOOL)),
            uc_ast::Literal::Name => Ok((Literal::Name, Ty::NAME)),
            uc_ast::Literal::String(_) => Ok((Literal::String, Ty::STRING)),
        }
    }

    fn konst_ty(&self, konst: DefId) -> Ty {
        todo!()
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

    /// Returns whether src is a subtype of dest
    fn ty_match(&self, dest: Ty, src: Ty) -> Option<u16> {
        ty::is_subtype(
            dest,
            src,
            self.ctx.special_items.object_id.unwrap(),
            &|general, specific| self.inheritance_distance(general, specific),
        )
    }
}
