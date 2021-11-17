use uc_ast::{
    visit::{self, Visitor},
    Expr, ExprKind, Hir,
};
use uc_files::Span;

struct AmbigNewVisitor {
    errs: Vec<AmbigNew>,
}

pub struct AmbigNew {
    pub new_expr: Span,
    pub cls_expr: Span,
}

pub fn visit_hir(hir: &'_ Hir) -> Vec<AmbigNew> {
    let mut visitor = AmbigNewVisitor { errs: vec![] };
    for func in &hir.funcs {
        if let Some(body) = &func.body {
            visitor.visit_statements(&body.statements);
        }
    }
    for state in &hir.states {
        visitor.visit_statements(&state.statements);
    }

    visitor.errs
}

impl Visitor for AmbigNewVisitor {
    const VISIT_EXPRS: bool = true;

    fn visit_expr(&mut self, expr: &Expr) {
        visit::walk_expr(self, expr);
        if let ExprKind::NewExpr { cls, .. } = &expr.kind {
            if !cls.paren {
                if let ExprKind::CallExpr { lhs, .. } = &cls.kind {
                    if let ExprKind::FieldExpr { rhs, .. } = &lhs.kind {
                        if rhs != "class" {
                            self.errs.push(AmbigNew {
                                new_expr: expr.span,
                                cls_expr: cls.span,
                            });
                        }
                    }
                    if let ExprKind::SymExpr { .. } = &lhs.kind {
                        self.errs.push(AmbigNew {
                            new_expr: expr.span,
                            cls_expr: cls.span,
                        });
                    }
                }
            }
        }
    }
}
