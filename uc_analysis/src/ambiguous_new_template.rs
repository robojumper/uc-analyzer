use uc_ast::{
    visit::{self, Visitor},
    Expr, ExprKind, Hir,
};
use uc_files::{ErrorReport, Sources, Span};

struct AmbigNewVisitor {
    errs: Vec<AmbigNew>,
}

pub struct AmbiguousNew;

pub fn run(hir: &Hir, _: &Sources) -> Vec<ErrorReport> {
    let mut visitor = AmbigNewVisitor { errs: vec![] };
    visitor.visit_hir(hir);
    visitor
        .errs
        .iter()
        .map(|err| ErrorReport {
            code: "ambiguous-new",
            full_text: err.new_expr,
            msg: "new with function call is ambiguous".to_owned(),
            inlay_messages: vec![(
                "this could be a function call or a field reference with template arguments"
                    .to_owned(),
                err.cls_expr,
            )],
        })
        .collect()
}

pub struct AmbigNew {
    pub new_expr: Span,
    pub cls_expr: Span,
}

impl Visitor for AmbigNewVisitor {
    const VISIT_EXPRS: bool = true;

    fn visit_expr(&mut self, expr: &Expr) {
        visit::walk_expr(self, expr);
        if let ExprKind::NewExpr { cls, .. } = &expr.kind {
            if !cls.paren {
                if let ExprKind::FuncCallExpr { lhs: Some(lhs), .. } = &cls.kind {
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
