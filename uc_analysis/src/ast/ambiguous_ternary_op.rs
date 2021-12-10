use uc_ast::{
    uc_def::Op,
    visit::{self, Visitor},
    Context, Expr, ExprKind, Hir,
};
use uc_files::{ErrorReport, Fragment, Sources, Span};

struct AmbigTernVisitor {
    errs: Vec<AmbigTern>,
}

pub struct AmbiguousTernary;

pub fn run(hir: &Hir, _: &Sources) -> Vec<ErrorReport> {
    let mut visitor = AmbigTernVisitor { errs: vec![] };
    visitor.visit_hir(hir);
    visitor
        .errs
        .iter()
        .map(|err| ErrorReport {
            code: "ambiguous-ternary-op",
            msg: "this ternary operator has dangerous ambiguous syntax".to_owned(),
            fragments: vec![Fragment {
                full_text: err.whole_expr,
                inlay_messages: vec![(
                    "conventionally, this operator has a higher precedence than `?...`"
                        .to_owned(),
                    err.lhs_expr,
                ), (
                    "... but if this is a bool, the `?` immediately binds it first, then the previous operator binds the entire ternary expression"
                        .to_owned(),
                    err.potentially_stronger_bool,
                )],
            }],
        })
        .collect()
}

pub struct AmbigTern {
    pub whole_expr: Span,
    pub lhs_expr: Span,
    pub potentially_stronger_bool: Span,
}

impl Visitor for AmbigTernVisitor {
    const VISIT_EXPRS: bool = true;

    fn visit_expr(&mut self, expr: &Expr) {
        visit::walk_expr(self, expr);
        if let ExprKind::TernExpr { cond, .. } = &expr.kind {
            if !cond.paren {
                if let ExprKind::BinOpExpr { rhs, op, .. } | ExprKind::PreOpExpr { rhs, op } =
                    &cond.kind
                {
                    if matches!(op, Op::Bang | Op::EqEq | Op::BangEq | Op::PowPow | Op::OrOr) {
                        self.errs.push(AmbigTern {
                            lhs_expr: cond.span,
                            potentially_stronger_bool: rhs.span,
                            whole_expr: expr.span,
                        });
                    }
                }
            }
        }
    }
}
