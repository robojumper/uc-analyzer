use uc_ast::{
    visit::{self, Visitor},
    Expr, ExprKind, Hir, Op, Statement, StatementKind,
};
use uc_files::Span;

struct UneffectfulStmtsVisitor {
    errs: Vec<(&'static str, Span)>,
}

impl Visitor for UneffectfulStmtsVisitor {
    const VISIT_EXPRS: bool = false;

    fn visit_statement(&mut self, stmt: &Statement) {
        visit::walk_statement(self, stmt);
        if let StatementKind::Expr { expr } = &stmt.kind {
            if let Some(err) = expr_no_effects(expr) {
                self.errs.push((err, stmt.span));
            }
        }
    }
}

pub fn visit_hir(hir: &Hir) -> Vec<(&'static str, Span)> {
    let mut visitor = UneffectfulStmtsVisitor { errs: vec![] };
    for func in &hir.funcs {
        if let Some(body) = &func.body {
            visit::walk_statements(&mut visitor, &body.statements);
        }
    }
    for state in &hir.states {
        visit::walk_statements(&mut visitor, &state.statements);
    }

    visitor.errs
}

fn expr_no_effects(expr: &Expr) -> Option<&'static str> {
    match &expr.kind {
        ExprKind::IndexExpr { base: _, idx: _ } => Some("index expression has no side effect"),
        ExprKind::FieldExpr { lhs: _, rhs: _ } => Some("place expression has no side effect"),
        ExprKind::CallExpr { lhs: _, args: _ } => None,
        ExprKind::ClassMetaCastExpr { ty: _, expr: _ } => {
            Some("class meta cast expression has no side effect")
        }
        ExprKind::NewExpr {
            args: _,
            cls: _,
            arch: _,
        } => Some("class construction has no side effect"),
        ExprKind::PreOpExpr { op: _, rhs: _ } => None,
        ExprKind::PostOpExpr { lhs: _, op: _ } => None,
        ExprKind::BinOpExpr { lhs: _, op, rhs: _ } => op_no_effects(*op),
        ExprKind::TernExpr { cond: _, then, alt } => {
            if expr_no_effects(then).is_some() || expr_no_effects(alt).is_some() {
                Some("a branch in this ternary expression has no effect")
            } else {
                None
            }
        }
        ExprKind::SymExpr { sym: _ } => Some("symbol expression has no effect"),
        ExprKind::LiteralExpr { lit: _ } => Some("literal expression has no effect"),
    }
}

fn op_no_effects(op: Op) -> Option<&'static str> {
    match op {
        Op::AtAssign
        | Op::AddAssign
        | Op::DivAssign
        | Op::MulAssign
        | Op::SubAssign
        | Op::DollarAssign => None,
        _ => Some("operator has no side effect"),
    }
}
