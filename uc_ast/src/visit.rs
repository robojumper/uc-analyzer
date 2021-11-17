use crate::{Expr, Statement, StatementKind};

macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr) => {
        for elem in $list {
            $visitor.$method(elem)
        }
    };
    ($visitor: expr, $method: ident, $list: expr, $($extra_args: expr),*) => {
        for elem in $list {
            $visitor.$method(elem, $($extra_args,)*)
        }
    }
}

pub trait Visitor: Sized {
    const VISIT_EXPRS: bool;

    fn visit_statements(&mut self, stmts: &[Statement]) {
        walk_statements(self, stmts)
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        walk_statement(self, stmt)
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr)
    }
}

pub fn walk_statements<V: Visitor>(visit: &mut V, stmts: &[Statement]) {
    walk_list!(visit, visit_statement, stmts)
}

pub fn walk_statement<V: Visitor>(visit: &mut V, stmt: &Statement) {
    match &stmt.kind {
        StatementKind::IfStatement {
            cond,
            then,
            or_else,
        } => {
            maybe_visit_expr(visit, cond);
            walk_list!(visit, visit_statement, &then.stmts);
            if let Some(b) = or_else {
                walk_list!(visit, visit_statement, &b.stmts);
            }
        }
        StatementKind::ForStatement {
            init,
            cond,
            retry,
            run,
        } => {
            visit.visit_statement(init);
            maybe_visit_expr(visit, cond);
            visit.visit_statement(retry);
            walk_list!(visit, visit_statement, &run.stmts);
        }
        StatementKind::ForeachStatement { source, run } => {
            maybe_visit_expr(visit, source);
            walk_list!(visit, visit_statement, &run.stmts)
        }
        StatementKind::WhileStatement { cond, run } => {
            maybe_visit_expr(visit, cond);
            walk_list!(visit, visit_statement, &run.stmts)
        }
        StatementKind::DoStatement { cond, run } => {
            walk_list!(visit, visit_statement, run);
            maybe_visit_expr(visit, cond);
        }
        StatementKind::SwitchStatement { scrutinee, cases } => {
            maybe_visit_expr(visit, scrutinee);
            for case in cases {
                walk_list!(visit, visit_statement, &case.stmts);
            }
        }
        StatementKind::BreakStatement => {}
        StatementKind::ContinueStatement => {}
        StatementKind::ReturnStatement { expr } => {
            if let Some(expr) = expr {
                maybe_visit_expr(visit, expr);
            }
        }
        StatementKind::Label(_) => {}
        StatementKind::Assignment { lhs, rhs } => {
            maybe_visit_expr(visit, lhs);
            maybe_visit_expr(visit, rhs);
        }
        StatementKind::Expr { expr } => {
            maybe_visit_expr(visit, expr);
        }
    }
}

#[inline]
fn maybe_visit_expr<V: Visitor>(visit: &mut V, expr: &Expr) {
    if V::VISIT_EXPRS {
        visit.visit_expr(expr);
    }
}

pub fn walk_expr<V: Visitor>(visit: &mut V, expr: &Expr) {
    match &expr.kind {
        crate::ExprKind::IndexExpr { base, idx } => {
            visit.visit_expr(base);
            visit.visit_expr(idx);
        }
        crate::ExprKind::FieldExpr { lhs, rhs: _ } => {
            visit.visit_expr(lhs);
        }
        crate::ExprKind::CallExpr { lhs, args } => {
            visit.visit_expr(lhs);
            for arg in args.iter().flatten() {
                visit.visit_expr(arg);
            }
        }
        crate::ExprKind::ClassMetaCastExpr { ty: _, expr } => {
            visit.visit_expr(expr);
        }
        crate::ExprKind::NewExpr { args, cls, arch } => {
            visit.visit_expr(cls);
            if let Some(arch) = arch {
                visit.visit_expr(arch);
            }
        }
        crate::ExprKind::PreOpExpr { op: _, rhs } => {
            visit.visit_expr(rhs);
        }
        crate::ExprKind::PostOpExpr { lhs, op: _ } => {
            visit.visit_expr(lhs);
        }
        crate::ExprKind::BinOpExpr { lhs, op: _, rhs } => {
            visit.visit_expr(lhs);
            visit.visit_expr(rhs);
        }
        crate::ExprKind::TernExpr { cond, then, alt } => {
            visit.visit_expr(cond);
            visit.visit_expr(then);
            visit.visit_expr(alt);
        }
        crate::ExprKind::SymExpr { sym: _ } => {}
        crate::ExprKind::LiteralExpr { lit: _ } => {}
    }
}
