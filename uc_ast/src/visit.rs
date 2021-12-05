use crate::{Expr, ExprKind, Hir, Statement, StatementKind};

pub trait Visitor: Sized {
    const VISIT_EXPRS: bool;

    fn visit_hir(&mut self, hir: &Hir) {
        walk_hir(self, hir)
    }

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

pub fn walk_hir<V: Visitor>(visit: &mut V, hir: &Hir) {
    for func in &hir.funcs {
        if let Some(body) = &func.body {
            visit.visit_statements(&body.statements);
        }
        if V::VISIT_EXPRS {
            for arg in &func.sig.args {
                if let Some(def) = &arg.def {
                    visit.visit_expr(def);
                }
            }
        }
    }

    for state in &hir.states {
        visit.visit_statements(&state.statements);
    }
}

pub fn walk_statements<V: Visitor>(visit: &mut V, stmts: &[Statement]) {
    for stmt in stmts {
        visit.visit_statement(stmt);
    }
}

pub fn walk_statement<V: Visitor>(visit: &mut V, stmt: &Statement) {
    match &stmt.kind {
        StatementKind::IfStatement {
            cond,
            then,
            or_else,
        } => {
            maybe_visit_expr(visit, cond);
            visit.visit_statements(&then.stmts);
            if let Some(b) = or_else {
                visit.visit_statements(&b.stmts);
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
            visit.visit_statements(&run.stmts);
        }
        StatementKind::ForeachStatement { source, run } => {
            maybe_visit_expr(visit, source);
            visit.visit_statements(&run.stmts)
        }
        StatementKind::WhileStatement { cond, run } => {
            maybe_visit_expr(visit, cond);
            visit.visit_statements(&run.stmts)
        }
        StatementKind::DoStatement { cond, run } => {
            visit.visit_statements(&run.stmts);
            maybe_visit_expr(visit, cond);
        }
        StatementKind::SwitchStatement { scrutinee, cases } => {
            maybe_visit_expr(visit, scrutinee);
            for case in cases {
                visit.visit_statements(&case.stmts);
            }
        }
        StatementKind::BreakStatement => {}
        StatementKind::ContinueStatement => {}
        StatementKind::ReturnStatement { expr } => {
            if let Some(expr) = expr {
                maybe_visit_expr(visit, expr);
            }
        }
        StatementKind::Label { .. } => {}
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
        ExprKind::IndexExpr { base, idx } => {
            visit.visit_expr(base);
            visit.visit_expr(idx);
        }
        ExprKind::FieldExpr { lhs, rhs: _ } => {
            visit.visit_expr(lhs);
        }
        ExprKind::FuncCallExpr { lhs, name: _, args } => {
            if let Some(lhs) = lhs {
                visit.visit_expr(lhs);
            }
            for arg in args.iter().flatten() {
                visit.visit_expr(arg);
            }
        }
        ExprKind::ClassMetaCastExpr { ty: _, expr } => {
            visit.visit_expr(expr);
        }
        ExprKind::NewExpr { args, cls, arch } => {
            for arg in args.iter().flatten() {
                visit.visit_expr(arg);
            }
            visit.visit_expr(cls);
            if let Some(arch) = arch {
                visit.visit_expr(arch);
            }
        }
        ExprKind::PreOpExpr { op: _, rhs } => {
            visit.visit_expr(rhs);
        }
        ExprKind::PostOpExpr { lhs, op: _ } => {
            visit.visit_expr(lhs);
        }
        ExprKind::BinOpExpr { lhs, op: _, rhs } => {
            visit.visit_expr(lhs);
            visit.visit_expr(rhs);
        }
        ExprKind::TernExpr { cond, then, alt } => {
            visit.visit_expr(cond);
            visit.visit_expr(then);
            visit.visit_expr(alt);
        }
        ExprKind::SymExpr { sym: _ } => {}
        ExprKind::LiteralExpr { lit: _ } => {}
    }
}
