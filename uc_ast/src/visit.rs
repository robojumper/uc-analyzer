use crate::{Statement, StatementKind};

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

pub trait StatementVisitor: Sized {
    fn visit_statements(&mut self, stmts: &[Statement]) {
        walk_statements(self, stmts)
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        walk_statement(self, stmt)
    }
}

pub fn walk_statements<V: StatementVisitor>(visit: &mut V, stmts: &[Statement]) {
    walk_list!(visit, visit_statement, stmts)
}

pub fn walk_statement<V: StatementVisitor>(visit: &mut V, stmt: &Statement) {
    match &stmt.kind {
        StatementKind::IfStatement {
            cond: _,
            then,
            or_else,
        } => {
            walk_list!(visit, visit_statement, &then.stmts);
            if let Some(b) = or_else {
                walk_list!(visit, visit_statement, &b.stmts);
            }
        }
        StatementKind::ForStatement {
            init,
            cond: _,
            retry,
            run,
        } => {
            visit.visit_statement(init);
            visit.visit_statement(retry);
            walk_list!(visit, visit_statement, &run.stmts);
        }
        StatementKind::ForeachStatement { source: _, run } => {
            walk_list!(visit, visit_statement, &run.stmts)
        }
        StatementKind::WhileStatement { cond: _, run } => {
            walk_list!(visit, visit_statement, &run.stmts)
        }
        StatementKind::DoStatement { cond: _, run } => walk_list!(visit, visit_statement, run),
        StatementKind::SwitchStatement {
            scrutinee: _,
            cases,
        } => {
            for case in cases {
                walk_list!(visit, visit_statement, &case.stmts);
            }
        }
        StatementKind::BreakStatement => {}
        StatementKind::ContinueStatement => {}
        StatementKind::ReturnStatement { expr: _ } => {}
        StatementKind::Label(_) => {}
        StatementKind::Assignment { lhs: _, rhs: _ } => {}
        StatementKind::Expr { expr: _ } => {}
    }
}
