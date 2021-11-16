use uc_ast::{Expr, Hir, Op, Statement, StatementKind};

pub fn visit_hir(hir: &Hir) {
    for func in &hir.funcs {
        if let Some(body) = &func.body {
            visit_statements(&body.statements);
        }
    }
    for state in &hir.states {
        visit_statements(&state.statements);
    }
}

fn visit_statements(statements: &[Statement]) {
    for s in statements {
        visit_statement(s);
    }
}

fn visit_statement(statement: &Statement) {
    match &statement.kind {
        StatementKind::IfStatement {
            cond: _,
            then,
            or_else,
        } => {
            visit_statements(&then.stmts);
            if let Some(b) = or_else {
                visit_statements(&b.stmts);
            }
        }
        StatementKind::ForStatement {
            init,
            cond,
            retry,
            run,
        } => {
            visit_statement(init);
            visit_statement(retry);
            visit_statements(&run.stmts);
        }
        StatementKind::ForeachStatement { source, run } => visit_statements(&run.stmts),
        StatementKind::WhileStatement { cond, run } => visit_statements(&run.stmts),
        StatementKind::DoStatement { cond, run } => visit_statements(run),
        StatementKind::SwitchStatement { scrutinee, cases } => {
            for case in cases {
                visit_statements(&case.statements)
            }
        }
        StatementKind::BreakStatement => {}
        StatementKind::ContinueStatement => {}
        StatementKind::ReturnStatement { expr } => {}
        StatementKind::Label(_) => {}
        StatementKind::Assignment { lhs, rhs } => {}
        StatementKind::Expr { expr } => {
            if !expr_side_effectful(expr) {
                panic!("Expression statement has no effect: {:?}", expr);
            }
        }
    }
}

fn expr_side_effectful(expr: &Expr) -> bool {
    match &expr {
        Expr::IndexExpr { base, idx } => false,
        Expr::FieldExpr { lhs, rhs } => false,
        Expr::CallExpr { lhs, args } => true,
        Expr::ClassMetaCastExpr { ty, expr } => false,
        Expr::NewExpr { args, cls, arch } => false,
        Expr::PreOpExpr { op, rhs } => true,
        Expr::PostOpExpr { lhs, op } => true,
        Expr::BinOpExpr { lhs, op, rhs } => op_side_effectful(*op),
        Expr::TernExpr { cond, then, alt } => expr_side_effectful(then) || expr_side_effectful(alt),
        Expr::SymExpr { sym } => false,
        Expr::LiteralExpr { lit } => false,
    }
}

fn op_side_effectful(op: Op) -> bool {
    matches!(
        op,
        Op::AtAssign
            | Op::AddAssign
            | Op::DivAssign
            | Op::MulAssign
            | Op::SubAssign
            | Op::DollarAssign
    )
}
