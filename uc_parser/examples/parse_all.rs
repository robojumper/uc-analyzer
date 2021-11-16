use core::panic;
use std::fs;
use std::str::FromStr;

use uc_ast::{Expr, Op, Statement, StatementKind};
use uc_files::Sources;
use uc_name::Identifier;
use uc_parser::{lexer, parser};
use walkdir::{DirEntry, WalkDir};

fn is_uc(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| s.ends_with(".uc") || s.ends_with(".UC"))
        .unwrap_or(false)
}

fn main() {
    let dir = std::env::args().nth(1).expect("missing directory");
    let preprocessed = if dir.contains("PreProcessedFiles") {
        true
    } else if dir.contains("Development") {
        false
    } else {
        panic!("Don't know if preprocessed or not.");
    };

    let walker = WalkDir::new(dir).into_iter();
    let mut hirs = vec![];
    let mut sources = Sources::new();

    for entry in walker {
        let entry = match entry {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{:?}", e);
                continue;
            }
        };

        if !is_uc(&entry) {
            continue;
        }

        let name = entry.file_name().to_str().unwrap();

        let contents = match fs::read(entry.path()) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("{:?}: I/O Error {:?}", entry.path(), e);
                continue;
            }
        };

        if !preprocessed && contents.contains(&b'`') {
            continue; // will be expanded
        }

        let id = sources
            .add_file(
                Identifier::from_str(&name[0..name.find('.').unwrap()]).unwrap(),
                &contents,
            )
            .expect("invalid encoding");

        eprintln!("{}", name);

        let lexer = lexer::Lexer::new(&sources, id);
        let (hir, errs) = parser::parse(lexer);
        if !errs.is_empty() {
            dbg!(&name);
            dbg!(&errs);
            panic!();
        }
        /*
        let out = std::io::stdout();
        let mut out = out.lock();
        uc_ast::pretty::format_hir(&hir, &mut out, uc_ast::pretty::IdentifierFormat).unwrap();
        */
        for func in &hir.funcs {
            if let Some(body) = &func.body {
                visit_statements(&body.statements);
            }
        }
        for state in &hir.states {
            visit_statements(&state.statements);
        }

        hirs.push(hir);
    }
    let mut buffer = String::new();
    let stdin = std::io::stdin(); // We get `Stdin` here.
    stdin.read_line(&mut buffer).unwrap();
}

fn visit_statements(statements: &[Statement]) {
    for s in statements {
        visit_statement(s);
    }
}

fn visit_statement(statement: &Statement) {
    match &statement.kind {
        StatementKind::IfStatement { cond, then, or_else } => {
            visit_statements(&then.stmts);
            if let Some(b) = or_else {
                visit_statements(&b.stmts);
            }
        },
        StatementKind::ForStatement { init, cond, retry, run } => {
            visit_statement(init);
            visit_statement(retry);
            visit_statements(&run.stmts);
        },
        StatementKind::ForeachStatement { source, run } => {
            visit_statements(&run.stmts)
        },
        StatementKind::WhileStatement { cond, run } => {
            visit_statements(&run.stmts)
        },
        StatementKind::DoStatement { cond, run } => {
            visit_statements(run)
        },
        StatementKind::SwitchStatement { scrutinee, cases } => {
            for case in cases {
                visit_statements(&case.statements)
            }
        },
        StatementKind::BreakStatement => {},
        StatementKind::ContinueStatement => {},
        StatementKind::ReturnStatement { expr } => {},
        StatementKind::Label(_) => {},
        StatementKind::Assignment { lhs, rhs } => {},
        StatementKind::Expr { expr } => {
            if !expr_side_effectful(expr) {
                panic!("Expression statement has no effect: {:?}", expr);
            }
        },
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
    match op {
        Op::AtAssign | Op::AddAssign | Op::DivAssign | Op::MulAssign | Op::SubAssign | Op::DollarAssign => true,
        _ => false,
    }
}