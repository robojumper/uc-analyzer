use uc_ast::{
    visit::{self, Visitor},
    Block, Hir, Statement, StatementKind,
};
use uc_files::{ErrorCode, ErrorReport, Fragment, Level, Sources, Span};

struct DanglingElseVisitor {
    errs: Vec<DanglingElse>,
}

pub struct DanglingElse {
    pub whole_thing: Span,
}

pub fn run(hir: &Hir, _: &Sources) -> Vec<ErrorReport> {
    let mut visitor = DanglingElseVisitor { errs: vec![] };
    visitor.visit_hir(hir);
    visitor
        .errs
        .iter()
        .map(|err| ErrorReport {
            code: ErrorCode { msg: "dangling-else", level: Level::Warning, priority: 3 },
            msg: "if if else is ambiguous".to_owned(),
            fragments: vec![Fragment {
                full_text: err.whole_thing,
            inlay_messages: vec![("this one is if (...) { if (...) { ... } else { ... } } but could be interpreted as if (...) { if (...) { ... } } else { ... }".to_owned(), err.whole_thing)],}]
        })
        .collect()
}

impl Visitor for DanglingElseVisitor {
    const VISIT_EXPRS: bool = false;

    fn visit_statement(&mut self, stmt: &Statement) {
        visit::walk_statement(self, stmt);
        if let StatementKind::IfStatement { then, .. } = &stmt.kind {
            if then.from_single_stmt {
                if let [ref inner_statement] = *then.stmts {
                    if let StatementKind::IfStatement {
                        or_else: Some(Block { from_single_stmt: true, .. }),
                        ..
                    } = &inner_statement.kind
                    {
                        self.errs.push(DanglingElse { whole_thing: stmt.span })
                    }
                }
            }
        }
    }
}
