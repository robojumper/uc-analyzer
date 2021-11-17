use uc_ast::{
    visit::{self, Visitor},
    Block, Hir, Statement, StatementKind,
};
use uc_files::Span;

struct DanglingElseVisitor {
    errs: Vec<DanglingElse>,
}

pub struct DanglingElse {
    pub whole_thing: Span,
}

pub fn visit_hir(hir: &'_ Hir) -> Vec<DanglingElse> {
    let mut visitor = DanglingElseVisitor { errs: vec![] };
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

impl Visitor for DanglingElseVisitor {
    const VISIT_EXPRS: bool = false;

    fn visit_statement(&mut self, stmt: &Statement) {
        visit::walk_statement(self, stmt);
        if let StatementKind::IfStatement { then, .. } = &stmt.kind {
            if then.from_single_stmt {
                if let [ref inner_statement] = *then.stmts {
                    if let StatementKind::IfStatement {
                        or_else:
                            Some(Block {
                                from_single_stmt: true,
                                ..
                            }),
                        ..
                    } = &inner_statement.kind
                    {
                        self.errs.push(DanglingElse {
                            whole_thing: stmt.span,
                        })
                    }
                }
            }
        }
    }
}
