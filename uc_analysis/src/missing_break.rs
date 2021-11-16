use uc_ast::{
    visit::{self, StatementVisitor},
    CaseClause, Hir, Statement, StatementKind,
};
use uc_files::{Sources, Span};

struct MissingBreakVisitor<'a> {
    errs: Vec<MissingBreak>,
    source: &'a Sources,
}

pub struct MissingBreak {
    pub from_label: Span,
    pub to_label: Span,
    pub and_then_executes: Option<Span>,
}

pub fn visit_hir(hir: &'_ Hir, source: &'_ Sources) -> Vec<MissingBreak> {
    let mut visitor = MissingBreakVisitor {
        errs: vec![],
        source,
    };
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

impl StatementVisitor for MissingBreakVisitor<'_> {
    fn visit_statement(&mut self, stmt: &Statement) {
        visit::walk_statement(self, stmt);
        if let StatementKind::SwitchStatement {
            scrutinee: _,
            cases,
        } = &stmt.kind
        {
            self.check_switch_statement(stmt, cases);
        }
    }
}

impl MissingBreakVisitor<'_> {
    fn check_switch_statement(&mut self, _statement: &Statement, cases: &[CaseClause]) {
        let ccs = cases
            .iter()
            .filter(|&cc| !cc.stmts.is_empty())
            .collect::<Vec<_>>();
        for cs in ccs.windows(2) {
            let cs1 = &cs[0];
            let cs2 = &cs[1];

            if cs1.stmts.is_empty() {
                // no statements in case means quite intentional fallthrough
                continue;
            }

            if last_statement_unconditionally_returns(&cs1.stmts) {
                // last statement is return -- obviously not falling through
                continue;
            }

            if any_break(&cs1.stmts) {
                // there is an explicit break statement somewhere
                continue;
            }

            let search_span = Span {
                start: cs1.stmts.last().unwrap().span.end,
                end: cs2.case_span.start,
            };

            if let Ok(text) = self.source.lookup_str(search_span) {
                if text.contains("fallthrough")
                    || text.contains("fall through")
                    || text.contains("missing break")
                    || text.contains("fall down")
                {
                    continue;
                }
            }

            let and_then_executes = match &*cs2.stmts {
                [] => None,
                [s1] => Some(s1.span),
                [s1, .., s2] => Some(Span {
                    start: s1.span.start,
                    end: s2.span.end,
                }),
            };

            self.errs.push(MissingBreak {
                from_label: cs1.case_span,
                to_label: cs2.case_span,
                and_then_executes,
            })
        }
    }
}

fn last_statement_unconditionally_returns(stmts: &[Statement]) -> bool {
    match stmts.last() {
        Some(stmt) => match &stmt.kind {
            StatementKind::IfStatement {
                cond: _,
                then,
                or_else,
            } => {
                let then_returns = last_statement_unconditionally_returns(&then.stmts);
                let or_else_returns = match or_else {
                    Some(oe) => last_statement_unconditionally_returns(&oe.stmts),
                    None => false,
                };
                then_returns && or_else_returns
            }
            StatementKind::ReturnStatement { expr: _ } => true,
            _ => false,
        },
        None => false,
    }
}

struct BreakFinder {
    found_break: bool,
}

impl StatementVisitor for BreakFinder {
    fn visit_statement(&mut self, stmt: &Statement) {
        visit::walk_statement(self, stmt);
        if let StatementKind::BreakStatement = &stmt.kind {
            self.found_break = true;
        }
    }
}

fn any_break(stmts: &[Statement]) -> bool {
    let mut visitor = BreakFinder { found_break: false };
    visit::walk_statements(&mut visitor, stmts);
    visitor.found_break
}
