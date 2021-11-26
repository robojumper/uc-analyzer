// Implementation mostly cribbed off https://github.com/rust-lang/rust-clippy/blob/5a6169d7f662bfa418f4dc1d1cb6bf725dd5f680/clippy_lints/src/loops/never_loop.rs

use uc_ast::{
    visit::{self, Visitor},
    Block, Expr, ExprKind, Hir, Literal, Statement, StatementKind,
};
use uc_files::{ErrorReport, Sources, Span};

struct NeverLoopVisitor {
    errs: Vec<NeverLoop>,
}

pub struct NeverLoop {
    pub loop_span: Span,
    pub cond_span: Span,
}

pub fn run(hir: &Hir, _: &Sources) -> Vec<ErrorReport> {
    let mut visitor = NeverLoopVisitor { errs: vec![] };
    visitor.visit_hir(hir);
    visitor
        .errs
        .iter()
        .map(|err| ErrorReport {
            code: "never-loop",
            full_text: err.loop_span,
            msg: "loop never runs twice".to_owned(),
            inlay_messages: vec![(
                "this loop always breaks in the first iteration".to_owned(),
                err.cond_span,
            )],
        })
        .collect()
}

impl Visitor for NeverLoopVisitor {
    const VISIT_EXPRS: bool = false;

    fn visit_statement(&mut self, stmt: &Statement) {
        visit::walk_statement(self, stmt);
        let (block, span) = if let StatementKind::DoStatement { run, cond }
        | StatementKind::ForStatement { run, cond, .. }
        | StatementKind::WhileStatement { run, cond, .. } = &stmt.kind
        {
            (run, cond.span)
        } else if let StatementKind::ForeachStatement { run, source } = &stmt.kind {
            if let &[.., ref stmt] = &*run.stmts {
                // Special case: a foreach loop with a class argument where the last statement
                // always breaks is likely a "discovery loop" like
                //
                // foreach VisualizeGameState.IterateByClassType(class'XComGameState_Unit', UnitState)
                // {
                //     break;
                // }

                if let ExprKind::FuncCallExpr { args, .. } = &source.kind {
                    if args.iter().flatten().any(|e| {
                        matches!(
                            e,
                            Expr {
                                kind: ExprKind::LiteralExpr {
                                    lit: Literal::ObjReference
                                },
                                ..
                            }
                        )
                    }) {
                        let mut checker = BreakContinueContext {
                            break_levels: 0,
                            continue_levels: 0,
                        };
                        if let NeverLoopResult::AlwaysBreak = checker.check_statement(stmt) {
                            return;
                        }
                    }
                } else {
                    panic!("foreach requires a function call")
                }
            }
            (run, source.span)
        } else {
            return;
        };

        let mut checker = BreakContinueContext {
            break_levels: 0,
            continue_levels: 0,
        };
        if let NeverLoopResult::AlwaysBreak = checker.check_block(block) {
            self.errs.push(NeverLoop {
                loop_span: stmt.span,
                cond_span: span,
            })
        }
    }
}

struct BreakContinueContext {
    break_levels: u32,
    continue_levels: u32,
}

enum NeverLoopResult {
    AlwaysBreak,
    MayContinue,
    Otherwise,
}

#[must_use]
fn combine_seq(first: NeverLoopResult, second: NeverLoopResult) -> NeverLoopResult {
    match first {
        NeverLoopResult::AlwaysBreak | NeverLoopResult::MayContinue => first,
        NeverLoopResult::Otherwise => second,
    }
}

// Combine two results where only one of the part may have been executed.
#[must_use]
fn combine_branches(b1: NeverLoopResult, b2: NeverLoopResult) -> NeverLoopResult {
    match (b1, b2) {
        (NeverLoopResult::AlwaysBreak, NeverLoopResult::AlwaysBreak) => {
            NeverLoopResult::AlwaysBreak
        }
        (NeverLoopResult::MayContinue, _) | (_, NeverLoopResult::MayContinue) => {
            NeverLoopResult::MayContinue
        }
        (NeverLoopResult::Otherwise, _) | (_, NeverLoopResult::Otherwise) => {
            NeverLoopResult::Otherwise
        }
    }
}

impl BreakContinueContext {
    fn check_block(&mut self, block: &Block) -> NeverLoopResult {
        self.check_statements(&block.stmts)
    }

    fn check_statements(&mut self, stmts: &[Statement]) -> NeverLoopResult {
        stmts
            .iter()
            .map(|s| self.check_statement(s))
            .fold(NeverLoopResult::Otherwise, combine_seq)
    }

    fn check_statement(&mut self, stmt: &Statement) -> NeverLoopResult {
        match &stmt.kind {
            StatementKind::IfStatement { then, or_else, .. } => {
                let then_result = self.check_block(then);
                let or_else_result = or_else
                    .as_ref()
                    .map_or(NeverLoopResult::Otherwise, |b| self.check_block(b));
                combine_branches(then_result, or_else_result)
            }
            StatementKind::ForStatement { run, .. } => {
                self.break_levels += 1;
                self.continue_levels += 1;
                let result = self.check_block(run);
                self.break_levels -= 1;
                self.continue_levels -= 1;
                result
            }
            StatementKind::ForeachStatement { run, .. } => {
                self.break_levels += 1;
                self.continue_levels += 1;
                let result = self.check_block(run);
                self.break_levels -= 1;
                self.continue_levels -= 1;
                result
            }
            StatementKind::WhileStatement { run, .. } => {
                self.break_levels += 1;
                self.continue_levels += 1;
                let result = self.check_block(run);
                self.break_levels -= 1;
                self.continue_levels -= 1;
                result
            }
            StatementKind::DoStatement { run, .. } => {
                self.break_levels += 1;
                self.continue_levels += 1;
                let result = self.check_block(run);
                self.break_levels -= 1;
                self.continue_levels -= 1;
                result
            }
            StatementKind::SwitchStatement { cases, .. } => {
                self.break_levels += 1;
                // This isn't really correct because multiple case clauses can be executed at once but
                // since a break in a case clause never results in AlwaysBreak, it's fine
                let result = cases
                    .iter()
                    .map(|cc| self.check_statements(&cc.stmts))
                    .fold(NeverLoopResult::Otherwise, combine_seq);
                self.break_levels -= 1;
                result
            }
            StatementKind::BreakStatement => {
                if self.break_levels == 0 {
                    NeverLoopResult::AlwaysBreak
                } else {
                    NeverLoopResult::Otherwise
                }
            }
            StatementKind::ContinueStatement => {
                if self.continue_levels == 0 {
                    NeverLoopResult::MayContinue
                } else {
                    NeverLoopResult::Otherwise
                }
            }
            StatementKind::ReturnStatement { .. } => NeverLoopResult::AlwaysBreak,
            StatementKind::Label { .. } => NeverLoopResult::Otherwise,
            StatementKind::Assignment { .. } => NeverLoopResult::Otherwise,
            StatementKind::Expr { .. } => NeverLoopResult::Otherwise,
        }
    }
}
