use bitflags::bitflags;

use uc_files::{ErrorReport, Fragment, Sources, Span};
use uc_middle::{
    body::{BlockId, Body, StatementKind, StmtId},
    DefKind, Defs,
};

bitflags! {
    pub struct Diverge: u8 {
        const RETURN = 1 << 0;
        const CONTINUE = 1 << 1;
        const BREAK = 1 << 2;
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Effect {
    div_options: Diverge,
    guaranteed_div: Option<StmtId>,
}

impl Effect {
    const FALL: Effect = Effect { div_options: Diverge::empty(), guaranteed_div: None };
}

#[derive(Clone, Copy)]
struct Unreachable {
    div_stmt: StmtId,
    unr_stmt: StmtId,
}

struct UnreachableStmtFinder<'a> {
    body: &'a Body,
    errs: Vec<Unreachable>,
}

pub fn run(defs: &Defs, _: &Sources) -> Vec<ErrorReport> {
    let mut errs = vec![];

    // Finds statements that are referenced in a function body,
    // but never executed due to a return, break, or continue.

    for def in defs.iter() {
        let (span, stmts, must_return) = match &def.kind {
            DefKind::State(s) => (def.span, &s.contents, false),
            DefKind::Operator(o) => match &o.contents {
                Some(c) => (def.span, &c.statements, o.sig.ret_ty.is_some()),
                None => continue,
            },
            DefKind::Function(f) => match &f.contents {
                Some(c) => (def.span, &c.statements, f.sig.ret_ty.is_some()),
                None => continue,
            },
            _ => continue,
        };

        let stmts = match stmts {
            Some(stmts) => stmts,
            _ => continue,
        };

        let mut finder = UnreachableStmtFinder { body: stmts, errs: vec![] };
        let effect = finder.block_effect(stmts.get_entry());
        errs.extend(finder.errs.iter().map(|e| {
            let stmt_a = stmts.get_stmt(e.div_stmt).span.unwrap();
            let stmt_b = stmts.get_stmt(e.unr_stmt).span.unwrap();
            let full_span = Span { start: stmt_a.start, end: stmt_b.end };
            ErrorReport {
                code: "unreachable-statement",
                msg: "a statement will never be executed".to_owned(),
                fragments: vec![Fragment {
                    full_text: full_span,
                    inlay_messages: vec![
                        ("this statement diverges...".to_owned(), stmt_a),
                        ("... so this statement will never run".to_owned(), stmt_b),
                    ],
                }],
            }
        }));

        // Sanity check: No stray continues/breaks
        assert!(!effect.div_options.intersects(Diverge::CONTINUE | Diverge::BREAK));

        if must_return && effect.guaranteed_div.is_none() {
            errs.push(ErrorReport {
                code: "missing-return",
                msg: "this function returns a type but not all paths return a value".to_owned(),
                fragments: vec![Fragment { full_text: span.unwrap(), inlay_messages: vec![] }],
            })
        }
    }

    errs
}

impl UnreachableStmtFinder<'_> {
    fn block_effect(&mut self, id: BlockId) -> Effect {
        let block = self.body.get_block(id);
        self.stmts_effect(&block.stmts, false)
    }

    fn stmts_effect(&mut self, stmts: &[StmtId], allow_trailing_break: bool) -> Effect {
        let mut ex_effect = Effect::FALL;
        for (idx, &stmt) in stmts.iter().enumerate() {
            if let Some(div_id) = ex_effect.guaranteed_div {
                // The UC switch/case pattern is to include a trailing break; in a switch case
                // even if the previous statement is a return/continue, so don't lint here
                if allow_trailing_break && idx == stmts.len() - 1 {
                    let check = self.body.get_stmt(stmt);
                    if matches!(&check.kind, StatementKind::Break) {
                        break;
                    }
                }
                self.errs.push(Unreachable { div_stmt: div_id, unr_stmt: stmt });
                break;
            }
            let stmt_effect = self.stmt_effect(stmt);
            ex_effect = combine_seq(ex_effect, stmt_effect);
        }
        ex_effect
    }

    fn stmt_effect(&mut self, id: StmtId) -> Effect {
        let stmt = self.body.get_stmt(id);
        match &stmt.kind {
            StatementKind::Expr(_) => Effect::FALL,
            StatementKind::If(_, then, or_else) => {
                let then_effect = self.block_effect(*then);
                let or_else_effect = or_else.map(|b| self.block_effect(b)).unwrap_or(Effect::FALL);
                combine_alt(id, then_effect, or_else_effect)
            }
            StatementKind::Loop(entry, retry, run, _) => {
                if let Some(e) = entry {
                    assert!(matches!(self.stmt_effect(*e), Effect::FALL));
                }
                if let Some(r) = retry {
                    assert!(matches!(self.stmt_effect(*r), Effect::FALL));
                }

                let block_effect = self.block_effect(*run);
                // Every loop will have a theoretical exit condition, which could be `if(false) break;`,
                // but that's still a legitimate exit. UC does not have a syntax for truly infinite loops.
                assert!(!block_effect.div_options.is_empty());

                if block_effect.guaranteed_div.is_some()
                    && !block_effect.div_options.contains(Diverge::CONTINUE)
                {
                    // If a loop never falls through and doesn't continue, this loop never loops... but we lint on
                    // the ast level, so this condition isn't actually interesting.
                }

                let eaten = block_effect.div_options.difference(Diverge::BREAK | Diverge::CONTINUE);
                if block_effect.div_options.contains(Diverge::BREAK) {
                    // if our block can break, then we don't diverge
                    Effect { div_options: eaten, guaranteed_div: None }
                } else {
                    // otherwise, our block only continues or returns (can happen in `do { return; } while(_);` loops),
                    // so the whole statement diverges.
                    Effect { div_options: eaten, guaranteed_div: Some(id) }
                }
            }
            StatementKind::Switch(_, cases, default, block_id) => {
                let block = self.body.get_block(*block_id);
                let mut idxs = cases.iter().map(|(_, idx)| *idx).collect::<Vec<_>>();
                idxs.extend(default);
                idxs.push(block.stmts.len() as u32);
                idxs.sort_unstable();
                idxs.dedup();

                let effects = idxs
                    .array_windows::<2>()
                    .map(|&[start, end]| {
                        self.stmts_effect(&block.stmts[start as usize..end as usize], true)
                    })
                    .collect::<Vec<_>>();
                // Every case label is reachable!
                let possible_divs =
                    effects.iter().fold(Diverge::empty(), |acc, eff| acc.union(eff.div_options));
                if !default.is_some()
                    || possible_divs.contains(Diverge::BREAK)
                    || effects.last().expect("switch/case without cases").guaranteed_div.is_none()
                {
                    // If we have no default branch, we might skip the whole block altogether. Otherwise, if one of the
                    // label sub-blocks breaks, or the last sub-block may fall through, then the following statement is
                    // also reachable
                    Effect {
                        div_options: possible_divs.difference(Diverge::BREAK),
                        guaranteed_div: None,
                    }
                } else {
                    // We always take a branch, none of our branches break, and the last sub-block diverges.
                    // This is the only way the switch statement itself is guaranteed to diverge.
                    Effect {
                        div_options: possible_divs.difference(Diverge::BREAK),
                        guaranteed_div: Some(id),
                    }
                }
            }
            StatementKind::Return(_) => {
                Effect { div_options: Diverge::RETURN, guaranteed_div: Some(id) }
            }
            StatementKind::Break => {
                Effect { div_options: Diverge::BREAK, guaranteed_div: Some(id) }
            }
            StatementKind::Continue => {
                Effect { div_options: Diverge::CONTINUE, guaranteed_div: Some(id) }
            }
            StatementKind::Assign(_, _) => Effect::FALL,
            StatementKind::Label => Effect::FALL,
        }
    }
}

fn combine_seq(first: Effect, second: Effect) -> Effect {
    match (first.guaranteed_div, second.guaranteed_div) {
        (Some(_), _) => panic!("checked stmt after diverging statement"),
        (None, opt) => {
            Effect { div_options: first.div_options.union(second.div_options), guaranteed_div: opt }
        }
    }
}

fn combine_alt(stmt: StmtId, first: Effect, second: Effect) -> Effect {
    let potential_exits = first.div_options.union(second.div_options);
    match (first.guaranteed_div, second.guaranteed_div) {
        (Some(_), Some(_)) => Effect { div_options: potential_exits, guaranteed_div: Some(stmt) },
        (_, _) => Effect { div_options: potential_exits, guaranteed_div: None },
    }
}
