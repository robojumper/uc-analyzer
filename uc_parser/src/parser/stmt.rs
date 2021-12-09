use std::vec;

use uc_ast::{Block, Case, CaseClause, Expr, ExprKind, Statement, StatementKind};
use uc_files::Span;

use crate::{
    kw,
    lexer::{Token, TokenKind as Tk},
    sig,
};

use super::{ParseError, Parser};

/// Does this statement usually want to be terminated by a semicolon?
fn stmt_wants_semi(stmt: &StatementKind) -> bool {
    match stmt {
        StatementKind::IfStatement { .. } => false,
        StatementKind::ForStatement { .. } => false,
        StatementKind::ForeachStatement { .. } => false,
        StatementKind::WhileStatement { .. } => false,
        StatementKind::DoStatement { .. } => true,
        StatementKind::SwitchStatement { .. } => false,
        StatementKind::BreakStatement => true,
        StatementKind::ContinueStatement => true,
        StatementKind::ReturnStatement { .. } => true,
        StatementKind::Label { .. } => false,
        StatementKind::Assignment { .. } => true,
        StatementKind::Expr { .. } => true,
    }
}

impl Parser<'_> {
    fn parse_block_or_stmt(&mut self, ctx: (&'static str, Span)) -> Result<Block, ParseError> {
        if self.eat(Tk::Semi) {
            Ok(Block {
                stmts: vec![],
                from_single_stmt: true,
            })
        } else if self.eat(sig!(LBrace)) {
            let stmts = self.parse_statements();
            self.expect(sig!(RBrace))?;
            Ok(Block {
                stmts,
                from_single_stmt: false,
            })
        } else {
            let stmt = self.expect_one_statement(ctx, true)?;
            Ok(Block {
                stmts: vec![stmt],
                from_single_stmt: true,
            })
        }
    }

    fn expect_one_statement(
        &mut self,
        ctx: (&'static str, Span),
        expect_semi: bool,
    ) -> Result<Statement, ParseError> {
        let stmt = self.parse_one_stmt()?.ok_or_else(|| {
            let mut err = self.fmt_err("missing statement", None);
            err.err.ctx_token = Some(("statement follows ".to_owned() + ctx.0, ctx.1));
            err
        })?;
        if expect_semi && stmt_wants_semi(&stmt.kind) && !self.eat(Tk::Semi) {
            let mut err = self.fmt_err("missing semicolon after statement", self.peek());
            err.err.ctx_token = Some(("statement follows ".to_owned() + ctx.0, stmt.span));
            return Err(err);
        }
        Ok(stmt)
    }

    fn parse_single_case(&mut self) -> Result<Option<CaseClause>, ParseError> {
        let (case, case_span) = match self.peek_any()?.kind {
            kw!(Default) => {
                let m = self.marker();
                self.next();
                self.expect(sig!(Colon))?;
                (Case::Default, m.complete(self))
            }
            kw!(Case) => {
                let m = self.marker();
                self.next();
                let expr = self.parse_base_expression()?;
                self.expect(sig!(Colon))?;
                (Case::Case(expr), m.complete(self))
            }
            sig!(RBrace) => return Ok(None),
            _ => {
                return Err(self.fmt_err(
                    "switch must be followed by case or default clause",
                    self.peek(),
                ))
            }
        };
        while self.eat(Tk::Semi) {} // FIXME: Where is this hit?
        let mut stmts = vec![];

        loop {
            match self.peek_any()?.kind {
                kw!(Default) | kw!(Case) | sig!(RBrace) => break,
                _ => {
                    // After a case label and eating free semicolons, the only
                    // thing that can appear here is a closing brace, another
                    // case label, or a statement
                    let stmt = self.expect_one_statement(("switch clause", case_span), true)?;
                    while self.eat(Tk::Semi) {} // FIXME: Where is this hit?
                    stmts.push(stmt);
                }
            }
        }

        Ok(Some(CaseClause {
            case,
            case_span,
            stmts,
        }))
    }

    fn parse_one_stmt(&mut self) -> Result<Option<Statement>, ParseError> {
        while self.eat(Tk::Semi) {} // FIXME: Where is this hit?
        match self.peek() {
            Some(tok) => match tok.kind {
                sig!(RBrace) => Ok(None),
                sig!(RParen) => Ok(None),
                Tk::Sym(_)
                    if matches!(
                        self.peek2(),
                        Some(Token {
                            kind: sig!(Colon),
                            ..
                        })
                    ) =>
                {
                    let m = self.marker();
                    let label = self.expect_ident()?;
                    self.next();
                    Ok(Some(Statement {
                        kind: StatementKind::Label { name: label },
                        span: m.complete(self),
                    }))
                }
                kw!(If) => {
                    let if_m = self.marker();
                    let kw_span = self.next().unwrap().span;
                    self.expect(sig!(LParen))?;
                    let cond = self.parse_base_expression()?;
                    self.expect(sig!(RParen))?;

                    let then = self.parse_block_or_stmt(("if", kw_span))?;

                    let pot_else_m = self.marker();
                    let or_else = if self.eat(kw!(Else)) {
                        let else_kw_span = pot_else_m.complete(self);
                        Some(self.parse_block_or_stmt(("else", else_kw_span))?)
                    } else {
                        None
                    };

                    Ok(Some(Statement {
                        kind: StatementKind::IfStatement {
                            cond,
                            then,
                            or_else,
                        },
                        span: if_m.complete(self),
                    }))
                }
                kw!(For) => {
                    let for_m = self.marker();
                    let kw_span = self.next().unwrap().span;
                    self.expect(sig!(LParen))?;
                    let init = self.expect_one_statement(("for init", kw_span), true)?;
                    let cond = self.parse_base_expression()?;
                    self.expect(Tk::Semi)?;
                    let retry = self.expect_one_statement(("for retry", kw_span), false)?;
                    self.expect(sig!(RParen))?;

                    let run = self.parse_block_or_stmt(("for", kw_span))?;

                    Ok(Some(Statement {
                        kind: StatementKind::ForStatement {
                            init: Box::new(init),
                            cond,
                            retry: Box::new(retry),
                            run,
                        },
                        span: for_m.complete(self),
                    }))
                }
                kw!(Foreach) => {
                    let foreach_m = self.marker();
                    let foreach_tok = self.next().unwrap();
                    let kw_span = foreach_tok.span;
                    let fake_call = self.parse_base_expression()?;
                    // Pull apart the function call expr. This makes it easier to work with later.
                    let (ctx, name, args) = match fake_call {
                        Expr {
                            kind: ExprKind::FuncCallExpr { lhs, name, args },
                            span,
                            paren,
                        } => (lhs, name, args),
                        _ => {
                            return Err(self.fmt_err(
                                "expected function call after foreach",
                                Some(foreach_tok),
                            ))
                        }
                    };
                    let run = self.parse_block_or_stmt(("foreach", kw_span))?;

                    Ok(Some(Statement {
                        kind: StatementKind::ForeachStatement {
                            ctx,
                            name,
                            args,
                            run,
                        },
                        span: foreach_m.complete(self),
                    }))
                }
                kw!(While) => {
                    let while_m = self.marker();
                    let kw_span = self.next().unwrap().span;
                    self.expect(sig!(LParen))?;
                    let cond = self.parse_base_expression()?;
                    self.expect(sig!(RParen))?;
                    let run = self.parse_block_or_stmt(("while", kw_span))?;

                    Ok(Some(Statement {
                        kind: StatementKind::WhileStatement { cond, run },
                        span: while_m.complete(self),
                    }))
                }
                kw!(Do) => {
                    let do_m = self.marker();
                    let kw_span = self.next().unwrap().span;
                    let run = self.parse_block_or_stmt(("do", kw_span))?;
                    self.expect(kw!(Until))?;
                    self.expect(sig!(LParen))?;
                    let cond = self.parse_base_expression()?;
                    self.expect(sig!(RParen))?;

                    Ok(Some(Statement {
                        kind: StatementKind::DoStatement { cond, run },
                        span: do_m.complete(self),
                    }))
                }
                kw!(Switch) => {
                    let switch_m = self.marker();
                    self.next();
                    self.expect(sig!(LParen))?;
                    let scrutinee = self.parse_base_expression()?;
                    self.expect(sig!(RParen))?;
                    self.expect(sig!(LBrace))?;

                    let mut cases = vec![];

                    loop {
                        match self.parse_single_case() {
                            Ok(Some(case)) => {
                                cases.push(case);
                            }
                            Ok(None) => break,
                            Err(e) => {
                                self.errs.push(e);
                                // TODO
                                self.recover_to_semi();
                            }
                        }
                    }

                    self.expect(sig!(RBrace))?;

                    Ok(Some(Statement {
                        kind: StatementKind::SwitchStatement { scrutinee, cases },
                        span: switch_m.complete(self),
                    }))
                }
                kw!(Break) => {
                    let break_m = self.marker();
                    self.next();
                    Ok(Some(Statement {
                        kind: StatementKind::BreakStatement,
                        span: break_m.complete(self),
                    }))
                }
                kw!(Continue) => {
                    let cont_m = self.marker();
                    self.next();
                    Ok(Some(Statement {
                        kind: StatementKind::ContinueStatement,
                        span: cont_m.complete(self),
                    }))
                }
                kw!(Return) => {
                    let ret_m = self.marker();
                    self.next();
                    let expr = match self.peek_any()?.kind {
                        Tk::Semi => None,
                        _ => Some(self.parse_base_expression()?),
                    };
                    Ok(Some(Statement {
                        kind: StatementKind::ReturnStatement { expr },
                        span: ret_m.complete(self),
                    }))
                }
                _ => {
                    let expr_m = self.marker();
                    let lhs = self.parse_base_expression()?;
                    let stat = if self.eat(sig!(Eq)) {
                        let rhs = self.parse_base_expression()?;
                        Statement {
                            kind: StatementKind::Assignment { lhs, rhs },
                            span: expr_m.complete(self),
                        }
                    } else {
                        Statement {
                            kind: StatementKind::Expr { expr: lhs },
                            span: expr_m.complete(self),
                        }
                    };
                    Ok(Some(stat))
                }
            },
            None => Ok(None),
        }
    }

    pub fn parse_statements(&mut self) -> Vec<Statement> {
        let mut stmts = vec![];
        loop {
            match self.parse_one_stmt() {
                Ok(Some(stmt)) => {
                    if stmt_wants_semi(&stmt.kind) && !self.eat(Tk::Semi) {
                        self.errs
                            .push(self.fmt_err("Error, missing semicolon", self.peek().clone()));
                    }
                    while self.eat(Tk::Semi) {} // FIXME: Where is this hit?
                    stmts.push(stmt);
                }
                Ok(None) => break,
                Err(e) => {
                    self.errs.push(e);
                    self.recover_to_semi();
                }
            }
        }
        stmts
    }
}
