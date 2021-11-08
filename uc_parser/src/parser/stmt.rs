use uc_def::{Block, Case, CaseClause, Identifier, Statement};

use crate::{
    kw,
    lexer::{Token, TokenKind as Tk},
    sig,
};

use super::Parser;

/// Does this statement usually want to be terminated by a semicolon?
fn stmt_wants_semi<I>(stmt: &Statement<I>) -> bool {
    match stmt {
        Statement::IfStatement { .. } => false,
        Statement::ForStatement { .. } => false,
        Statement::ForeachStatement { .. } => false,
        Statement::WhileStatement { .. } => false,
        Statement::DoStatement { .. } => true,
        Statement::SwitchStatement { .. } => false,
        Statement::BreakStatement => true,
        Statement::ContinueStatement => true,
        Statement::ReturnStatement { .. } => true,
        Statement::Label(_) => false,
        Statement::Assignment { .. } => true,
        Statement::Expr { .. } => true,
    }
}

impl Parser<'_> {
    fn parse_block_or_stmt(&mut self, ctx: &'static str) -> Result<Block<Identifier>, String> {
        if self.eat(sig!(LBrace)) {
            let stmts = self.parse_statements();
            self.expect(sig!(RBrace))?;
            Ok(Block { stmts })
        } else {
            let stmt = self.expect_one_statement(ctx, true)?;
            Ok(Block { stmts: vec![stmt] })
        }
    }

    fn expect_one_statement(
        &mut self,
        ctx: &'static str,
        expect_semi: bool,
    ) -> Result<Statement<Identifier>, String> {
        let stmt = self
            .parse_one_stmt()?
            .ok_or_else(|| format!("missing statement after {}", ctx))?;
        if expect_semi && stmt_wants_semi(&stmt) && !self.eat(Tk::Semi) {
            return Err(format!(
                "missing semicolon after statement after {} (next is {:?}, previously parsed {:?})",
                ctx,
                self.peek(),
                stmt
            ));
        }
        Ok(stmt)
    }

    fn parse_single_case(&mut self) -> Result<Option<CaseClause<Identifier>>, String> {
        let case = match self.peek_any()?.kind {
            kw!(Default) => {
                self.next();
                self.expect(sig!(Colon))?;
                Case::Default
            }
            kw!(Case) => {
                self.next();
                let expr = self.parse_base_expression()?;
                self.expect(sig!(Colon))?;
                Case::Case(expr)
            }
            sig!(RBrace) => return Ok(None),
            _ => return Err("switch must be followed by case or default clause".to_owned()),
        };
        while self.eat(Tk::Semi) {}
        let mut statements = vec![];

        loop {
            match self.peek_any()?.kind {
                kw!(Default) | kw!(Case) | sig!(RBrace) => break,
                _ => {
                    // After a case label and eating free semicolons, the only
                    // thing that can appear here is a closing brace, another
                    // case label, or a statement
                    let stmt = self.expect_one_statement("switch clause", true)?;
                    while self.eat(Tk::Semi) {}
                    statements.push(stmt);
                }
            }
        }

        Ok(Some(CaseClause { case, statements }))
    }

    fn parse_one_stmt(&mut self) -> Result<Option<Statement<Identifier>>, String> {
        while self.eat(Tk::Semi) {}
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
                    let label = self.expect_ident()?;
                    self.next();
                    Ok(Some(Statement::Label(label)))
                }
                kw!(If) => {
                    self.next();
                    self.expect(sig!(LParen))?;
                    let cond = self.parse_base_expression()?;
                    self.expect(sig!(RParen))?;

                    let then = self.parse_block_or_stmt("if")?;

                    let or_else = if self.eat(kw!(Else)) {
                        Some(self.parse_block_or_stmt("else")?)
                    } else {
                        None
                    };

                    Ok(Some(Statement::IfStatement {
                        cond,
                        then,
                        or_else,
                    }))
                }
                kw!(For) => {
                    self.next();
                    self.expect(sig!(LParen))?;
                    let init = self.expect_one_statement("for init", true)?;
                    let cond = self.parse_base_expression()?;
                    self.expect(Tk::Semi)?;
                    let retry = self.expect_one_statement("for retry", false)?;
                    self.expect(sig!(RParen))?;

                    let run = if self.eat(Tk::Semi) {
                        None
                    } else {
                        Some(self.parse_block_or_stmt("for")?)
                    };

                    Ok(Some(Statement::ForStatement {
                        init: Box::new(init),
                        cond,
                        retry: Box::new(retry),
                        run,
                    }))
                }
                kw!(Foreach) => {
                    self.next();
                    let source = self.parse_base_expression()?;
                    let run = self.parse_block_or_stmt("foreach")?;

                    Ok(Some(Statement::ForeachStatement { source, run }))
                }
                kw!(While) => {
                    self.next();
                    self.expect(sig!(LParen))?;
                    let cond = self.parse_base_expression()?;
                    self.expect(sig!(RParen))?;
                    let run = self.parse_block_or_stmt("while")?;

                    Ok(Some(Statement::WhileStatement { cond, run }))
                }
                kw!(Do) => {
                    self.next();
                    self.expect(sig!(LBrace))?;
                    let stmts = self.parse_statements();
                    self.expect(sig!(RBrace))?;
                    self.expect(kw!(Until))?;
                    self.expect(sig!(LParen))?;
                    let cond = self.parse_base_expression()?;
                    self.expect(sig!(RParen))?;

                    Ok(Some(Statement::DoStatement { cond, run: stmts }))
                }
                kw!(Switch) => {
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

                    Ok(Some(Statement::SwitchStatement { scrutinee, cases }))
                }
                kw!(Break) => {
                    self.next();
                    Ok(Some(Statement::BreakStatement))
                }
                kw!(Continue) => {
                    self.next();
                    Ok(Some(Statement::ContinueStatement))
                }
                kw!(Return) => {
                    self.next();
                    let expr = match self.peek_any()?.kind {
                        Tk::Semi => None,
                        _ => Some(self.parse_base_expression()?),
                    };
                    Ok(Some(Statement::ReturnStatement { expr }))
                }
                _ => {
                    let lhs = self.parse_base_expression()?;
                    let stat = if self.eat(sig!(Eq)) {
                        let rhs = self.parse_base_expression()?;
                        Statement::Assignment { lhs, rhs }
                    } else {
                        Statement::Expr { expr: lhs }
                    };
                    Ok(Some(stat))
                }
            },
            None => Ok(None),
        }
    }

    pub fn parse_statements(&mut self) -> Vec<Statement<Identifier>> {
        let mut stmts = vec![];
        loop {
            match self.parse_one_stmt() {
                Ok(Some(stmt)) => {
                    if stmt_wants_semi(&stmt) && !self.eat(Tk::Semi) {
                        self.errs
                            .push(format!("Error, missing semicolon, got {:?}", self.peek()));
                    }
                    while self.eat(Tk::Semi) {}
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
