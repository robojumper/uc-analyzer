use uc_def::{BlockOrStatement, Expr, Identifier, Statement};

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
        Statement::DoStatement { .. } => false,
        Statement::SwitchStatement { .. } => false,
        Statement::BreakStatement => true,
        Statement::ContinueStatement => true,
        Statement::GotoStatement => true,
        Statement::ReturnStatement { .. } => true,
        Statement::Label(_) => false,
        Statement::Expression(_) => true,
    }
}

impl Parser<'_> {
    fn parse_block_or_stmt(
        &mut self,
        ctx: &'static str,
    ) -> Result<BlockOrStatement<Identifier>, String> {
        if self.eat(sig!(LBrace)) {
            let stmts = self.parse_statements();
            self.expect(sig!(RBrace))?;
            Ok(BlockOrStatement::Block(stmts))
        } else {
            Ok(BlockOrStatement::Statement(Box::new(
                self.expect_one_statement(ctx, true)?,
            )))
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

    fn parse_one_stmt(&mut self) -> Result<Option<Statement<Identifier>>, String> {
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
                    Err("label".to_owned())
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

                    let run = self.parse_block_or_stmt("for")?;

                    Ok(Some(Statement::ForStatement {
                        init: Box::new(init),
                        cond,
                        retry: Box::new(retry),
                        run,
                    }))
                }
                kw!(Foreach) => Err("foreach".to_owned()),
                kw!(While) => {
                    self.next();
                    self.expect(sig!(LParen))?;
                    let cond = self.parse_base_expression()?;
                    self.expect(sig!(RParen))?;
                    let run = self.parse_block_or_stmt("while")?;

                    Ok(Some(Statement::WhileStatement { cond, run }))
                }
                kw!(Do) => Err("do".to_owned()),
                kw!(Switch) => Err("switch".to_owned()),
                kw!(Break) => Err("break".to_owned()),
                kw!(Continue) => Err("continue".to_owned()),
                kw!(Goto) => Err("goto".to_owned()),
                kw!(Return) => {
                    self.next();
                    let expr = self.parse_base_expression()?;
                    Ok(Some(Statement::ReturnStatement { expr }))
                }
                _ => {
                    let lhs = self.parse_base_expression()?;
                    let expr = if self.eat(sig!(Eq)) {
                        let rhs = self.parse_base_expression()?;
                        Expr::AssignmentExpr { lhs, rhs }
                    } else {
                        Expr::BaseExpr { expr: lhs }
                    };
                    Ok(Some(Statement::Expression(expr)))
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
                        self.errs.push("Error, missing semicolon".to_owned());
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
