//! The UnrealScript parser, parsing lexemes to an unresolved
//! High level Intermediate Represenation.
use std::str::FromStr;

mod expr;
mod item;
mod modifiers;
mod stmt;

use uc_def::{Hir, Identifier, Ty};

use crate::{
    lexer::{Lexer, Symbol, Token, TokenKind as Tk},
    parser::item::TopLevelItem,
    NumberLiteral,
};

#[macro_export]
macro_rules! kw {
    ($i:ident) => {
        crate::lexer::TokenKind::Sym(crate::lexer::Symbol::Kw(crate::lexer::Keyword::$i))
    };
}

#[macro_export]
macro_rules! sig {
    ($i:ident) => {
        crate::lexer::TokenKind::Sig(crate::lexer::Sigil::$i)
    };
}

#[derive(Clone, Debug)]
struct Parser<'a> {
    lex: Lexer<'a>,
    errs: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lex: Lexer<'a>) -> Self {
        Self { lex, errs: vec![] }
    }

    fn next(&mut self) -> Option<Token> {
        loop {
            match self.lex.next()? {
                Token {
                    kind: Tk::Comment, ..
                } => {}
                x => return Some(x),
            }
        }
    }

    fn peek(&self) -> Option<Token> {
        self.clone().next()
    }

    /// This pretty much only exists to support `simulated state` and labels
    fn peek2(&self) -> Option<Token> {
        let mut s = self.clone();
        s.next();
        s.next()
    }

    fn peek_any(&self) -> Result<Token, String> {
        self.peek().ok_or_else(|| "eof".to_owned())
    }

    fn next_any(&mut self) -> Result<Token, String> {
        self.next().ok_or_else(|| "eof".to_owned())
    }

    fn sym_to_ident(&self, tok: &Token) -> Identifier {
        match tok.kind {
            Tk::Sym(Symbol::Identifier) => self.lex.extract_ident(tok),
            Tk::Sym(Symbol::Kw(k)) => Identifier::from_str(k.as_ref()).unwrap(),
            _ => panic!("{:?} not a symbol", tok),
        }
    }

    fn expect(&mut self, kind: Tk) -> Result<Token, String> {
        let tok = self.next().ok_or_else(|| "eof".to_owned())?;
        if tok.kind == kind {
            Ok(tok)
        } else if tok.kind == Tk::Sym(Symbol::Identifier) {
            Err(format!(
                "expected {:?}, got {:?} ({})",
                kind,
                tok,
                self.lex.extract_ident(&tok)
            ))
        } else {
            Err(format!("expected {:?}, got {:?}", kind, tok))
        }
    }

    fn expect_ident(&mut self) -> Result<Identifier, String> {
        let tok = self.next().ok_or_else(|| "eof".to_owned())?;
        if let Tk::Sym(_) = tok.kind {
            Ok(self.sym_to_ident(&tok))
        } else {
            Err(format!("expected symbol, got {:?}", tok))
        }
    }

    fn expect_number(&mut self) -> Result<NumberLiteral, String> {
        let tok = self.next().ok_or_else(|| "eof".to_owned())?;
        if let Tk::Number(_) = tok.kind {
            self.lex.extract_number(&tok)
        } else {
            Err(format!("expected number, got {:?}", tok.kind))
        }
    }

    fn fmt_unexpected(&self, token: &Token) -> String {
        format!("unexpected token: {:?}", token)
    }

    pub fn eat(&mut self, kind: Tk) -> bool {
        match self.peek() {
            None => false,
            Some(tok) => {
                if tok.kind == kind {
                    self.next();
                    true
                } else {
                    false
                }
            }
        }
    }

    pub fn eat_symbol(&mut self) -> bool {
        match self.peek() {
            None => false,
            Some(tok) => {
                if let Tk::Sym(_) = tok.kind {
                    self.next();
                    true
                } else {
                    false
                }
            }
        }
    }

    fn recover_to_semi(&mut self) {
        while let Some(Token { kind, .. }) = self.next() {
            if kind == Tk::Semi {
                break;
            }
        }
    }

    fn parse_parts_until(&mut self, end_token: Tk) -> Result<Vec<Identifier>, String> {
        let mut parts = vec![];
        let mut dot = false;
        loop {
            let mut tok = self.next_any()?;
            match tok.kind {
                tk if tk == end_token => break,
                sig!(Dot) if dot => {
                    tok = self.next_any()?;
                }
                _ => {}
            }
            if let Tk::Sym(_) = tok.kind {
                parts.push(self.sym_to_ident(&tok));
                dot = true;
            } else {
                return Err(format!(
                    "expected type name or type path part name, got {:?}",
                    tok
                ));
            }
        }

        Ok(parts)
    }

    fn parse_angle_type(&mut self) -> Result<Vec<Identifier>, String> {
        self.expect(sig!(Lt))?;

        self.parse_parts_until(sig!(Gt))
    }

    fn parse_ty(&mut self, first_tok: Option<Token>) -> Result<Ty<Identifier>, String> {
        let ty_tok = match first_tok {
            Some(t) => t,
            _ => self.next_any()?,
        };

        match &ty_tok.kind {
            kw!(Array) => {
                self.expect(sig!(Lt))?;
                let ty = self.parse_ty(None)?;
                self.expect(sig!(Gt))?;
                Ok(Ty::Array(Box::new(ty)))
            }
            kw!(Class) => {
                let class = if self.eat(sig!(Lt)) {
                    let c = self.expect_ident()?;
                    self.expect(sig!(Gt))?;
                    Some(c)
                } else {
                    None
                };
                Ok(Ty::Class(class))
            }
            kw!(Delegate) => Ok(Ty::Delegate(self.parse_angle_type()?)),
            kw!(Map) => {
                self.expect(sig!(LBrace))?;
                self.ignore_foreign_block(sig!(LBrace))?;
                Ok(Ty::Simple(Identifier::from_str("Map").unwrap()))
            }
            Tk::Sym(_) => {
                if self.eat(sig!(Dot)) {
                    let mut parts = vec![self.sym_to_ident(&ty_tok)];
                    loop {
                        parts.push(self.expect_ident()?);
                        match self.peek() {
                            Some(Token {
                                kind: sig!(Dot), ..
                            }) => {
                                self.next();
                                continue;
                            }
                            _ => break,
                        }
                    }
                    Ok(Ty::Qualified(parts))
                } else {
                    Ok(Ty::Simple(self.sym_to_ident(&ty_tok)))
                }
            }
            _ => Err(format!("expected type after modifiers, got {:?}", ty_tok)),
        }
    }

    fn ignore_foreign_block(&mut self, opener: Tk) -> Result<(), String> {
        self.lex
            .ignore_foreign_block(opener)
            .map_err(|e| format!("{:?}", e))
    }
}

pub fn parse(lex: Lexer) -> (Hir<Identifier>, Vec<String>) {
    let mut parser = Parser::new(lex);
    let header = parser.parse_class_def().unwrap();
    let items = parser.parse_items();

    let mut structs = vec![];
    let mut enums = vec![];
    let mut consts = vec![];
    let mut vars = vec![];
    let mut dels = vec![];
    let mut states = vec![];
    let mut funcs = vec![];

    for i in items {
        match i {
            TopLevelItem::Const(c) => consts.push(c),
            TopLevelItem::Var(v) => vars.push(v),
            TopLevelItem::Struct(s) => structs.push(s),
            TopLevelItem::Enum(e) => enums.push(e),
            TopLevelItem::Delegate(d) => dels.push(d),
            TopLevelItem::State(s) => states.push(s),
            TopLevelItem::Func(f) => funcs.push(f),
        }
    }

    (
        Hir {
            header,
            structs,
            enums,
            consts,
            vars,
            dels,
            states,
            funcs,
        },
        parser.errs,
    )
}
