//! The UnrealScript parser, parsing lexemes to an unresolved
//! High level Intermediate Represenation.
use std::str::FromStr;

mod expr;
mod item;
mod modifiers;
mod stmt;

use uc_ast::{Hir, Ty};
use uc_files::Span;
use uc_name::Identifier;

use crate::{
    lexer::{Lexer, NumberLiteral, Symbol, Token, TokenKind as Tk},
    parser::item::TopLevelItem,
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
    errs: Vec<ParseError>,
    last_end: Option<u32>,
}

// pretty much stolen from rust-analyzer
#[derive(Debug)]
struct SpanMarker {
    pos: Option<u32>,
}

impl SpanMarker {
    fn complete(self, parser: &Parser<'_>) -> Span {
        Span {
            start: self.pos.unwrap(),
            end: parser.last_end.unwrap(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    err: Box<ParseErrorInner>,
}

#[derive(Clone, Debug)]
pub struct ParseErrorInner {
    /// Message.
    error_message: &'static str,
    /// Token that caused us to realize there's an error.
    bad_token: Option<Token>,
    /// Reason this token is bad, if any.
    ctx_token: Option<(String, Span)>,
    /// Expected kind
    expected_token: Option<Tk>,
}

impl<'a> Parser<'a> {
    fn new(lex: Lexer<'a>) -> Self {
        Self {
            lex,
            errs: vec![],
            last_end: None,
        }
    }

    fn next(&mut self) -> Option<Token> {
        loop {
            match self.lex.next()? {
                Token {
                    kind: Tk::Comment, ..
                } => {}
                x => {
                    self.last_end = Some(x.span.end);
                    return Some(x);
                }
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

    fn marker(&self) -> SpanMarker {
        SpanMarker {
            pos: self.peek().map(|t| t.span.start),
        }
    }

    fn peek_any(&self) -> Result<Token, ParseError> {
        self.peek()
            .ok_or_else(|| self.fmt_err("Unexpected end of file", None))
    }

    fn next_any(&mut self) -> Result<Token, ParseError> {
        self.next()
            .ok_or_else(|| self.fmt_err("Unexpected end of file", None))
    }

    fn sym_to_ident(&self, tok: &Token) -> Identifier {
        match tok.kind {
            Tk::Sym(Symbol::Identifier) => self.lex.extract_ident(tok),
            Tk::Sym(Symbol::Kw(k)) => Identifier::from_str(k.as_ref()).unwrap(),
            _ => panic!("{:?} not a symbol", tok),
        }
    }

    fn expect(&mut self, kind: Tk) -> Result<Token, ParseError> {
        let tok = self.next_any()?;
        if tok.kind == kind {
            Ok(tok)
        } else {
            let mut err = self.fmt_err("Unexpected token", Some(tok));
            err.err.expected_token = Some(kind);
            Err(err)
        }
    }

    fn expect_ident(&mut self) -> Result<Identifier, ParseError> {
        let tok = self.next_any()?;
        if let Tk::Sym(_) = tok.kind {
            Ok(self.sym_to_ident(&tok))
        } else {
            Err(self.fmt_err("Unexpected token, expected identifier", Some(tok)))
        }
    }

    fn expect_nonnegative_integer(&mut self) -> Result<i32, ParseError> {
        let tok = self.next_any()?;
        if let Tk::Number(_) = tok.kind {
            let num = self
                .lex
                .extract_number(&tok)
                .map_err(|_| self.fmt_err("Malformed integer", Some(tok.clone())))?;
            match num {
                NumberLiteral::Int(i @ 0..) => Ok(i),
                NumberLiteral::Int(_) => Err(self.fmt_err("integer is negative", Some(tok))),
                NumberLiteral::Float(_) => {
                    Err(self.fmt_err("expected int but got float", Some(tok)))
                }
            }
        } else {
            Err(self.fmt_err("Expected number", Some(tok)))
        }
    }

    fn fmt_err(&self, msg: &'static str, token: Option<Token>) -> ParseError {
        ParseError {
            err: Box::new(ParseErrorInner {
                error_message: msg,
                bad_token: token,
                ctx_token: None,
                expected_token: None,
            }),
        }
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

    fn recover_to_semi(&mut self) {
        while let Some(Token { kind, .. }) = self.next() {
            if kind == Tk::Semi {
                break;
            }
        }
    }

    fn parse_maybe_qualified_path(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut parts = vec![self.expect_ident()?];
        loop {
            let tok = self.peek_any()?;
            match tok.kind {
                sig!(Dot) => {
                    self.next_any()?;
                }
                _ => break,
            }
            parts.push(self.expect_ident()?);
        }

        Ok(parts)
    }

    fn parse_angle_type(&mut self) -> Result<Vec<Identifier>, ParseError> {
        self.expect(sig!(Lt))?;
        let parts = self.parse_maybe_qualified_path()?;
        self.expect(sig!(Gt))?;
        Ok(parts)
    }

    fn parse_ty(&mut self, first_tok: Option<Token>) -> Result<Ty, ParseError> {
        let ty_tok = match first_tok {
            Some(t) => t,
            _ => self.next_any()?,
        };

        match &ty_tok.kind {
            kw!(Array) => {
                self.expect(sig!(Lt))?;

                {
                    // A grand total of 3 files have `// array<const native transient pointer>`,
                    // which we just eat here:
                    self.eat(kw!(Const));
                    self.eat(kw!(Native));
                    self.eat(kw!(Transient));
                }

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
            _ => Err(self.fmt_err("expected type after modifiers", Some(ty_tok))),
        }
    }

    fn ignore_foreign_block(&mut self, opener: Tk) -> Result<(), ParseError> {
        self.lex
            .ignore_foreign_block(opener)
            .map_err(|e| self.fmt_err("Unclosed foreign block", Some(e)))
    }
}

pub fn parse(lex: Lexer) -> (Hir, Vec<ParseError>) {
    let mut parser = Parser::new(lex);
    let header = parser.parse_class_def().unwrap();
    let items = parser.parse_items();

    let mut structs = vec![];
    let mut enums = vec![];
    let mut consts = vec![];
    let mut vars = vec![];
    let mut states = vec![];
    let mut funcs = vec![];

    for i in items {
        match i {
            TopLevelItem::Const(c) => consts.push(c),
            TopLevelItem::Var(v) => vars.push(v),
            TopLevelItem::Struct(s) => structs.push(s),
            TopLevelItem::Enum(e) => enums.push(e),
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
            states,
            funcs,
        },
        parser.errs,
    )
}
