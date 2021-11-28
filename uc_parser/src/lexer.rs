use std::str::{self, FromStr};

use uc_files::{BytePos, FileId, Sources, Span};
use uc_name::Identifier;

pub use enums::{Keyword, Sigil};

mod enums;

#[derive(Clone, Debug)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    /// A block or EOL comment
    Comment,
    /// An unknown start of a token
    Error(u8),
    /// A token that hit EOF or an invalid character before being terminated
    Incomplete(IncompleteReason),
    /// The statement and item delimiter `;`
    Semi,
    /// The variant, arg, name delimiter `,`
    Comma,
    /// A sigil consisting of 1-3 characters
    Sig(Sigil),
    /// A keyword or non-keyword identifier
    Sym(Symbol),
    /// The `#`
    Directive,
    /// An integer, hex, or floating point literal
    Number(NumberSyntax),
    /// A string literal
    String,
    /// A name literal
    Name,
    /// The "name" part of an object literal Object'Package.ObjName'
    DotName,
    /// A bool literal
    Bool(bool),
}

#[derive(Clone, Copy, Debug)]
pub enum NumberLiteral {
    Int(i32),
    Float(f32),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum IncompleteReason {
    Eol,
    InvalidChar,
}

/// The sad state of identifiers in UnrealScript is that there
/// aren't any strong keywords -- every keyword can *probably*
/// be used in some identifier position. It's not recommended
/// to do that, however. The worst example of this is that there's
/// a class called `Input` in Engine, but `input` is a keyword
/// for variable declarations, so it's impossible to have a var
/// or local of type `Input`!
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Symbol {
    Kw(Keyword),
    Identifier,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NumberSyntax {
    Int,
    Float,
    Hex,
    Wild,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    bytes: Bytes<'a>,
    source: &'a Sources,
}

fn is_uc_whitespace(c: u8) -> bool {
    matches!(c, b' ' | b'\t' | b'\r' | b'\n')
}

impl<'a> Lexer<'a> {
    /// Lex an UnrealScript source file.
    pub fn new(source: &'a Sources, f_id: FileId) -> Self {
        let bytes_span = source.file_span(f_id);
        let bytes = source.lookup_bytes(bytes_span);
        Self {
            bytes: Bytes::new(bytes, bytes_span.start),
            source,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<Token> {
        let (c, pos) = loop {
            let pos = self.bytes.pos();
            let c = match self.bytes.next() {
                Some(c) => c,
                None => return None,
            };

            if is_uc_whitespace(c) {
                continue;
            }
            break (c, pos);
        };

        let c_ = self.bytes.peek();

        let kind = {
            use self::Sigil::*;
            use self::TokenKind::*;
            match (c, c_) {
                (b'+', Some(b'+')) => {
                    self.bytes.next();
                    Sig(AddAdd)
                }
                (b'+', Some(b'=')) => {
                    self.bytes.next();
                    Sig(AddAssign)
                }
                (b'-', Some(b'=')) => {
                    self.bytes.next();
                    Sig(SubAssign)
                }
                (b'-', Some(b'-')) => {
                    self.bytes.next();
                    Sig(SubSub)
                }
                (b'*', Some(b'=')) => {
                    self.bytes.next();
                    Sig(MulAssign)
                }
                (b'/', Some(b'=')) => {
                    self.bytes.next();
                    Sig(DivAssign)
                }
                (b'@', Some(b'=')) => {
                    self.bytes.next();
                    Sig(AtAssign)
                }
                (b'$', Some(b'=')) => {
                    self.bytes.next();
                    Sig(DollarAssign)
                }
                (b'<', Some(b'<')) => {
                    self.bytes.next();
                    Sig(LtLt)
                }
                (b'<', Some(b'=')) => {
                    self.bytes.next();
                    Sig(LtEq)
                }
                (b'>', Some(b'>')) => {
                    self.bytes.next();
                    if let Some(b'>') = self.bytes.peek() {
                        self.bytes.next();
                        Sig(GtGtGt)
                    } else {
                        Sig(GtGt)
                    }
                }
                (b'>', Some(b'=')) => {
                    self.bytes.next();
                    Sig(GtEq)
                }
                (b'|', Some(b'|')) => {
                    self.bytes.next();
                    Sig(OrOr)
                }
                (b'&', Some(b'&')) => {
                    self.bytes.next();
                    Sig(AndAnd)
                }
                (b'=', Some(b'=')) => {
                    self.bytes.next();
                    Sig(EqEq)
                }
                (b'!', Some(b'=')) => {
                    self.bytes.next();
                    Sig(BangEq)
                }
                (b'~', Some(b'=')) => {
                    self.bytes.next();
                    Sig(TildeEq)
                }
                (b'^', Some(b'^')) => {
                    self.bytes.next();
                    Sig(PowPow)
                }
                (b'*', Some(b'*')) => {
                    self.bytes.next();
                    Sig(MulMul)
                }
                (b'/', Some(b'/')) => return Some(self.parse_eol_comment(pos)),
                (b'/', Some(b'*')) => return Some(self.parse_block_comment(pos)),
                (b'0'..=b'9', _) | (b'.', Some(b'0'..=b'9')) => {
                    return Some(self.parse_number(pos))
                }
                (c, _) => match c {
                    b'(' => Sig(LParen),
                    b')' => Sig(RParen),
                    b'[' => Sig(LBrack),
                    b']' => Sig(RBrack),
                    b'{' => Sig(LBrace),
                    b'}' => Sig(RBrace),
                    b'.' => Sig(Dot),
                    b',' => Comma,
                    b':' => Sig(Colon),
                    b';' => Semi,
                    b'+' => Sig(Add),
                    b'-' => Sig(Sub),
                    b'*' => Sig(Mul),
                    b'/' => Sig(Div),
                    b'&' => Sig(And),
                    b'|' => Sig(Or),
                    b'@' => Sig(At),
                    b'$' => Sig(Dollar),
                    b'%' => Sig(Mod),
                    b'^' => Sig(Pow),
                    b'<' => Sig(Lt),
                    b'>' => Sig(Gt),
                    b'~' => Sig(Tilde),
                    b'!' => Sig(Bang),
                    b'=' => Sig(Eq),
                    b'?' => Sig(Tern),
                    b'#' => Directive,
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => return Some(self.parse_ident(pos)),
                    b'\'' => return Some(self.parse_name(pos)),
                    b'\"' => return Some(self.parse_string(pos)),
                    any => {
                        return Some(Token {
                            kind: TokenKind::Error(any),
                            span: Span {
                                start: pos,
                                end: self.bytes.pos(),
                            },
                        })
                    }
                },
            }
        };

        Some(Token {
            kind,
            span: Span {
                start: pos,
                end: self.bytes.pos(),
            },
        })
    }

    /// Eats the {...} from a defaultproperties block, or the <Tooltip=...>
    /// from a variable declaration. Returns Err(Incomplete) if incomplete.
    pub fn ignore_foreign_block(&mut self, opener: TokenKind) -> Result<(), Token> {
        let (recurser, closer) = match opener {
            TokenKind::Sig(Sigil::LBrace) => (Some(b'{'), Some(b'}')),
            TokenKind::Sig(Sigil::Lt) => (None, Some(b'>')),
            _ => panic!(
                "TokenKind {:?} is not a valid opener for foreign block",
                opener
            ),
        };

        let mut brace_count = 1;
        let first = self.bytes.pos();

        loop {
            if brace_count == 0 {
                break;
            }

            let next = if let Some(next) = self.bytes.next() {
                next
            } else {
                return Err(Token {
                    span: Span {
                        start: first,
                        end: self.bytes.pos(),
                    },
                    kind: TokenKind::Incomplete(IncompleteReason::Eol),
                });
            };

            if recurser.map(|c| c == next).unwrap_or(false) {
                brace_count += 1;
            }

            if closer.map(|c| c == next).unwrap_or(false) {
                brace_count -= 1;
            }
        }
        Ok(())
    }

    fn parse_ident(&mut self, first: BytePos) -> Token {
        while matches!(
            self.bytes.peek(),
            Some(b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9')
        ) {
            self.bytes.next();
        }

        let span = Span {
            start: first,
            end: self.bytes.pos(),
        };
        let text = self
            .source
            .lookup_str(span)
            .expect("just checked for ASCII subset");

        let kind = if text.eq_ignore_ascii_case("true") {
            TokenKind::Bool(true)
        } else if text.eq_ignore_ascii_case("false") {
            TokenKind::Bool(false)
        } else {
            match Keyword::from_str(text) {
                Ok(kw) => TokenKind::Sym(Symbol::Kw(kw)),
                Err(_) => TokenKind::Sym(Symbol::Identifier),
            }
        };

        Token { span, kind }
    }

    pub fn extract_ident(&self, token: &Token) -> Identifier {
        assert_eq!(token.kind, TokenKind::Sym(Symbol::Identifier));
        let text = self
            .source
            .lookup_str(token.span)
            .expect("this is a valid identifier");
        Identifier::from_str(text).unwrap()
    }

    pub fn extract_string(&self, token: &Token) -> Box<str> {
        assert_eq!(token.kind, TokenKind::String);
        let bytes = self.source.lookup_bytes(token.span);
        let string = String::from_utf8_lossy(bytes);
        string.into_owned().into_boxed_str()
    }

    pub fn extract_number(&self, token: &Token) -> Result<NumberLiteral, String> {
        let text = self.source.lookup_str(token.span).expect("valid num token");
        match token.kind {
            TokenKind::Number(NumberSyntax::Int) => Ok(NumberLiteral::Int(
                text.parse::<i32>().map_err(|e| e.to_string())?,
            )),
            TokenKind::Number(NumberSyntax::Hex) => {
                if text.len() < 3 || (&text[0..2] != "0x" && &text[0..2] != "0X") {
                    return Err("invalid hex number".to_owned());
                }
                Ok(NumberLiteral::Int(
                    // Most important cast right here
                    u32::from_str_radix(&text[2..], 16).map_err(|e| e.to_string())? as i32,
                ))
            }
            TokenKind::Number(NumberSyntax::Float) => Ok(NumberLiteral::Float(
                text.parse::<f32>().map_err(|e| e.to_string())?,
            )),
            TokenKind::Number(NumberSyntax::Wild) => Err("unrecognized number format".to_owned()),
            _ => panic!("not a number: {:?}", token),
        }
    }

    fn parse_eol_comment(&mut self, first: BytePos) -> Token {
        while !matches!(self.bytes.peek(), Some(b'\r' | b'\n') | None) {
            self.bytes.next();
        }

        let span = Span {
            start: first,
            end: self.bytes.pos(),
        };
        Token {
            kind: TokenKind::Comment,
            span,
        }
    }

    fn parse_block_comment(&mut self, first: BytePos) -> Token {
        let mut level = 1;
        loop {
            match self.bytes.next() {
                Some(b'*') => match self.bytes.peek() {
                    Some(b'/') => {
                        self.bytes.next();
                        level -= 1;
                        if level == 0 {
                            break;
                        }
                    }
                    Some(_) => continue,
                    None => {
                        return Token {
                            kind: TokenKind::Incomplete(IncompleteReason::Eol),
                            span: Span {
                                start: first,
                                end: self.bytes.pos(),
                            },
                        }
                    }
                },
                Some(b'/') => match self.bytes.peek() {
                    Some(b'*') => {
                        self.bytes.next();
                        level += 1;
                    }
                    Some(_) => continue,
                    None => {
                        return Token {
                            kind: TokenKind::Incomplete(IncompleteReason::Eol),
                            span: Span {
                                start: first,
                                end: self.bytes.pos(),
                            },
                        }
                    }
                },
                Some(_) => continue,
                None => {
                    return Token {
                        kind: TokenKind::Incomplete(IncompleteReason::Eol),
                        span: Span {
                            start: first,
                            end: self.bytes.pos(),
                        },
                    }
                }
            }
        }

        Token {
            kind: TokenKind::Comment,
            span: Span {
                start: first,
                end: self.bytes.pos(),
            },
        }
    }

    fn parse_number(&mut self, first: BytePos) -> Token {
        let mut hex = false;
        let mut float = false;
        while let Some(c @ (b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' | b'.' | b'x' | b'X')) =
            self.bytes.peek()
        {
            float |= c == b'.';
            hex |= c == b'x' || c == b'X';
            self.bytes.next();
        }

        let span = Span {
            start: first,
            end: self.bytes.pos(),
        };
        let num_kind = match (float, hex) {
            (true, true) => NumberSyntax::Wild,
            (true, false) => NumberSyntax::Float,
            (false, true) => NumberSyntax::Hex,
            (false, false) => NumberSyntax::Int,
        };
        Token {
            kind: TokenKind::Number(num_kind),
            span,
        }
    }

    fn parse_name(&mut self, first: BytePos) -> Token {
        let mut seen_dot = false;
        while let Some(x @ (b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'-' | b'.' | b' ')) =
            self.bytes.peek()
        {
            seen_dot |= x == b'.';
            self.bytes.next();
        }

        let kind = match self.bytes.peek() {
            Some(b'\'') => {
                self.bytes.next();
                if seen_dot {
                    TokenKind::DotName
                } else {
                    TokenKind::Name
                }
            }
            Some(_) => TokenKind::Incomplete(IncompleteReason::InvalidChar),
            None => TokenKind::Incomplete(IncompleteReason::Eol),
        };
        Token {
            kind,
            span: Span {
                start: first,
                end: self.bytes.pos(),
            },
        }
    }

    fn parse_string(&mut self, first: BytePos) -> Token {
        loop {
            match self.bytes.peek() {
                Some(b'"') => {
                    self.bytes.next();
                    return Token {
                        kind: TokenKind::String,
                        span: Span {
                            start: first,
                            end: self.bytes.pos(),
                        },
                    };
                }
                Some(b'\\') => {
                    self.bytes.next();
                    match self.bytes.peek() {
                        Some(b'\r' | b'\n') | None => {
                            return Token {
                                kind: TokenKind::Incomplete(IncompleteReason::Eol),
                                span: Span {
                                    start: first,
                                    end: self.bytes.pos(),
                                },
                            }
                        }
                        _ => {
                            self.bytes.next();
                        }
                    }
                }
                Some(_) => {
                    self.bytes.next();
                }
                None => {
                    return Token {
                        kind: TokenKind::Incomplete(IncompleteReason::Eol),
                        span: Span {
                            start: first,
                            end: self.bytes.pos(),
                        },
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Bytes<'a> {
    text: &'a [u8],
    bytes: std::slice::Iter<'a, u8>,
    start_offset: BytePos,
}

impl<'a> Bytes<'a> {
    fn new(text: &'a [u8], start_offset: BytePos) -> Self {
        Self {
            text,
            bytes: text.iter(),
            start_offset,
        }
    }

    fn pos(&self) -> BytePos {
        BytePos::new(
            self.text.len() as u32 - self.bytes.as_slice().len() as u32 + self.start_offset.get(),
        )
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.clone().next().copied()
    }
}

impl<'a> Iterator for Bytes<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        self.bytes.next().copied()
    }
}
