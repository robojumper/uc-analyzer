use std::{collections::VecDeque, str::FromStr};

use crate::Identifier;

pub struct SpanExp {
    pub exp_start: usize,
    pub exp_end: usize,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub span: SpanExp,
    pub kind: TokenKind,
}

#[derive(Clone, Debug)]
pub enum TokenKind {
    /// A block or EOL comment
    Comment,
    /// A (struct)defaultproperties or (struct)cpptext block
    ForeignBlock(Keyword, Option<SpanExp>),
    /// An unknown start of a token
    Error(char),
    /// A token that hit EOF or an invalid character before being terminated
    Incomplete(Box<TokenKind>, IncompleteReason),
    /// An opening delimiter
    Open(Delim),
    /// A closing delimiter
    Close(Delim),
    /// The statement delimiter `;`
    Semi,
    /// A sigil consisting of 1-3 characters
    Sig(Sigil),
    /// A keyword
    Kw(Keyword),
    /// An identifier
    Identifier(Identifier),
    /// An integer, hex, or floating point literal
    Number(NumberSyntax),
    /// A string literal
    String(String),
    /// A name literal
    Name(String),
}

#[derive(Copy, Clone, Debug)]
pub enum IncompleteReason {
    Eol,
    InvalidChar,
}

#[derive(Copy, Clone, Debug)]
pub enum Delim {
    LBrack,
    RBrack,
    LParen,
    RParen,
    LBrace,
    RBrace,
}

#[derive(Copy, Clone, Debug)]
pub enum Sigil {
    Add,
    AddAdd,
    AddAssign,
    And,
    AndAnd,
    At,
    AtAssign,
    Bang,
    BangEq,
    Colon,
    Comma,
    Div,
    DivAssign,
    Dollar,
    DollarAssign,
    Dot,
    Eq,
    EqEq,
    Gt,
    GtEq,
    GtGt,
    GtGtGt,
    Lt,
    LtEq,
    LtLt,
    Mod,
    Mul,
    MulAssign,
    Or,
    OrOr,
    Pow,
    Tern,
    Sub,
    SubAssign,
    SubSub,
    Tilde,
    TildeEq,
}

#[derive(Copy, Clone, Debug, strum::EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum Keyword {
    Abstract,
    Class,
    Config,
    CppText,
    DefaultProperties,
    DependsOn,
    EditConst,
    EditorOnly,
    Enum,
    Event,
    Extends,
    Localized,
    Native,
    Placeable,
    Struct,
    StructCppText,
    StructDefaultProperties,
    Transient,
    Var,
}

#[derive(Copy, Clone, Debug)]
pub enum NumberSyntax {
    Int,
    Float,
    Hex,
    Wild,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: Source<'a>,
    peeked: VecDeque<Token>,
}

fn is_uc_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\r' | '\n')
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            source: Source::new(text),
            peeked: VecDeque::new(),
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<Token> {
        if let Some(token) = self.peeked.pop_front() {
            return Some(token);
        }

        let (c, pos) = loop {
            let pos = self.source.pos();
            let c = match self.source.next() {
                Some(c) => c,
                None => return None,
            };

            if is_uc_whitespace(c) {
                continue;
            }
            break (c, pos);
        };

        let c_ = self.source.peek();

        let kind = {
            use self::Delim::*;
            use self::Sigil::*;
            use self::TokenKind::*;
            match (c, c_) {
                ('+', Some('+')) => {
                    self.source.next();
                    Sig(AddAdd)
                }
                ('+', Some('=')) => {
                    self.source.next();
                    Sig(AddAssign)
                }
                ('-', Some('=')) => {
                    self.source.next();
                    Sig(SubAssign)
                }
                ('-', Some('-')) => {
                    self.source.next();
                    Sig(SubSub)
                }
                ('*', Some('=')) => {
                    self.source.next();
                    Sig(MulAssign)
                }
                ('/', Some('=')) => {
                    self.source.next();
                    Sig(DivAssign)
                }
                ('@', Some('=')) => {
                    self.source.next();
                    Sig(AtAssign)
                }
                ('$', Some('=')) => {
                    self.source.next();
                    Sig(DollarAssign)
                }
                ('<', Some('<')) => {
                    self.source.next();
                    Sig(LtLt)
                }
                ('<', Some('=')) => {
                    self.source.next();
                    Sig(LtEq)
                }
                ('>', Some('>')) => {
                    self.source.next();
                    if let Some('>') = self.source.peek() {
                        self.source.next();
                        Sig(GtGtGt)
                    } else {
                        Sig(GtGt)
                    }
                }
                ('>', Some('=')) => {
                    self.source.next();
                    Sig(GtEq)
                }
                ('|', Some('|')) => {
                    self.source.next();
                    Sig(OrOr)
                }
                ('&', Some('&')) => {
                    self.source.next();
                    Sig(AndAnd)
                }
                ('=', Some('=')) => {
                    self.source.next();
                    Sig(EqEq)
                }
                ('!', Some('=')) => {
                    self.source.next();
                    Sig(BangEq)
                }
                ('~', Some('=')) => {
                    self.source.next();
                    Sig(TildeEq)
                }
                ('/', Some('/')) => return Some(self.parse_eol_comment(pos)),
                ('/', Some('*')) => return Some(self.parse_block_comment(pos)),
                ('0'..='9', _) | ('+' | '-', Some('0'..='9')) => {
                    return Some(self.parse_number(pos))
                }
                (c, _) => match c {
                    '(' => Open(LParen),
                    ')' => Open(RParen),
                    '[' => Open(LBrack),
                    ']' => Open(RBrack),
                    '{' => Open(LBrace),
                    '}' => Open(RBrace),
                    '.' => Sig(Dot),
                    ',' => Sig(Comma),
                    ':' => Sig(Colon),
                    ';' => Semi,
                    '+' => Sig(Add),
                    '-' => Sig(Sub),
                    '*' => Sig(Mul),
                    '/' => Sig(Div),
                    '&' => Sig(And),
                    '|' => Sig(Or),
                    '@' => Sig(At),
                    '$' => Sig(Dollar),
                    '%' => Sig(Mod),
                    '^' => Sig(Pow),
                    '<' => Sig(Lt),
                    '>' => Sig(Gt),
                    '~' => Sig(Tilde),
                    '!' => Sig(Bang),
                    '=' => Sig(Eq),
                    '?' => Sig(Tern),
                    '`' => return Some(self.parse_macro_call(pos)),
                    'a'..='z' | 'A'..='Z' | '_' => return Some(self.parse_ident(pos)),
                    '\'' => return Some(self.parse_name(pos)),
                    '\"' => return Some(self.parse_string(pos)),
                    any => {
                        return Some(Token {
                            kind: TokenKind::Error(any),
                            span: SpanExp {
                                exp_start: pos,
                                exp_end: self.source.pos(),
                            },
                        })
                    }
                },
            }
        };

        Some(Token {
            kind,
            span: SpanExp {
                exp_start: pos,
                exp_end: self.source.pos(),
            },
        })
    }

    fn parse_foreign_block(&mut self, first: usize, kw: Keyword) -> Token {
        while self.source.peek().map(is_uc_whitespace).unwrap_or(false) {
            self.source.next();
        }

        let start_pos;
        let end_pos;

        match self.source.peek() {
            Some('{') => {
                self.source.next();
                start_pos = self.source.pos();
            }
            Some(_) => {
                return Token {
                    kind: TokenKind::Incomplete(
                        Box::new(TokenKind::ForeignBlock(kw, None)),
                        IncompleteReason::InvalidChar,
                    ),
                    span: SpanExp {
                        exp_start: first,
                        exp_end: self.source.pos(),
                    },
                }
            }
            None => {
                return Token {
                    kind: TokenKind::Incomplete(
                        Box::new(TokenKind::ForeignBlock(kw, None)),
                        IncompleteReason::Eol,
                    ),
                    span: SpanExp {
                        exp_start: first,
                        exp_end: self.source.pos(),
                    },
                }
            }
        }

        let mut brace_count = 1;
        loop {
            if brace_count == 0 {
                end_pos = self.source.pos();
                break;
            }

            match self.source.next() {
                Some('{') => brace_count += 1,
                Some('}') => brace_count -= 1,
                Some(_) => {}
                None => {
                    return Token {
                        kind: TokenKind::Incomplete(
                            Box::new(TokenKind::ForeignBlock(kw, None)),
                            IncompleteReason::Eol,
                        ),
                        span: SpanExp {
                            exp_start: first,
                            exp_end: self.source.pos(),
                        },
                    }
                }
            }
        }

        Token {
            kind: TokenKind::ForeignBlock(
                kw,
                Some(SpanExp {
                    exp_start: start_pos,
                    exp_end: end_pos,
                }),
            ),
            span: SpanExp {
                exp_start: first,
                exp_end: self.source.pos(),
            },
        }
    }

    fn parse_ident(&mut self, first: usize) -> Token {
        while matches!(
            self.source.peek(),
            Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9')
        ) {
            self.source.next();
        }

        let span = SpanExp {
            exp_start: first,
            exp_end: self.source.pos(),
        };
        let text = &self.source.text[span.start..span.end];

        match Keyword::from_str(text) {
            Ok(
                kw
                @
                (Keyword::CppText
                | Keyword::StructCppText
                | Keyword::DefaultProperties
                | Keyword::StructDefaultProperties),
            ) => {
                // Eat a block recognized by a balanced set of curly braces.
                // Doing this in the lexer might not be 100% proper but since
                // these blocks can contain properties or C++ syntax,
                // any consumer of the lexer output would have to
                // contain the same logic to avoid lexer errors due to weird
                // tokens (#, \, )
                self.parse_foreign_block(first, kw)
            }
            Ok(kw) => Token {
                kind: TokenKind::Kw(kw),
                span,
            },
            Err(_) => Token {
                kind: TokenKind::Identifier,
                span,
            },
        }
    }

    fn parse_eol_comment(&mut self, first: usize) -> Token {
        while !matches!(self.source.peek(), Some('\r' | '\n') | None) {
            self.source.next();
        }

        let span = SpanExp {
            exp_start: first,
            exp_end: self.source.pos(),
        };
        Token {
            kind: TokenKind::Comment,
            span,
        }
    }

    fn parse_block_comment(&mut self, first: usize) -> Token {
        loop {
            match self.source.next() {
                Some('*') => match self.source.peek() {
                    Some('/') => {
                        self.source.next();
                        break;
                    }
                    Some(_) => continue,
                    None => {
                        return Token {
                            kind: TokenKind::Incomplete(
                                Box::new(TokenKind::Comment),
                                IncompleteReason::Eol,
                            ),
                            span: SpanExp {
                                exp_start: first,
                                exp_end: self.source.pos(),
                            },
                        }
                    }
                },
                Some(_) => continue,
                None => {
                    return Token {
                        kind: TokenKind::Incomplete(
                            Box::new(TokenKind::Comment),
                            IncompleteReason::Eol,
                        ),
                        span: SpanExp {
                            exp_start: first,
                            exp_end: self.source.pos(),
                        },
                    }
                }
            }
        }

        Token {
            kind: TokenKind::Comment,
            span: SpanExp {
                exp_start: first,
                exp_end: self.source.pos(),
            },
        }
    }

    fn parse_number(&mut self, first: usize) -> Token {
        let mut hex = false;
        let mut float = false;
        while let Some(c @ ('0'..='9' | 'a'..='f' | 'A'..='F' | '.' | 'x' | 'X')) =
            self.source.peek()
        {
            float |= c == '.';
            hex |= c == 'x' || c == 'X';
            self.source.next();
        }

        let span = SpanExp {
            exp_start: first,
            exp_end: self.source.pos(),
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

    fn parse_name(&mut self, first: usize) -> Token {
        while let Some('0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | '-' | '.' | ' ') =
            self.source.peek()
        {
            self.source.next();
        }

        let kind = match self.source.peek() {
            Some('\'') => {
                self.source.next();
                TokenKind::Name
            }
            Some(_) => {
                TokenKind::Incomplete(Box::new(TokenKind::Name), IncompleteReason::InvalidChar)
            }
            None => TokenKind::Incomplete(Box::new(TokenKind::Name), IncompleteReason::Eol),
        };
        Token {
            kind,
            span: SpanExp {
                exp_start: first,
                exp_end: self.source.pos(),
            },
        }
    }

    fn parse_string(&mut self, first: usize) -> Token {
        loop {
            match self.source.peek() {
                Some('"') => {
                    self.source.next();
                    return Token {
                        kind: TokenKind::String,
                        span: SpanExp {
                            exp_start: first,
                            exp_end: self.source.pos(),
                        },
                    };
                }
                Some('\\') => {
                    self.source.next();
                    match self.source.peek() {
                        Some('\r' | '\n') | None => {
                            return Token {
                                kind: TokenKind::Incomplete(
                                    Box::new(TokenKind::String),
                                    IncompleteReason::Eol,
                                ),
                                span: SpanExp {
                                    exp_start: first,
                                    exp_end: self.source.pos(),
                                },
                            }
                        }
                        _ => {
                            self.source.next();
                        }
                    }
                }
                Some(_) => {
                    self.source.next();
                }
                None => {
                    return Token {
                        kind: TokenKind::Incomplete(
                            Box::new(TokenKind::String),
                            IncompleteReason::Eol,
                        ),
                        span: SpanExp {
                            exp_start: first,
                            exp_end: self.source.pos(),
                        },
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
struct Source<'a> {
    text: &'a str,
    chars: std::str::Chars<'a>,
}

impl<'a> Source<'a> {
    fn new(text: &'a str) -> Self {
        Source {
            text,
            chars: text.chars(),
        }
    }

    fn pos(&self) -> usize {
        self.text.len() - self.chars.as_str().len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }
}

impl<'a> Iterator for Source<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
    }
}
