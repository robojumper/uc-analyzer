use std::str::FromStr;

use uc_def::Identifier;

use crate::NumberLiteral;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ExpSpan {
    pub exp_start: usize,
    pub exp_end: usize,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub span: ExpSpan,
    pub kind: TokenKind,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    /// A block or EOL comment
    Comment,
    /// An unknown start of a token
    Error(char),
    /// A token that hit EOF or an invalid character before being terminated
    Incomplete(IncompleteReason),
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
    /// The `#`
    Directive,
    /// An identifier
    Identifier,
    /// An integer, hex, or floating point literal
    Number(NumberSyntax),
    /// A string literal
    String,
    /// A name literal
    Name,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum IncompleteReason {
    Eol,
    InvalidChar,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Delim {
    /// `[`
    LBrack,
    /// `]`
    RBrack,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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

impl Sigil {
    pub fn is_overloadable_op(&self) -> bool {
        use Sigil::*;
        match self {
            Add | AddAdd | AddAssign | And | AndAnd | At | AtAssign | Bang | BangEq | Div
            | DivAssign | Dollar | DollarAssign | Eq | EqEq | Gt | GtEq | GtGt | GtGtGt | Lt
            | LtEq | LtLt | Mod | Mul | MulAssign | Or | OrOr | Pow | Sub | SubAssign | SubSub
            | Tilde | TildeEq => true,
            Colon | Comma | Dot | Tern => false,
        }
    }
}

#[derive(Copy, Clone, Debug, strum::EnumString, Eq, PartialEq, Hash)]
#[strum(serialize_all = "lowercase")]
pub enum Keyword {
    Abstract,
    Array,
    Class,
    Config,
    Const,
    CppText,
    DataBinding,
    Default,
    DefaultProperties,
    Delegate,
    DependsOn,
    EditConst,
    EditorOnly,
    Enum,
    Event,
    Extends,
    Final,
    Function,
    Immutable,
    Interface,
    Localized,
    Native,
    NoExport,
    Operator,
    Placeable,
    PostOperator,
    PreOperator,
    Private,
    PrivateWrite,
    Protected,
    ProtectedWrite,
    Public,
    Replication,
    Static,
    Struct,
    StructCppText,
    StructDefaultProperties,
    Transient,
    Var,
    Within,
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
    source: Source<'a>,
}

fn is_uc_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\r' | '\n')
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            source: Source::new(text),
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<Token> {
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
                    ')' => Close(RParen),
                    '[' => Open(LBrack),
                    ']' => Close(RBrack),
                    '{' => Open(LBrace),
                    '}' => Close(RBrace),
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
                    '#' => Directive,
                    'a'..='z' | 'A'..='Z' | '_' => return Some(self.parse_ident(pos)),
                    '\'' => return Some(self.parse_name(pos)),
                    '\"' => return Some(self.parse_string(pos)),
                    any => {
                        return Some(Token {
                            kind: TokenKind::Error(any),
                            span: ExpSpan {
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
            span: ExpSpan {
                exp_start: pos,
                exp_end: self.source.pos(),
            },
        })
    }

    /// Eats the {...} from a defaultproperties block, or the <Tooltip=...>
    /// from a variable declaration. Returns Err(Incomplete) if incomplete.
    pub fn ignore_foreign_block(&mut self, opener: TokenKind) -> Result<(), Token> {
        let (recurser, closer) = match opener {
            TokenKind::Open(Delim::LBrace) => (Some('{'), Some('}')),
            TokenKind::Sig(Sigil::Lt) => (None, Some('>')),
            _ => panic!(
                "TokenKind {:?} is not a valid opener for foreign block",
                opener
            ),
        };

        let mut brace_count = 1;
        let first = self.source.pos();

        loop {
            if brace_count == 0 {
                break;
            }

            let next = if let Some(next) = self.source.next() {
                next
            } else {
                return Err(Token {
                    span: ExpSpan {
                        exp_start: first,
                        exp_end: self.source.pos(),
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

    fn parse_ident(&mut self, first: usize) -> Token {
        while matches!(
            self.source.peek(),
            Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9')
        ) {
            self.source.next();
        }

        let span = ExpSpan {
            exp_start: first,
            exp_end: self.source.pos(),
        };
        let text = &self.source.text[span.exp_start..span.exp_end];

        match Keyword::from_str(text) {
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

    pub fn extract_ident(&self, token: &Token) -> Identifier {
        assert_eq!(token.kind, TokenKind::Identifier);
        Identifier::from_str(&self.source.text[token.span.exp_start..token.span.exp_end]).unwrap()
    }

    pub fn extract_number(&self, token: &Token) -> Result<NumberLiteral, String> {
        match token.kind {
            TokenKind::Number(NumberSyntax::Int | NumberSyntax::Hex) => Ok(NumberLiteral::Int(
                self.source.text[token.span.exp_start..token.span.exp_end]
                    .parse::<i32>()
                    .map_err(|e| e.to_string())?,
            )),
            TokenKind::Number(NumberSyntax::Float) => Ok(NumberLiteral::Float(
                self.source.text[token.span.exp_start..token.span.exp_end]
                    .parse::<f32>()
                    .map_err(|e| e.to_string())?,
            )),
            TokenKind::Number(NumberSyntax::Wild) => Err("unrecognized number format".to_owned()),
            _ => panic!("not a number: {:?}", token),
        }
    }

    fn parse_eol_comment(&mut self, first: usize) -> Token {
        while !matches!(self.source.peek(), Some('\r' | '\n') | None) {
            self.source.next();
        }

        let span = ExpSpan {
            exp_start: first,
            exp_end: self.source.pos(),
        };
        Token {
            kind: TokenKind::Comment,
            span,
        }
    }

    fn parse_block_comment(&mut self, first: usize) -> Token {
        let mut level = 1;
        loop {
            match self.source.next() {
                Some('*') => match self.source.peek() {
                    Some('/') => {
                        self.source.next();
                        level -= 1;
                        if level == 0 {
                            break;
                        }
                    }
                    Some(_) => continue,
                    None => {
                        return Token {
                            kind: TokenKind::Incomplete(IncompleteReason::Eol),
                            span: ExpSpan {
                                exp_start: first,
                                exp_end: self.source.pos(),
                            },
                        }
                    }
                },
                Some('/') => match self.source.peek() {
                    Some('*') => {
                        self.source.next();
                        level += 1;
                    }
                    Some(_) => continue,
                    None => {
                        return Token {
                            kind: TokenKind::Incomplete(IncompleteReason::Eol),
                            span: ExpSpan {
                                exp_start: first,
                                exp_end: self.source.pos(),
                            },
                        }
                    }
                },
                Some(_) => continue,
                None => {
                    return Token {
                        kind: TokenKind::Incomplete(IncompleteReason::Eol),
                        span: ExpSpan {
                            exp_start: first,
                            exp_end: self.source.pos(),
                        },
                    }
                }
            }
        }

        Token {
            kind: TokenKind::Comment,
            span: ExpSpan {
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

        let span = ExpSpan {
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
            Some(_) => TokenKind::Incomplete(IncompleteReason::InvalidChar),
            None => TokenKind::Incomplete(IncompleteReason::Eol),
        };
        Token {
            kind,
            span: ExpSpan {
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
                        span: ExpSpan {
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
                                kind: TokenKind::Incomplete(IncompleteReason::Eol),
                                span: ExpSpan {
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
                        kind: TokenKind::Incomplete(IncompleteReason::Eol),
                        span: ExpSpan {
                            exp_start: first,
                            exp_end: self.source.pos(),
                        },
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
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
