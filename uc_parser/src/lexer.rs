use std::str::{self, FromStr};

use uc_def::Op;
use uc_name::Identifier;

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

#[derive(Copy, Clone, Debug, Eq, PartialEq, strum::AsRefStr)]
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
    MulMul,
    MulAssign,
    Or,
    OrOr,
    Pow,
    PowPow,
    Tern,
    Sub,
    SubAssign,
    SubSub,
    Tilde,
    TildeEq,

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

impl Sigil {
    pub fn is_overloadable_op(&self) -> bool {
        use Sigil::*;
        match self {
            Add | AddAdd | AddAssign | And | AndAnd | At | AtAssign | Bang | BangEq | Div
            | DivAssign | Dollar | DollarAssign | Eq | EqEq | Gt | GtEq | GtGt | GtGtGt | Lt
            | LtEq | LtLt | Mod | Mul | MulAssign | MulMul | Or | OrOr | Pow | PowPow | Sub
            | SubAssign | SubSub | Tilde | TildeEq => true,
            Colon | Dot | Tern | LParen | RParen | LBrack | RBrack | LBrace | RBrace => false,
        }
    }

    pub fn to_op(self) -> Op {
        use Sigil::*;
        match self {
            Add => Op::Add,
            AddAdd => Op::AddAdd,
            AddAssign => Op::AddAssign,
            And => Op::And,
            AndAnd => Op::AndAnd,
            At => Op::At,
            AtAssign => Op::AtAssign,
            Bang => Op::Bang,
            BangEq => Op::BangEq,
            Div => Op::Div,
            DivAssign => Op::DivAssign,
            Dollar => Op::Dollar,
            DollarAssign => Op::DollarAssign,
            EqEq => Op::EqEq,
            Gt => Op::Gt,
            GtEq => Op::GtEq,
            GtGt => Op::GtGt,
            GtGtGt => Op::GtGtGt,
            Lt => Op::Lt,
            LtEq => Op::LtEq,
            LtLt => Op::LtLt,
            Mod => Op::Mod,
            Mul => Op::Mul,
            MulMul => Op::MulMul,
            MulAssign => Op::MulAssign,
            Or => Op::Or,
            OrOr => Op::OrOr,
            Pow => Op::Pow,
            PowPow => Op::PowPow,
            Sub => Op::Sub,
            SubAssign => Op::SubAssign,
            SubSub => Op::SubSub,
            Tilde => Op::Tilde,
            TildeEq => Op::TildeEq,
            s => panic!("Not an operator: {:?}", s),
        }
    }
}

#[derive(Copy, Clone, Debug, strum::EnumString, strum::AsRefStr, Eq, PartialEq, Hash)]
#[strum(ascii_case_insensitive)]
pub enum Keyword {
    Abstract,
    Array,
    Auto,
    AutoExpandCategories,
    Break,
    Case,
    Class,
    ClassGroup,
    Client,
    Coerce,
    CollapseCategories,
    Config,
    Const,
    Continue,
    CppText,
    Cross,
    CrossLevelPassive,
    DataBinding,
    Default,
    DefaultProperties,
    Delegate,
    DependsOn,
    Deprecated,
    Do,
    DontCollapseCategories,
    DontSortCategories,
    Dot,
    DuplicateTransient,
    EditConst,
    EditHide,
    EditFixedSize,
    EditInline,
    EditInlineNew,
    EditInlineUse,
    EditorOnly,
    EditTextBox,
    Else,
    Enum,
    Event,
    Exec,
    Export,
    Extends,
    Final,
    For,
    ForceScriptOrder,
    Foreach,
    Function,
    GlobalConfig,
    Goto,
    HideCategories,
    If,
    Ignores,
    Immutable,
    ImmutableWhenCooked,
    Implements,
    Inherits,
    Init,
    Input,
    Instanced,
    Interface,
    Interp,
    Iterator,
    Latent,
    Local,
    Localized,
    Map,
    Native,
    NativeReplication,
    New,
    NoClear,
    NoExport,
    NoExportHeader,
    NoImport,
    None,
    NonTransactional,
    NotForConsole,
    NotPlaceable,
    Operator,
    Optional,
    Out,
    PerObjectConfig,
    Placeable,
    PostOperator,
    PreOperator,
    Private,
    PrivateWrite,
    Protected,
    ProtectedWrite,
    Public,
    Ref,
    Reliable,
    Replication,
    RepNotify,
    RepRetry,
    Return,
    SerializeText,
    Server,
    ShowCategories,
    Simulated,
    Singular,
    Skip,
    State,
    StateOnly,
    Static,
    Struct,
    StructCppText,
    StructDefaultProperties,
    Switch,
    Transient,
    Unreliable,
    Until,
    Var,
    Virtual,
    While,
    Within,
}

impl Keyword {
    pub fn is_weak(&self) -> bool {
        matches!(
            self,
            Keyword::Class | Keyword::Interface | Keyword::Event | Keyword::Init
        )
    }
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

fn is_uc_whitespace(c: u8) -> bool {
    matches!(c, b' ' | b'\t' | b'\r' | b'\n')
}

impl<'a> Lexer<'a> {
    /// Lex an UnrealScript source file.
    pub fn new(text: &'a [u8]) -> Self {
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
            use self::Sigil::*;
            use self::TokenKind::*;
            match (c, c_) {
                (b'+', Some(b'+')) => {
                    self.source.next();
                    Sig(AddAdd)
                }
                (b'+', Some(b'=')) => {
                    self.source.next();
                    Sig(AddAssign)
                }
                (b'-', Some(b'=')) => {
                    self.source.next();
                    Sig(SubAssign)
                }
                (b'-', Some(b'-')) => {
                    self.source.next();
                    Sig(SubSub)
                }
                (b'*', Some(b'=')) => {
                    self.source.next();
                    Sig(MulAssign)
                }
                (b'/', Some(b'=')) => {
                    self.source.next();
                    Sig(DivAssign)
                }
                (b'@', Some(b'=')) => {
                    self.source.next();
                    Sig(AtAssign)
                }
                (b'$', Some(b'=')) => {
                    self.source.next();
                    Sig(DollarAssign)
                }
                (b'<', Some(b'<')) => {
                    self.source.next();
                    Sig(LtLt)
                }
                (b'<', Some(b'=')) => {
                    self.source.next();
                    Sig(LtEq)
                }
                (b'>', Some(b'>')) => {
                    self.source.next();
                    if let Some(b'>') = self.source.peek() {
                        self.source.next();
                        Sig(GtGtGt)
                    } else {
                        Sig(GtGt)
                    }
                }
                (b'>', Some(b'=')) => {
                    self.source.next();
                    Sig(GtEq)
                }
                (b'|', Some(b'|')) => {
                    self.source.next();
                    Sig(OrOr)
                }
                (b'&', Some(b'&')) => {
                    self.source.next();
                    Sig(AndAnd)
                }
                (b'=', Some(b'=')) => {
                    self.source.next();
                    Sig(EqEq)
                }
                (b'!', Some(b'=')) => {
                    self.source.next();
                    Sig(BangEq)
                }
                (b'~', Some(b'=')) => {
                    self.source.next();
                    Sig(TildeEq)
                }
                (b'^', Some(b'^')) => {
                    self.source.next();
                    Sig(PowPow)
                }
                (b'*', Some(b'*')) => {
                    self.source.next();
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
            TokenKind::Sig(Sigil::LBrace) => (Some(b'{'), Some(b'}')),
            TokenKind::Sig(Sigil::Lt) => (None, Some(b'>')),
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
            Some(b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9')
        ) {
            self.source.next();
        }

        let span = ExpSpan {
            exp_start: first,
            exp_end: self.source.pos(),
        };
        let text = str::from_utf8(&self.source.text[span.exp_start..span.exp_end])
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

        Token { kind, span }
    }

    pub fn extract_ident(&self, token: &Token) -> Identifier {
        assert_eq!(token.kind, TokenKind::Sym(Symbol::Identifier));
        Identifier::from_str(
            str::from_utf8(&self.source.text[token.span.exp_start..token.span.exp_end]).unwrap(),
        )
        .unwrap()
    }

    pub fn extract_string(&self, token: &Token) -> String {
        assert_eq!(token.kind, TokenKind::String);
        let string =
            String::from_utf8_lossy(&self.source.text[token.span.exp_start..token.span.exp_end]);
        string.into_owned()
    }

    pub fn extract_number(&self, token: &Token) -> Result<NumberLiteral, String> {
        let text =
            str::from_utf8(&self.source.text[token.span.exp_start..token.span.exp_end]).unwrap();
        match token.kind {
            TokenKind::Number(NumberSyntax::Int | NumberSyntax::Hex) => Ok(NumberLiteral::Int(
                text.parse::<i32>().map_err(|e| e.to_string())?,
            )),
            TokenKind::Number(NumberSyntax::Float) => Ok(NumberLiteral::Float(
                text.parse::<f32>().map_err(|e| e.to_string())?,
            )),
            TokenKind::Number(NumberSyntax::Wild) => Err("unrecognized number format".to_owned()),
            _ => panic!("not a number: {:?}", token),
        }
    }

    fn parse_eol_comment(&mut self, first: usize) -> Token {
        while !matches!(self.source.peek(), Some(b'\r' | b'\n') | None) {
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
                Some(b'*') => match self.source.peek() {
                    Some(b'/') => {
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
                Some(b'/') => match self.source.peek() {
                    Some(b'*') => {
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
        while let Some(c @ (b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' | b'.' | b'x' | b'X')) =
            self.source.peek()
        {
            float |= c == b'.';
            hex |= c == b'x' || c == b'X';
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
        while let Some(b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'-' | b'.' | b' ') =
            self.source.peek()
        {
            self.source.next();
        }

        let kind = match self.source.peek() {
            Some(b'\'') => {
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
                Some(b'"') => {
                    self.source.next();
                    return Token {
                        kind: TokenKind::String,
                        span: ExpSpan {
                            exp_start: first,
                            exp_end: self.source.pos(),
                        },
                    };
                }
                Some(b'\\') => {
                    self.source.next();
                    match self.source.peek() {
                        Some(b'\r' | b'\n') | None => {
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
    text: &'a [u8],
    bytes: std::slice::Iter<'a, u8>,
}

impl<'a> Source<'a> {
    fn new(text: &'a [u8]) -> Self {
        Source {
            text,
            bytes: text.iter(),
        }
    }

    fn pos(&self) -> usize {
        self.text.len() - self.bytes.as_slice().len()
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.clone().next().copied()
    }
}

impl<'a> Iterator for Source<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        self.bytes.next().copied()
    }
}
