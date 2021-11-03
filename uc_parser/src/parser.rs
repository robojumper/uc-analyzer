//! The UnrealScript parser, parsing lexemes to an unresolved
//! High level Intermediate Represenation.

use bitflags::bitflags;
use once_cell::sync::Lazy;

use std::collections::HashMap;

use uc_def::{
    ClassDef, ClassFlags, ClassHeader, ConstDef, ConstVal, DelegateDef, DimCount, EnumDef, FuncDef,
    Hir, Identifier, PropFlags, StructDef, Ty, VarDef, VarInstance,
};

use crate::{
    lexer::{Delim, Keyword, Lexer, NumberSyntax, Sigil, Token, TokenKind as Tk},
    NumberLiteral,
};

#[derive(Debug)]
enum TopLevelItem {
    Const(ConstDef),
    Var(VarDef<Identifier>),
    Struct(StructDef<Identifier>),
    Enum(EnumDef),
    Delegate(DelegateDef<Identifier>),
    Func(FuncDef<Identifier>),
}

#[derive(Clone, Debug)]
struct Parser<'a> {
    lex: Lexer<'a>,
    open_delims: isize,
    errs: Vec<String>,
}

bitflags! {
    pub struct ModifierCount: u32 {
        const ALLOW_NONE = 1 << 0;
        const ALLOW_EMPTY = 1 << 1;
        const ALLOW_ONE = 1 << 2;
        const ALLOW_MULTIPLE = 1 << 3;

        const ALLOW_PAREN = Self::ALLOW_NONE.bits | Self::ALLOW_ONE.bits | Self::ALLOW_MULTIPLE.bits;
    }
}

#[derive(Clone, Copy, Debug)]
enum DeclFollowups {
    /// `var const`
    Nothing,
    /// `var private{private}`
    OptForeignBlock,
    /// `var(Category)`
    IdentModifiers(ModifierCount),
    /// `native(129)`
    NumberModifiers(ModifierCount),
}

#[derive(Clone, Debug)]
struct ModifierConfig {
    modifiers: HashMap<Keyword, DeclFollowups>,
}

static CLASS_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(
        Keyword::Native,
        DeclFollowups::IdentModifiers(ModifierCount::ALLOW_NONE | ModifierCount::ALLOW_ONE),
    );

    ModifierConfig { modifiers }
});

static VAR_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(Keyword::Native, DeclFollowups::Nothing);
    modifiers.insert(
        Keyword::Config,
        DeclFollowups::IdentModifiers(ModifierCount::ALLOW_NONE | ModifierCount::ALLOW_ONE),
    );
    modifiers.insert(Keyword::Const, DeclFollowups::Nothing);
    modifiers.insert(Keyword::EditConst, DeclFollowups::Nothing);

    modifiers.insert(Keyword::NoExport, DeclFollowups::Nothing);
    modifiers.insert(Keyword::Transient, DeclFollowups::Nothing);
    modifiers.insert(Keyword::DataBinding, DeclFollowups::Nothing);

    modifiers.insert(Keyword::Public, DeclFollowups::OptForeignBlock);
    modifiers.insert(Keyword::Private, DeclFollowups::OptForeignBlock);
    modifiers.insert(Keyword::Protected, DeclFollowups::OptForeignBlock);
    modifiers.insert(Keyword::PrivateWrite, DeclFollowups::OptForeignBlock);
    modifiers.insert(Keyword::ProtectedWrite, DeclFollowups::OptForeignBlock);

    ModifierConfig { modifiers }
});

static STRUCT_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(Keyword::Native, DeclFollowups::Nothing);
    modifiers.insert(Keyword::Immutable, DeclFollowups::Nothing);

    ModifierConfig { modifiers }
});

static FUNC_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(Keyword::Function, DeclFollowups::Nothing);
    modifiers.insert(Keyword::Event, DeclFollowups::Nothing);
    modifiers.insert(
        Keyword::Native,
        DeclFollowups::NumberModifiers(ModifierCount::ALLOW_NONE | ModifierCount::ALLOW_ONE),
    );
    modifiers.insert(
        Keyword::Operator,
        DeclFollowups::NumberModifiers(ModifierCount::ALLOW_NONE | ModifierCount::ALLOW_ONE),
    );
    modifiers.insert(Keyword::PreOperator, DeclFollowups::Nothing);
    modifiers.insert(Keyword::PostOperator, DeclFollowups::Nothing);

    modifiers.insert(Keyword::Static, DeclFollowups::Nothing);
    modifiers.insert(Keyword::Final, DeclFollowups::Nothing);

    ModifierConfig { modifiers }
});

impl<'a> Parser<'a> {
    fn new(lex: Lexer<'a>) -> Self {
        Self {
            lex,
            open_delims: 0,
            errs: vec![],
        }
    }

    fn next(&mut self) -> Option<Token> {
        loop {
            match self.lex.next()? {
                Token {
                    kind: Tk::Comment, ..
                } => {}
                x => {
                    match x.kind {
                        Tk::Open(_) => self.open_delims += 1,
                        Tk::Close(_) => self.open_delims -= 1,
                        _ => {}
                    }
                    return Some(x);
                }
            }
        }
    }

    fn expect(&mut self, kind: Tk) -> Result<Token, String> {
        let tok = self.next().ok_or_else(|| "eof".to_owned())?;
        if tok.kind == kind {
            Ok(tok)
        } else if tok.kind == Tk::Identifier {
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
        self.expect(Tk::Identifier)
            .map(|t| self.lex.extract_ident(&t))
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

    fn peek(&self) -> Option<Token> {
        self.clone().next()
    }

    fn peek_any(&self) -> Result<Token, String> {
        self.peek().ok_or_else(|| "eof".to_owned())
    }

    fn next_any(&mut self) -> Result<Token, String> {
        self.next().ok_or_else(|| "eof".to_owned())
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

    fn parse_class_def(&mut self) -> Result<ClassDef<Identifier>, String> {
        let token = self.next_any()?;
        let class = match token.kind {
            Tk::Kw(Keyword::Class) => true,
            Tk::Kw(Keyword::Interface) => false,
            _ => return Err(self.fmt_unexpected(&token)),
        };

        let name = self
            .expect(Tk::Identifier)
            .map(|t| self.lex.extract_ident(&t))?;

        let def = if class {
            let extends = if self.eat(Tk::Kw(Keyword::Extends)) {
                let extends_name = self.expect_ident()?;
                Some(extends_name)
            } else {
                None
            };

            let within = if self.eat(Tk::Kw(Keyword::Within)) {
                let within_name = self.expect_ident()?;
                Some(within_name)
            } else {
                None
            };

            // TODO: Parse all the class modifiers
            //self.expect(TokenKind::Semi)?;
            self.recover_to_semi();

            ClassHeader::Class {
                extends,
                within,
                implements: vec![],
                flags: ClassFlags::empty(),
            }
        } else {
            let extends = if self.eat(Tk::Kw(Keyword::Extends)) {
                Some(self.expect_ident()?)
            } else {
                None
            };

            self.eat(Tk::Kw(Keyword::Native));
            self.expect(Tk::Semi)?;

            ClassHeader::Interface { extends }
        };

        Ok(ClassDef { kind: def, name })
    }

    fn parse_const(&mut self) -> Result<ConstDef, String> {
        let name = self.expect_ident()?;
        self.expect(Tk::Sig(Sigil::Eq))?;

        let val = match self.next_any()?.kind {
            Tk::Name => ConstVal::Name,
            Tk::String => ConstVal::String,
            Tk::Number(NumberSyntax::Int | NumberSyntax::Hex) => ConstVal::Int,
            Tk::Number(NumberSyntax::Float) => ConstVal::Float,
            _ => return Err("expected const value".to_owned()),
        };

        self.expect(Tk::Semi)?;

        Ok(ConstDef { name, val })
    }

    fn parse_parts_until(&mut self, end_token: Tk) -> Result<Vec<Identifier>, String> {
        let mut parts = vec![];
        let mut dot = false;
        loop {
            let mut tok = self.next_any()?;
            match tok.kind {
                tk if tk == end_token => break,
                Tk::Sig(Sigil::Dot) if dot => {
                    tok = self.next_any()?;
                }
                _ => {}
            }
            if tok.kind == Tk::Identifier {
                parts.push(self.lex.extract_ident(&tok));
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
        self.expect(Tk::Sig(Sigil::Lt))?;

        self.parse_parts_until(Tk::Sig(Sigil::Gt))
    }

    fn parse_var(&mut self) -> Result<VarDef<Identifier>, String> {
        if self.eat(Tk::Open(Delim::LParen)) {
            self.eat(Tk::Identifier);
            self.expect(Tk::Close(Delim::RParen))?;
        }

        self.ignore_kws(&*VAR_MODIFIERS)?;
        let ty_tok = self.next_any()?;

        let ty = match &ty_tok.kind {
            Tk::Kw(Keyword::Array) => {
                self.expect(Tk::Sig(Sigil::Lt))?;
                let id = self.expect_ident()?;
                self.expect(Tk::Sig(Sigil::Gt))?;
                Ty::Array(id)
            }
            Tk::Kw(Keyword::Class) => {
                let class = if self.eat(Tk::Sig(Sigil::Lt)) {
                    let c = self.expect_ident()?;
                    self.expect(Tk::Sig(Sigil::Gt))?;
                    Some(c)
                } else {
                    None
                };
                Ty::Class(class)
            }
            Tk::Kw(Keyword::Delegate) => Ty::Delegate(self.parse_angle_type()?),
            Tk::Identifier => Ty::Simple(self.lex.extract_ident(&ty_tok)),
            _ => {
                return Err(format!("expected type after modifiers, got {:?}", ty_tok));
            }
        };

        let mut names = vec![];

        loop {
            let var_name = self.expect_ident()?;
            let count = if self.eat(Tk::Open(Delim::LBrack)) {
                match self.peek_any()?.kind {
                    Tk::Number(_) => {
                        let cnt_lit = self.expect_number()?;
                        let cnt = if let NumberLiteral::Int(cnt) = cnt_lit {
                            cnt
                        } else {
                            return Err("not an integer".to_owned());
                        };
                        self.expect(Tk::Close(Delim::RBrack))?;
                        DimCount::Number(cnt as u32)
                    }
                    Tk::Identifier => {
                        let parts = self.parse_parts_until(Tk::Close(Delim::RBrack))?;
                        DimCount::Complex(parts)
                    }
                    kind => {
                        return Err(format!("expected number or identifier, got {:?}", kind));
                    }
                }
            } else {
                DimCount::None
            };

            // Native export text
            if self.eat(Tk::Open(Delim::LBrace)) {
                self.lex
                    .ignore_foreign_block(Tk::Open(Delim::LBrace))
                    .map_err(|e| format!("{:?}", e))?;
            }
            // Editor metadata
            if self.eat(Tk::Sig(Sigil::Lt)) {
                self.lex
                    .ignore_foreign_block(Tk::Sig(Sigil::Lt))
                    .map_err(|e| format!("{:?}", e))?;
            }

            names.push(VarInstance {
                name: var_name,
                count,
            });

            if !self.eat(Tk::Sig(Sigil::Comma)) {
                break;
            }
        }

        self.expect(Tk::Semi)?;

        Ok(VarDef {
            names,
            ty,
            flags: PropFlags::empty(),
        })
    }

    fn parse_enum(&mut self) -> Result<EnumDef, String> {
        let name = self.expect_ident()?;
        self.expect(Tk::Open(Delim::LBrace))?;

        let mut variants = vec![];
        let mut comma = false;
        loop {
            if self.eat(Tk::Close(Delim::RBrace)) {
                break;
            }
            if comma {
                self.expect(Tk::Sig(Sigil::Comma))?;
            }
            if self.eat(Tk::Close(Delim::RBrace)) {
                break;
            }
            let tok = self.next_any()?;
            if tok.kind == Tk::Identifier {
                variants.push(self.lex.extract_ident(&tok));
                // Editor metadata
                if self.eat(Tk::Sig(Sigil::Lt)) {
                    self.ignore_foreign_block(Tk::Sig(Sigil::Lt))?;
                }
                comma = true;
            } else {
                return Err(format!("expected enum variant name, got {:?}", tok));
            }
        }

        self.expect(Tk::Semi)?;

        Ok(EnumDef { name, variants })
    }

    fn ignore_foreign_block(&mut self, opener: Tk) -> Result<(), String> {
        self.lex
            .ignore_foreign_block(opener)
            .map_err(|e| format!("{:?}", e))
    }

    fn parse_struct(&mut self) -> Result<StructDef<Identifier>, String> {
        if self.eat(Tk::Open(Delim::LBrace)) {
            self.ignore_foreign_block(Tk::Open(Delim::LBrace))?;
        }
        self.ignore_kws(&*STRUCT_MODIFIERS)?;
        let name = self.expect_ident()?;

        let extends = if self.eat(Tk::Kw(Keyword::Extends)) {
            Some(self.expect_ident()?)
        } else {
            None
        };

        self.expect(Tk::Open(Delim::LBrace))?;

        let mut fields = vec![];

        loop {
            match self.next_any()?.kind {
                Tk::Close(Delim::RBrace) => break,
                Tk::Kw(Keyword::Var) => {
                    fields.push(self.parse_var()?);
                }
                Tk::Kw(Keyword::StructDefaultProperties | Keyword::StructCppText) => {
                    let opener = self.expect(Tk::Open(Delim::LBrace))?;
                    self.ignore_foreign_block(opener.kind)?;
                }
                t => return Err(format!("unexpected token: {:?}", t)),
            }
        }

        self.expect(Tk::Semi)?;

        Ok(StructDef {
            name,
            extends,
            fields,
        })
    }

    fn ignore_directive(&mut self) -> Result<(), String> {
        let directive_name = self.expect_ident()?;
        if directive_name.as_ref().eq_ignore_ascii_case("error") {
            return Err("error directive".to_owned());
        } else if directive_name.as_ref().eq_ignore_ascii_case("linenumber") {
            self.expect(Tk::Number(NumberSyntax::Int))?;
        } else {
            return Err("unknown directive".to_owned());
        }
        self.eat(Tk::Semi);
        Ok(())
    }

    fn ignore_followups(&mut self, followups: &DeclFollowups) -> Result<(), String> {
        match followups {
            DeclFollowups::Nothing => Ok(()),
            DeclFollowups::OptForeignBlock => match self.peek() {
                Some(Token {
                    kind: opener @ Tk::Open(Delim::LParen),
                    ..
                }) => {
                    self.next();
                    self.ignore_foreign_block(opener)?;
                    Ok(())
                }
                Some(_) | None => Ok(()),
            },
            DeclFollowups::IdentModifiers(mods) | DeclFollowups::NumberModifiers(mods) => {
                match self.peek() {
                    Some(Token {
                        kind: Tk::Open(Delim::LBrace),
                        ..
                    }) => {
                        if mods.intersects(ModifierCount::ALLOW_PAREN) {
                            self.next();
                            let mut comma = false;
                            loop {
                                match self.peek() {
                                    Some(Token {
                                        kind: Tk::Close(Delim::RBrace),
                                        ..
                                    }) => {
                                        self.next();
                                        break Ok(());
                                    }
                                    Some(_) | None => {}
                                }
                                if comma {
                                    self.expect(Tk::Sig(Sigil::Comma))?;
                                }
                                if let DeclFollowups::IdentModifiers(_) = followups {
                                    self.expect_ident()?;
                                } else {
                                    self.expect_number()?;
                                }
                                comma = true;
                            }
                        } else {
                            Ok(())
                        }
                    }
                    Some(_) | None => {
                        if mods.contains(ModifierCount::ALLOW_EMPTY) {
                            Ok(())
                        } else {
                            Err("missing followups".to_owned())
                        }
                    }
                }
            }
        }
    }

    fn ignore_kws(&mut self, mods: &ModifierConfig) -> Result<(), String> {
        loop {
            let kw_or_next = self.peek();
            match &kw_or_next {
                Some(tok) => match tok.kind {
                    Tk::Kw(kw) => match mods.modifiers.get(&kw) {
                        Some(followups) => {
                            self.next();
                            self.ignore_followups(followups)?;
                        }
                        None => return Ok(()),
                    },
                    _ => return Ok(()),
                },
                None => return Ok(()),
            }
        }
    }

    fn parse_one_item(&mut self) -> Result<Option<TopLevelItem>, String> {
        loop {
            break match self.next() {
                Some(tok) => match tok.kind {
                    Tk::Kw(Keyword::Const) => Ok(Some(TopLevelItem::Const(self.parse_const()?))),
                    Tk::Kw(Keyword::Var) => Ok(Some(TopLevelItem::Var(self.parse_var()?))),
                    Tk::Kw(Keyword::Enum) => Ok(Some(TopLevelItem::Enum(self.parse_enum()?))),
                    Tk::Kw(Keyword::Struct) => Ok(Some(TopLevelItem::Struct(self.parse_struct()?))),
                    Tk::Kw(Keyword::CppText | Keyword::DefaultProperties) => {
                        let brace = self.expect(Tk::Open(Delim::LBrace))?;
                        self.ignore_foreign_block(brace.kind)?;
                        continue;
                    }
                    Tk::Kw(kw) if FUNC_MODIFIERS.modifiers.contains_key(&kw) => Ok(Some(self.parse_function()?)),
                    Tk::Directive => {
                        self.ignore_directive()?;
                        continue;
                    }
                    Tk::Comment => unreachable!("filtered out in next"),
                    _ => continue,
                },
                None => Ok(None),
            };
        }
    }

    fn parse_items(&mut self) -> Vec<TopLevelItem> {
        let mut items = vec![];
        loop {
            match self.parse_one_item() {
                Ok(Some(item)) => items.push(item),
                Ok(None) => break,
                Err(e) => {
                    self.errs.push(e);
                    self.recover_to_semi();
                }
            }
        }
        items
    }
}

pub fn parse(lex: Lexer) -> Hir<Identifier> {
    let mut parser = Parser::new(lex);
    let x = parser.parse_class_def();
    println!("{:#?}", &x);
    let items = parser.parse_items();
    println!("{:#?}", &items);
    println!("{:#?}", &parser.errs);

    todo!();
}
