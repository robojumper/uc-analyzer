//! The UnrealScript parser, parsing lexemes to an unresolved
//! High level Intermediate Represenation.
use std::str::FromStr;

mod modifiers;

use uc_def::{
    ClassDef, ClassFlags, ClassHeader, ConstDef, ConstVal, DelegateDef, DimCount, EnumDef, FuncArg,
    FuncBody, FuncDef, FuncFlags, FuncSig, Hir, Identifier, PropFlags, StructDef, Ty, VarDef,
    VarInstance,
};

use crate::{
    lexer::{Delim, Keyword, Lexer, NumberSyntax, Sigil, Token, TokenKind as Tk},
    NumberLiteral,
};

use self::modifiers::{DeclFollowups, ModifierConfig, ModifierCount};

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

    fn expect_ident_weak(&mut self) -> Result<Identifier, String> {
        let tok = self.next_any()?;
        match tok.kind {
            Tk::Identifier => Ok(self.lex.extract_ident(&tok)),
            Tk::Kw(kw) => {
                if kw.is_weak() {
                    Ok(Identifier::from_str(kw.as_ref()).unwrap())
                } else {
                    Err(format!("token {:?} is strong keyword", tok))
                }
            }
            _ => Err(self.fmt_unexpected(&tok)),
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

            self.ignore_kws(&*modifiers::CLASS_MODIFIERS)?;

            self.expect(Tk::Semi)?;

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

            self.ignore_kws(&*modifiers::INTERFACE_MODIFIERS)?;
            self.expect(Tk::Semi)?;

            ClassHeader::Interface { extends }
        };

        Ok(ClassDef { kind: def, name })
    }

    fn parse_const_val(&mut self) -> Result<ConstVal, String> {
        let tok = self.next_any()?;
        match tok.kind {
            Tk::Name => Ok(ConstVal::Name),
            Tk::String => Ok(ConstVal::String),
            Tk::Number(NumberSyntax::Int | NumberSyntax::Hex) => Ok(ConstVal::Int),
            Tk::Number(NumberSyntax::Float) => Ok(ConstVal::Float),
            Tk::Bool(_) => Ok(ConstVal::Bool),
            Tk::Identifier => Ok(ConstVal::ValueReference),
            _ => Err(format!("expected const value, got {:?}", tok)),
        }
    }

    fn parse_const(&mut self) -> Result<ConstDef, String> {
        let name = self.expect_ident()?;
        self.expect(Tk::Sig(Sigil::Eq))?;
        let val = self.parse_const_val()?;
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

    fn parse_ty(&mut self, first_tok: Option<Token>) -> Result<Ty<Identifier>, String> {
        let ty_tok = match first_tok {
            Some(t) => t,
            _ => self.next_any()?,
        };

        match &ty_tok.kind {
            Tk::Kw(Keyword::Array) => {
                self.expect(Tk::Sig(Sigil::Lt))?;
                let id = self.expect_ident()?;
                self.expect(Tk::Sig(Sigil::Gt))?;
                Ok(Ty::Array(id))
            }
            Tk::Kw(Keyword::Class) => {
                let class = if self.eat(Tk::Sig(Sigil::Lt)) {
                    let c = self.expect_ident()?;
                    self.expect(Tk::Sig(Sigil::Gt))?;
                    Some(c)
                } else {
                    None
                };
                Ok(Ty::Class(class))
            }
            Tk::Kw(Keyword::Delegate) => Ok(Ty::Delegate(self.parse_angle_type()?)),
            Tk::Identifier => Ok(Ty::Simple(self.lex.extract_ident(&ty_tok))),
            Tk::Kw(Keyword::Map) => {
                self.expect(Tk::Open(Delim::LBrace))?;
                self.ignore_foreign_block(Tk::Open(Delim::LBrace))?;
                Ok(Ty::Simple(Identifier::from_str("Map").unwrap()))
            }
            Tk::Kw(kw) if kw.is_weak() => {
                Ok(Ty::Simple(Identifier::from_str(kw.as_ref()).unwrap()))
            }
            _ => Err(format!("expected type after modifiers, got {:?}", ty_tok)),
        }
    }

    fn parse_var(&mut self) -> Result<VarDef<Identifier>, String> {
        if self.eat(Tk::Open(Delim::LParen)) {
            self.eat(Tk::Identifier);
            self.expect(Tk::Close(Delim::RParen))?;
        }

        self.ignore_kws(&*modifiers::VAR_MODIFIERS)?;
        let ty = self.parse_ty(None)?;

        let mut names = vec![];

        loop {
            let var_name = self.expect_ident_weak()?;
            let count = if self.eat(Tk::Open(Delim::LBrack)) {
                match self.peek_any()?.kind {
                    Tk::Number(_) => {
                        let cnt_lit = self.expect_number()?;
                        let NumberLiteral::Int(cnt) = cnt_lit else {
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

            if !self.eat(Tk::Comma) {
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
                self.expect(Tk::Comma)?;
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
        self.ignore_kws(&*modifiers::STRUCT_MODIFIERS)?;
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

    fn parse_function_sig(
        &mut self,
        allow_op_sigil: bool,
    ) -> Result<(Identifier, FuncSig<Identifier>), String> {
        let ty_or_name = self.next_any()?;
        let (ret_ty, name) = match (ty_or_name.kind, self.peek_any()?.kind) {
            (Tk::Identifier, Tk::Open(Delim::LParen)) => {
                (None, self.lex.extract_ident(&ty_or_name))
            }
            (Tk::Kw(kw), Tk::Open(Delim::LParen)) if kw.is_weak() => {
                (None, Identifier::from_str(kw.as_ref()).unwrap())
            }
            _ => (Some(self.parse_ty(Some(ty_or_name))?), {
                let name_tok = self.next_any()?;
                match name_tok.kind {
                    Tk::Identifier => self.lex.extract_ident(&name_tok),
                    Tk::Kw(kw) if kw.is_weak() => Identifier::from_str(kw.as_ref()).unwrap(),
                    Tk::Sig(s) if allow_op_sigil && s.is_overloadable_op() => {
                        Identifier::from_str(&format!("__op_{}", s.as_ref())).unwrap()
                    }
                    _ => return Err(format!("expected function name, got {:?}", name_tok)),
                }
            }),
        };

        self.expect(Tk::Open(Delim::LParen))?;
        let mut comma = false;
        let mut args = vec![];
        loop {
            if self.eat(Tk::Close(Delim::RParen)) {
                break;
            }
            if comma {
                self.expect(Tk::Comma)?;
            }

            self.ignore_kws(&*modifiers::ARG_MODIFIERS)?;
            let ty = self.parse_ty(None)?;
            let name = self.expect_ident_weak()?;

            let val = if self.eat(Tk::Sig(Sigil::Eq)) {
                Some(self.parse_const_val()?)
            } else {
                None
            };

            args.push(FuncArg { ty, name, val });

            comma = true;
        }
        Ok((name, FuncSig { ret_ty, args }))
    }

    fn parse_function(&mut self, first: Token) -> Result<FuncDef<Identifier>, String> {
        let Tk::Kw(kw) = first.kind else { panic!("parse_function needs the first function kw") };
        let followups = modifiers::FUNC_MODIFIERS
            .get(kw)
            .expect("not a valid function keyword");
        self.ignore_followups(followups)?;
        self.ignore_kws(&*modifiers::FUNC_MODIFIERS)?;

        let (name, sig) = self.parse_function_sig(true)?;

        self.eat(Tk::Kw(Keyword::Const));

        let body = if self.eat(Tk::Semi) {
            None
        } else if self.eat(Tk::Open(Delim::LBrace)) {
            // TODO: Body
            self.ignore_foreign_block(Tk::Open(Delim::LBrace))?;
            Some(FuncBody { locals: vec![] })
        } else {
            return Err(format!("expected ; or {{, got {:?}", self.peek_any()));
        };

        Ok(FuncDef {
            name,
            overrides: None,
            flags: FuncFlags::empty(),
            sig,
            body,
        })
    }

    fn parse_state(&mut self) -> Result<(), String> {
        // TODO
        self.expect_ident()?;
        self.expect(Tk::Open(Delim::LBrace))?;
        self.ignore_foreign_block(Tk::Open(Delim::LBrace))?;

        Ok(())
    }

    fn parse_delegate(&mut self) -> Result<DelegateDef<Identifier>, String> {
        let (name, sig) = self.parse_function_sig(false)?;
        self.expect(Tk::Semi)?;

        Ok(DelegateDef { name, sig })
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
                        kind: Tk::Open(Delim::LParen),
                        ..
                    }) => {
                        if mods.intersects(ModifierCount::ALLOW_PAREN) {
                            self.next();
                            let mut comma = false;
                            loop {
                                if self.eat(Tk::Close(Delim::RParen)) {
                                    break Ok(());
                                }
                                if comma {
                                    self.expect(Tk::Comma)?;
                                }
                                match followups {
                                    DeclFollowups::IdentModifiers(_) => {
                                        self.expect_ident()?;
                                    }
                                    &DeclFollowups::NumberModifiers(_) => {
                                        self.expect_number()?;
                                    }
                                    _ => unreachable!("checked in outer match"),
                                }
                                comma = true;
                            }
                        } else {
                            Ok(())
                        }
                    }
                    t @ (Some(_) | None) => {
                        if mods.contains(ModifierCount::ALLOW_NONE) {
                            Ok(())
                        } else {
                            Err(format!("missing followups: {:?}, got {:?}", followups, t))
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
                    Tk::Kw(kw) => match mods.get(kw) {
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
            break match (self.next(), self.peek()) {
                (
                    Some(Token {
                        kind: Tk::Kw(Keyword::Simulated),
                        ..
                    }),
                    Some(Token {
                        kind: Tk::Kw(Keyword::State),
                        ..
                    }),
                ) => {
                    // hardcoded exception for simulated state
                    self.next();
                    self.parse_state()?;
                    continue;
                }
                (Some(tok), _) => match tok.kind {
                    Tk::Kw(Keyword::Const) => Ok(Some(TopLevelItem::Const(self.parse_const()?))),
                    Tk::Kw(Keyword::Var) => Ok(Some(TopLevelItem::Var(self.parse_var()?))),
                    Tk::Kw(Keyword::Enum) => Ok(Some(TopLevelItem::Enum(self.parse_enum()?))),
                    Tk::Kw(Keyword::Struct) => Ok(Some(TopLevelItem::Struct(self.parse_struct()?))),
                    Tk::Kw(Keyword::Delegate) => {
                        Ok(Some(TopLevelItem::Delegate(self.parse_delegate()?)))
                    }
                    Tk::Kw(Keyword::CppText | Keyword::DefaultProperties) => {
                        let brace = self.expect(Tk::Open(Delim::LBrace))?;
                        self.ignore_foreign_block(brace.kind)?;
                        continue;
                    }
                    Tk::Kw(Keyword::State) => {
                        self.parse_state()?;
                        continue;
                    }
                    Tk::Kw(kw) if modifiers::FUNC_MODIFIERS.contains(kw) => {
                        Ok(Some(TopLevelItem::Func(self.parse_function(tok)?)))
                    }
                    Tk::Directive => {
                        self.ignore_directive()?;
                        continue;
                    }
                    Tk::Semi => continue,
                    Tk::Comment => unreachable!("filtered out in next"),
                    _ => panic!("Unknown start of item: {:?}", tok),
                },
                (None, _) => Ok(None),
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

pub fn parse(lex: Lexer) -> (Hir<Identifier>, Vec<String>) {
    let mut parser = Parser::new(lex);
    let header = parser.parse_class_def().unwrap();
    let items = parser.parse_items();

    let mut structs = vec![];
    let mut enums = vec![];
    let mut consts = vec![];
    let mut vars = vec![];
    let mut delegate_defs = vec![];
    let mut funcs = vec![];

    for i in items {
        match i {
            TopLevelItem::Const(c) => consts.push(c),
            TopLevelItem::Var(v) => vars.push(v),
            TopLevelItem::Struct(s) => structs.push(s),
            TopLevelItem::Enum(e) => enums.push(e),
            TopLevelItem::Delegate(d) => delegate_defs.push(d),
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
            dels: delegate_defs,
            funcs,
        },
        parser.errs,
    )
}
