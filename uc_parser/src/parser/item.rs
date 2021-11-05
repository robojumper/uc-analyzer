use std::str::FromStr;

use uc_def::{
    ClassDef, ClassFlags, ClassHeader, ConstDef, ConstVal, DelegateDef, DimCount, EnumDef, FuncArg,
    FuncBody, FuncDef, FuncFlags, FuncSig, Identifier, Local, PropFlags, StructDef, VarDef,
    VarInstance,
};

use super::{
    modifiers::{DeclFollowups, ModifierConfig, ModifierCount},
    Parser,
};
use crate::{
    kw,
    lexer::{Delim, NumberSyntax, Sigil, Symbol, Token, TokenKind as Tk},
    parser::modifiers,
    NumberLiteral,
};

#[derive(Debug)]
pub enum TopLevelItem {
    Const(ConstDef),
    Var(VarDef<Identifier>),
    Struct(StructDef<Identifier>),
    Enum(EnumDef),
    Delegate(DelegateDef<Identifier>),
    Func(FuncDef<Identifier>),
}

impl Parser<'_> {
    pub fn parse_class_def(&mut self) -> Result<ClassDef<Identifier>, String> {
        let token = self.next_any()?;
        let class = match token.kind {
            kw!(Class) => true,
            kw!(Interface) => false,
            _ => return Err(self.fmt_unexpected(&token)),
        };

        let name = self.expect_ident()?;

        let def = if class {
            let extends = if self.eat(kw!(Extends)) {
                let extends_name = self.expect_ident()?;
                Some(extends_name)
            } else {
                None
            };

            let within = if self.eat(kw!(Within)) {
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
            let extends = if self.eat(kw!(Extends)) {
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
            Tk::Sym(_) => Ok(ConstVal::ValueReference),
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

    fn parse_var(&mut self) -> Result<VarDef<Identifier>, String> {
        if self.eat(Tk::Open(Delim::LParen)) {
            self.eat_symbol();
            self.expect(Tk::Close(Delim::RParen))?;
        }

        self.ignore_kws(&*modifiers::VAR_MODIFIERS)?;
        let ty = self.parse_ty(None)?;

        let mut names = vec![];

        loop {
            let var_name = self.expect_ident()?;
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
                    Tk::Sym(_) => {
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
            if let Tk::Sym(_) = tok.kind {
                variants.push(self.sym_to_ident(&tok));
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

    fn parse_struct(&mut self) -> Result<StructDef<Identifier>, String> {
        if self.eat(Tk::Open(Delim::LBrace)) {
            self.ignore_foreign_block(Tk::Open(Delim::LBrace))?;
        }
        self.ignore_kws(&*modifiers::STRUCT_MODIFIERS)?;
        let name = self.expect_ident()?;

        let extends = if self.eat(kw!(Extends)) {
            Some(self.expect_ident()?)
        } else {
            None
        };

        self.expect(Tk::Open(Delim::LBrace))?;

        let mut fields = vec![];

        loop {
            match self.next_any()?.kind {
                Tk::Close(Delim::RBrace) => break,
                kw!(Var) => {
                    fields.push(self.parse_var()?);
                }
                kw!(StructCppText) | kw!(StructDefaultProperties) => {
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
            (Tk::Sym(_), Tk::Open(Delim::LParen)) => (None, self.sym_to_ident(&ty_or_name)),
            _ => (Some(self.parse_ty(Some(ty_or_name))?), {
                let name_tok = self.next_any()?;
                match name_tok.kind {
                    Tk::Sym(_) => self.sym_to_ident(&name_tok),
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
            let name = self.expect_ident()?;

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
        let Tk::Sym(Symbol::Kw(kw)) = first.kind else { panic!("parse_function needs the first function kw") };
        let followups = modifiers::FUNC_MODIFIERS
            .get(kw)
            .expect("not a valid function keyword");
        self.ignore_followups(followups)?;
        self.ignore_kws(&*modifiers::FUNC_MODIFIERS)?;

        let (name, sig) = self.parse_function_sig(true)?;

        self.eat(kw!(Const));

        let body = if self.eat(Tk::Semi) {
            None
        } else if self.eat(Tk::Open(Delim::LBrace)) {
            let locals = self.parse_locals()?;
            self.ignore_foreign_block(Tk::Open(Delim::LBrace))?;
            Some(FuncBody {
                locals,
                statements: vec![],
            })
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

    fn parse_locals(&mut self) -> Result<Vec<Local<Identifier>>, String> {
        let mut locals = vec![];
        while self.eat(kw!(Local)) {
            match self.parse_local() {
                Ok(l) => locals.push(l),
                Err(e) => self.errs.push(e),
            }
        }
        Ok(locals)
    }

    fn parse_local(&mut self) -> Result<Local<Identifier>, String> {
        let ty = self.parse_ty(None)?;
        let mut names = vec![];
        loop {
            let var_name = self.expect_ident()?;
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
                    Tk::Sym(_) => {
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

            names.push(VarInstance {
                name: var_name,
                count,
            });

            if !self.eat(Tk::Comma) {
                break;
            }
        }
        Ok(Local { ty, names })
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
                    Tk::Sym(Symbol::Kw(kw)) => match mods.get(kw) {
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
                        kind: kw!(Simulated),
                        ..
                    }),
                    Some(Token {
                        kind: kw!(State), ..
                    }),
                ) => {
                    // hardcoded exception for simulated state
                    self.next();
                    self.parse_state()?;
                    continue;
                }
                (Some(tok), _) => match tok.kind {
                    kw!(Const) => Ok(Some(TopLevelItem::Const(self.parse_const()?))),
                    kw!(Var) => Ok(Some(TopLevelItem::Var(self.parse_var()?))),
                    kw!(Enum) => Ok(Some(TopLevelItem::Enum(self.parse_enum()?))),
                    kw!(Struct) => Ok(Some(TopLevelItem::Struct(self.parse_struct()?))),
                    kw!(Delegate) => Ok(Some(TopLevelItem::Delegate(self.parse_delegate()?))),
                    kw!(CppText) | kw!(DefaultProperties) | kw!(Replication) => {
                        let brace = self.expect(Tk::Open(Delim::LBrace))?;
                        self.ignore_foreign_block(brace.kind)?;
                        continue;
                    }
                    kw!(State) => {
                        self.parse_state()?;
                        continue;
                    }
                    Tk::Sym(Symbol::Kw(kw)) if modifiers::FUNC_MODIFIERS.contains(kw) => {
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

    pub fn parse_items(&mut self) -> Vec<TopLevelItem> {
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
