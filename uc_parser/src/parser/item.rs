use uc_def::{
    ClassDef, ClassFlags, ClassHeader, ConstDef, ConstVal, DelegateDef, DimCount, EnumDef, FuncArg,
    FuncBody, FuncDef, FuncName, FuncSig, Identifier, Local, Op, StructDef, VarDef, VarInstance,
};

use super::Parser;
use crate::{
    kw,
    lexer::{NumberSyntax, Symbol, Token, TokenKind as Tk},
    parser::modifiers,
    sig,
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

        let (def, mods) = if class {
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

            let mods = self.parse_kws(&*modifiers::CLASS_MODIFIERS)?;

            self.expect(Tk::Semi)?;

            (
                ClassHeader::Class {
                    extends,
                    within,
                    implements: vec![],
                    flags: ClassFlags::empty(),
                },
                mods,
            )
        } else {
            let extends = if self.eat(kw!(Extends)) {
                Some(self.expect_ident()?)
            } else {
                None
            };

            let mods = self.parse_kws(&*modifiers::INTERFACE_MODIFIERS)?;
            self.expect(Tk::Semi)?;

            (ClassHeader::Interface { extends }, mods)
        };

        Ok(ClassDef {
            kind: def,
            name,
            mods,
        })
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
            sig!(Sub) => {
                let next = self.next_any()?;
                match next.kind {
                    Tk::Number(NumberSyntax::Int | NumberSyntax::Hex) => Ok(ConstVal::Int),
                    Tk::Number(NumberSyntax::Float) => Ok(ConstVal::Float),
                    _ => Err(format!("expected number after -, got {:?}", tok)),
                }
            }
            _ => Err(format!("expected const value, got {:?}", tok)),
        }
    }

    fn parse_const(&mut self) -> Result<ConstDef, String> {
        self.expect(kw!(Const))?;
        let name = self.expect_ident()?;
        self.expect(sig!(Eq))?;
        let val = self.parse_const_val()?;
        self.expect(Tk::Semi)?;

        Ok(ConstDef { name, val })
    }

    fn parse_var(&mut self) -> Result<VarDef<Identifier>, String> {
        self.expect(kw!(Var))?;
        if self.eat(sig!(LParen)) {
            self.eat_symbol();
            self.expect(sig!(RParen))?;
        }

        let mods = self.parse_kws(&*modifiers::VAR_MODIFIERS)?;
        let ty = self.parse_ty(None)?;

        let mut names = vec![];

        loop {
            let var_name = self.expect_ident()?;
            let count = if self.eat(sig!(LBrack)) {
                match self.peek_any()?.kind {
                    Tk::Number(_) => {
                        let cnt = self.expect_number()?.expect_int()?;
                        self.expect(sig!(RBrack))?;
                        DimCount::Number(cnt as u32)
                    }
                    Tk::Sym(_) => {
                        let parts = self.parse_parts_until(sig!(RBrack))?;
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
            if self.eat(sig!(LBrace)) {
                self.lex
                    .ignore_foreign_block(sig!(LBrace))
                    .map_err(|e| format!("{:?}", e))?;
            }
            // Editor metadata
            if self.eat(sig!(Lt)) {
                self.lex
                    .ignore_foreign_block(sig!(Lt))
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

        Ok(VarDef { names, ty, mods })
    }

    fn parse_enum(&mut self) -> Result<EnumDef, String> {
        self.expect(kw!(Enum))?;
        let name = self.expect_ident()?;
        self.expect(sig!(LBrace))?;

        let mut variants = vec![];
        let mut comma = false;
        loop {
            if self.eat(sig!(RBrace)) {
                break;
            }
            if comma {
                self.expect(Tk::Comma)?;
            }
            if self.eat(sig!(RBrace)) {
                break;
            }
            let tok = self.next_any()?;
            if let Tk::Sym(_) = tok.kind {
                variants.push(self.sym_to_ident(&tok));
                // Editor metadata
                if self.eat(sig!(Lt)) {
                    self.ignore_foreign_block(sig!(Lt))?;
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
        self.expect(kw!(Struct))?;
        if self.eat(sig!(LBrace)) {
            self.ignore_foreign_block(sig!(LBrace))?;
        }
        let mods = self.parse_kws(&*modifiers::STRUCT_MODIFIERS)?;
        let name = self.expect_ident()?;

        let extends = if self.eat(kw!(Extends)) {
            Some(self.expect_ident()?)
        } else {
            None
        };

        self.expect(sig!(LBrace))?;

        let mut fields = vec![];

        loop {
            match self.peek_any()?.kind {
                sig!(RBrace) => {
                    self.next();
                    break;
                }
                kw!(Var) => {
                    fields.push(self.parse_var()?);
                }
                kw!(StructCppText) | kw!(StructDefaultProperties) => {
                    self.next();
                    let opener = self.expect(sig!(LBrace))?;
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
            mods,
        })
    }

    fn parse_function_sig(
        &mut self,
        allow_op_sigil: bool,
    ) -> Result<(FuncName, FuncSig<Identifier>), String> {
        let ty_or_name = self.next_any()?;
        let (ret_ty, name) = match (ty_or_name.kind, self.peek_any()?.kind) {
            (Tk::Sym(_), sig!(LParen)) => (None, FuncName::Iden(self.sym_to_ident(&ty_or_name))),
            _ => (Some(self.parse_ty(Some(ty_or_name))?), {
                let name_tok = self.next_any()?;
                match name_tok.kind {
                    kw!(Cross) => FuncName::Oper(Op::VecCross),
                    kw!(Dot) => FuncName::Oper(Op::VecDot),
                    Tk::Sym(_) => FuncName::Iden(self.sym_to_ident(&name_tok)),
                    Tk::Sig(s) if allow_op_sigil && s.is_overloadable_op() => {
                        FuncName::Oper(s.to_op())
                    }
                    _ => return Err(format!("expected function name, got {:?}", name_tok)),
                }
            }),
        };

        self.expect(sig!(LParen))?;
        let mut comma = false;
        let mut args = vec![];
        loop {
            if self.eat(sig!(RParen)) {
                break;
            }
            if comma {
                self.expect(Tk::Comma)?;
            }

            let mods = self.parse_kws(&*modifiers::ARG_MODIFIERS)?;
            let ty = self.parse_ty(None)?;
            let name = self.expect_ident()?;

            let count = if self.eat(sig!(LBrack)) {
                match self.peek_any()?.kind {
                    Tk::Number(_) => {
                        let cnt = self.expect_number()?.expect_int()?;
                        self.expect(sig!(RBrack))?;
                        DimCount::Number(cnt as u32)
                    }
                    Tk::Sym(_) => {
                        let parts = self.parse_parts_until(sig!(RBrack))?;
                        DimCount::Complex(parts)
                    }
                    kind => {
                        return Err(format!("expected number or identifier, got {:?}", kind));
                    }
                }
            } else {
                DimCount::None
            };

            let def = if self.eat(sig!(Eq)) {
                Some(self.parse_base_expression()?)
            } else {
                None
            };

            args.push(FuncArg {
                ty,
                name,
                count,
                def,
                mods,
            });

            comma = true;
        }
        Ok((name, FuncSig { ret_ty, args }))
    }

    fn parse_function(&mut self) -> Result<FuncDef<Identifier>, String> {
        let mods = self.parse_kws(&*modifiers::FUNC_MODIFIERS)?;

        let (name, sig) = self.parse_function_sig(true)?;

        self.eat(kw!(Const));

        let body = if self.eat(Tk::Semi) {
            None
        } else if self.eat(sig!(LBrace)) {
            let locals = self.parse_locals()?;
            let statements = self.parse_statements();
            self.expect(sig!(RBrace))?;
            Some(FuncBody { locals, statements })
        } else {
            return Err(format!("expected ; or {{, got {:?}", self.peek_any()));
        };

        Ok(FuncDef {
            name,
            overrides: None,
            mods,
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
            let count = if self.eat(sig!(LBrack)) {
                match self.peek_any()?.kind {
                    Tk::Number(_) => {
                        let cnt = self.expect_number()?.expect_int()?;
                        self.expect(sig!(RBrack))?;
                        DimCount::Number(cnt as u32)
                    }
                    Tk::Sym(_) => {
                        let parts = self.parse_parts_until(sig!(RBrack))?;
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
        self.expect(Tk::Semi)?;
        Ok(Local { ty, names })
    }

    fn parse_state(&mut self) -> Result<(), String> {
        // TODO
        self.eat(kw!(Simulated));
        self.eat(kw!(Auto));
        self.expect(kw!(State))?;
        self.expect_ident()?;
        if self.eat(kw!(Extends)) {
            self.expect_ident()?;
        }
        self.expect(sig!(LBrace))?;
        self.ignore_foreign_block(sig!(LBrace))?;

        Ok(())
    }

    fn parse_delegate(&mut self) -> Result<DelegateDef<Identifier>, String> {
        self.expect(kw!(Delegate))?;
        let (name, sig) = self.parse_function_sig(false)?;
        let name = match name {
            FuncName::Oper(o) => return Err(format!("Invalid delegate name: {:?}", o)),
            FuncName::Iden(i) => i,
        };

        let body = if self.eat(Tk::Semi) {
            None
        } else if self.eat(sig!(LBrace)) {
            let locals = self.parse_locals()?;
            let statements = self.parse_statements();
            self.expect(sig!(RBrace))?;
            Some(FuncBody { locals, statements })
        } else {
            return Err(format!("expected ; or {{, got {:?}", self.peek_any()));
        };

        Ok(DelegateDef { name, body, sig })
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

    fn parse_one_item(&mut self) -> Result<Option<TopLevelItem>, String> {
        loop {
            break match self.peek() {
                Some(tok) => match tok.kind {
                    kw!(Const) => Ok(Some(TopLevelItem::Const(self.parse_const()?))),
                    kw!(Var) => Ok(Some(TopLevelItem::Var(self.parse_var()?))),
                    kw!(Enum) => Ok(Some(TopLevelItem::Enum(self.parse_enum()?))),
                    kw!(Struct) => Ok(Some(TopLevelItem::Struct(self.parse_struct()?))),
                    kw!(Delegate) => Ok(Some(TopLevelItem::Delegate(self.parse_delegate()?))),
                    kw!(CppText) | kw!(DefaultProperties) | kw!(Replication) => {
                        self.next();
                        let brace = self.expect(sig!(LBrace))?;
                        self.ignore_foreign_block(brace.kind)?;
                        continue;
                    }
                    kw!(State) | kw!(Auto) => {
                        self.parse_state()?;
                        continue;
                    }
                    kw!(Simulated)
                        if matches!(
                            self.peek2(),
                            Some(Token {
                                kind: kw!(State),
                                ..
                            })
                        ) =>
                    {
                        self.parse_state()?;
                        continue;
                    }
                    Tk::Sym(Symbol::Kw(kw)) if modifiers::FUNC_MODIFIERS.contains(kw) => {
                        Ok(Some(TopLevelItem::Func(self.parse_function()?)))
                    }
                    Tk::Directive => {
                        self.next();
                        self.ignore_directive()?;
                        continue;
                    }
                    Tk::Semi => {
                        self.next();
                        continue;
                    }
                    Tk::Comment => unreachable!("filtered out in next"),
                    _ => panic!(
                        "Unknown start of item: {:?}, buffered errs: {:?}",
                        tok, self.errs
                    ),
                },
                None => Ok(None),
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
