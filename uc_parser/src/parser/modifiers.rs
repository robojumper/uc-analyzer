use std::collections::HashMap;

use bitflags::bitflags;
use once_cell::sync::Lazy;
use uc_def::{ArgFlags, ClassFlags, FuncFlags, Identifier, InterfaceFlags, StructFlags, VarFlags};

use crate::lexer::{Delim, Keyword as Kw, Symbol, Token, TokenKind as Tk};

use super::Parser;

pub trait Flags: Copy {
    fn into_raw(self) -> u32;
    fn from_raw(bits: u32) -> Self;
}

macro_rules! impl_flags_for_bitflags {
    ($($t:ty),+ $(,)?) => {
        $(
            impl Flags for $t {
                fn into_raw(self) -> u32 {
                    self.bits()
                }

                fn from_raw(bits: u32) -> Self {
                    let ret = <$t>::from_bits_truncate(bits);
                    assert_eq!(bits, ret.bits());
                    ret
                }
            }
        )+
    }
}

impl_flags_for_bitflags! {
    InterfaceFlags, ClassFlags, FuncFlags, VarFlags, ArgFlags, StructFlags,
}

bitflags! {
    pub struct ModifierCount: u32 {
        const ALLOW_NONE = 1 << 0;
        const ALLOW_EMPTY = 1 << 1;
        const ALLOW_ONE = 1 << 2;
        const ALLOW_TWO_OR_MORE = 1 << 3;

        const ALLOW_PAREN = Self::ALLOW_NONE.bits | Self::ALLOW_ONE.bits | Self::ALLOW_MULTIPLE.bits;
        const ALLOW_MULTIPLE = Self::ALLOW_ONE.bits | Self::ALLOW_TWO_OR_MORE.bits;
    }
}

#[derive(Clone, Copy, Debug)]
pub enum DeclFollowups {
    /// `var const`
    Nothing,
    /// `var private{private}`
    OptForeignBlock,
    /// `var(Category)`
    IdentModifiers(ModifierCount),
    /// `native(129)`
    NumberModifiers(ModifierCount),
}

type DF = DeclFollowups;
type MC = ModifierCount;
type C<F> = KeywordConfig<F>;

const ONE_IDENT: DeclFollowups = DeclFollowups::IdentModifiers(ModifierCount::ALLOW_ONE);
const MORE_IDENTS: DeclFollowups = DeclFollowups::IdentModifiers(ModifierCount::ALLOW_MULTIPLE);

#[derive(Debug)]
struct KeywordConfig<F: Flags> {
    flag: F,
    foll: DeclFollowups,
}

impl<F: Flags> KeywordConfig<F> {
    fn new(flag: F, foll: DeclFollowups) -> Self {
        KeywordConfig { flag, foll }
    }
}

#[derive(Debug)]
pub struct ModifierConfig<F: Flags> {
    modifiers: HashMap<Kw, KeywordConfig<F>>,
}

impl<F: Flags> ModifierConfig<F> {
    pub fn contains(&self, kw: Kw) -> bool {
        self.modifiers.contains_key(&kw)
    }

    fn get(&self, kw: Kw) -> Option<&KeywordConfig<F>> {
        self.modifiers.get(&kw)
    }
}

#[derive(Debug)]
pub enum Values {
    Absent,
    Nums(Box<[i32]>),
    Idents(Box<[Identifier]>),
}

#[derive(Debug)]
pub struct ParseResult<F: Flags> {
    pub flags: F,
    pub followups: HashMap<Kw, Option<Values>>,
}

impl Parser<'_> {
    fn parse_list<T, F: Fn(&mut Parser) -> Result<T, String>>(
        &mut self,
        f: F,
    ) -> Result<Box<[T]>, String> {
        let mut list = vec![];
        self.next();
        let mut comma = false;
        loop {
            if self.eat(Tk::Close(Delim::RParen)) {
                break Ok(list.into_boxed_slice());
            }
            if comma {
                self.expect(Tk::Comma)?;
            }
            list.push(f(self)?);
            comma = true;
        }
    }

    pub fn parse_followups(&mut self, followups: &DeclFollowups) -> Result<Option<Values>, String> {
        match followups {
            DeclFollowups::Nothing => Ok(None),
            DeclFollowups::OptForeignBlock => match self.peek() {
                Some(Token {
                    kind: opener @ Tk::Open(Delim::LParen),
                    ..
                }) => {
                    self.next();
                    self.ignore_foreign_block(opener)?;
                    Ok(None)
                }
                Some(_) | None => Ok(None),
            },
            DeclFollowups::IdentModifiers(mods) | DeclFollowups::NumberModifiers(mods) => {
                match self.peek() {
                    Some(Token {
                        kind: Tk::Open(Delim::LParen),
                        ..
                    }) => {
                        if mods.intersects(ModifierCount::ALLOW_PAREN) {
                            match followups {
                                DeclFollowups::IdentModifiers(_) => self
                                    .parse_list(|p| p.expect_ident())
                                    .map(|l| Some(Values::Idents(l))),
                                DeclFollowups::NumberModifiers(_) => self
                                    .parse_list(|p| p.expect_number()?.expect_int())
                                    .map(|l| Some(Values::Nums(l))),
                                _ => unreachable!("checked in outer match"),
                            }
                        } else {
                            Ok(None)
                        }
                    }
                    t @ (Some(_) | None) => {
                        if mods.contains(ModifierCount::ALLOW_NONE) {
                            Ok(Some(Values::Absent))
                        } else {
                            Err(format!("missing followups: {:?}, got {:?}", followups, t))
                        }
                    }
                }
            }
        }
    }

    pub fn parse_kws<F: Flags>(
        &mut self,
        mods: &ModifierConfig<F>,
    ) -> Result<ParseResult<F>, String> {
        let mut flags = 0u32;
        let mut followups = HashMap::new();
        loop {
            let kw_or_next = self.peek();
            match &kw_or_next {
                Some(tok) => match tok.kind {
                    Tk::Sym(Symbol::Kw(kw)) => match mods.get(kw) {
                        Some(config) => {
                            self.next();
                            let f = config.flag.into_raw();
                            let vals = self.parse_followups(&config.foll)?;
                            if f != 0u32 {
                                followups.insert(kw, vals);
                            }
                            flags |= f;
                        }
                        None => break,
                    },
                    _ => break,
                },
                None => break,
            }
        }

        Ok(ParseResult {
            flags: F::from_raw(flags),
            followups,
        })
    }
}

pub static CLASS_MODIFIERS: Lazy<ModifierConfig<ClassFlags>> = Lazy::new(|| {
    let mut m = HashMap::new();
    type CF = ClassFlags;
    let e = ClassFlags::empty();

    m.insert(
        Kw::Native,
        C::new(
            CF::NATIVE,
            DF::IdentModifiers(MC::ALLOW_NONE | MC::ALLOW_ONE),
        ),
    );

    m.insert(Kw::Config, C::new(CF::CONFIG, ONE_IDENT));

    m.insert(
        Kw::PerObjectConfig,
        C::new(CF::PEROBJECTCONFIG, DF::Nothing),
    );
    m.insert(Kw::Implements, C::new(CF::IMPLEMENTS, MORE_IDENTS));

    m.insert(Kw::Abstract, C::new(CF::ABSTRACT, DF::Nothing));
    m.insert(Kw::DependsOn, C::new(e, MORE_IDENTS));
    m.insert(Kw::Transient, C::new(e, DF::Nothing));
    m.insert(Kw::Deprecated, C::new(e, DF::Nothing));

    m.insert(Kw::NativeReplication, C::new(e, DF::Nothing));

    m.insert(Kw::NoExport, C::new(e, DF::Nothing));
    m.insert(Kw::CollapseCategories, C::new(e, DF::Nothing));
    m.insert(Kw::HideCategories, C::new(e, MORE_IDENTS));
    m.insert(Kw::EditInlineNew, C::new(e, DF::Nothing));
    m.insert(Kw::NotPlaceable, C::new(e, DF::Nothing));
    m.insert(Kw::Placeable, C::new(e, DF::Nothing));
    m.insert(Kw::ClassGroup, C::new(e, ONE_IDENT));
    m.insert(Kw::Inherits, C::new(e, MORE_IDENTS));
    m.insert(Kw::ShowCategories, C::new(e, MORE_IDENTS));

    ModifierConfig { modifiers: m }
});

pub static INTERFACE_MODIFIERS: Lazy<ModifierConfig<InterfaceFlags>> = Lazy::new(|| {
    let mut m = HashMap::new();
    type IF = InterfaceFlags;
    let e = InterfaceFlags::empty();

    m.insert(
        Kw::Native,
        C::new(
            IF::NATIVE,
            DF::IdentModifiers(MC::ALLOW_NONE | MC::ALLOW_ONE),
        ),
    );

    m.insert(Kw::DependsOn, C::new(e, MORE_IDENTS));

    ModifierConfig { modifiers: m }
});

pub static VAR_MODIFIERS: Lazy<ModifierConfig<VarFlags>> = Lazy::new(|| {
    let mut m = HashMap::new();
    type VF = VarFlags;
    let e = VarFlags::empty();

    m.insert(Kw::Native, C::new(VF::NATIVE, DF::Nothing));
    m.insert(Kw::Config, C::new(VF::CONFIG, DF::Nothing));
    m.insert(Kw::GlobalConfig, C::new(VF::GLOBALCONFIG, DF::Nothing));
    m.insert(Kw::Localized, C::new(VF::LOCALIZED, DF::Nothing));
    m.insert(Kw::Const, C::new(VF::CONST, DF::Nothing));

    m.insert(Kw::EditConst, C::new(e, DF::Nothing));
    m.insert(Kw::EditInline, C::new(e, DF::Nothing));
    m.insert(Kw::NoExport, C::new(e, DF::Nothing));
    m.insert(Kw::Transient, C::new(e, DF::Nothing));
    m.insert(Kw::DuplicateTransient, C::new(e, DF::Nothing));
    m.insert(Kw::DataBinding, C::new(e, DF::Nothing));
    m.insert(Kw::Init, C::new(e, DF::Nothing));
    m.insert(Kw::RepNotify, C::new(e, DF::Nothing));
    m.insert(Kw::Input, C::new(e, DF::Nothing));

    m.insert(Kw::Public, C::new(VF::PUBLIC, DF::OptForeignBlock));
    m.insert(Kw::Private, C::new(VF::PRIVATE, DF::OptForeignBlock));
    m.insert(
        Kw::PrivateWrite,
        C::new(VF::PRIVATEWRITE, DF::OptForeignBlock),
    );
    m.insert(Kw::Protected, C::new(VF::PROTECTED, DF::OptForeignBlock));
    m.insert(
        Kw::ProtectedWrite,
        C::new(VF::PROTECTEDWRITE, DF::OptForeignBlock),
    );

    ModifierConfig { modifiers: m }
});

pub static ARG_MODIFIERS: Lazy<ModifierConfig<ArgFlags>> = Lazy::new(|| {
    let mut m = HashMap::new();
    let e = ArgFlags::empty();
    type AF = ArgFlags;

    m.insert(Kw::Coerce, C::new(AF::COERCE, DF::Nothing));
    m.insert(Kw::Const, C::new(AF::CONST, DF::Nothing));
    m.insert(Kw::Optional, C::new(AF::OPTIONAL, DF::Nothing));
    m.insert(Kw::Skip, C::new(AF::SKIP, DF::Nothing));
    m.insert(Kw::Out, C::new(AF::OUT, DF::Nothing));
    m.insert(Kw::Ref, C::new(AF::REF, DF::Nothing));

    ModifierConfig { modifiers: m }
});

pub static STRUCT_MODIFIERS: Lazy<ModifierConfig<StructFlags>> = Lazy::new(|| {
    let mut m = HashMap::new();
    let e = StructFlags::empty();
    type SF = StructFlags;

    m.insert(Kw::Native, C::new(e, DF::Nothing));
    m.insert(Kw::Immutable, C::new(e, DF::Nothing));
    m.insert(Kw::Transient, C::new(e, DF::Nothing));

    ModifierConfig { modifiers: m }
});

pub static FUNC_MODIFIERS: Lazy<ModifierConfig<FuncFlags>> = Lazy::new(|| {
    let mut m = HashMap::new();
    let e = FuncFlags::empty();
    type FF = FuncFlags;

    m.insert(Kw::Function, C::new(e, DF::Nothing));
    m.insert(Kw::Event, C::new(FF::EVENT, DF::Nothing));
    m.insert(
        Kw::Native,
        C::new(
            FF::NATIVE,
            DF::NumberModifiers(MC::ALLOW_NONE | MC::ALLOW_ONE),
        ),
    );
    m.insert(
        Kw::Operator,
        C::new(
            FF::OPERATOR,
            DF::NumberModifiers(MC::ALLOW_NONE | MC::ALLOW_ONE),
        ),
    );
    m.insert(Kw::PreOperator, C::new(FF::PREOPERATOR, DF::Nothing));
    m.insert(Kw::PostOperator, C::new(FF::POSTOPERATOR, DF::Nothing));

    m.insert(Kw::Static, C::new(FF::STATIC, DF::Nothing));
    m.insert(Kw::Final, C::new(FF::FINAL, DF::Nothing));
    m.insert(Kw::Exec, C::new(FF::EXEC, DF::Nothing));
    m.insert(Kw::Latent, C::new(FF::LATENT, DF::Nothing));

    m.insert(Kw::Simulated, C::new(e, DF::Nothing));
    m.insert(Kw::Reliable, C::new(e, DF::Nothing));
    m.insert(Kw::Client, C::new(e, DF::Nothing));
    m.insert(Kw::Server, C::new(e, DF::Nothing));

    m.insert(Kw::NoExport, C::new(e, DF::Nothing));

    m.insert(Kw::Public, C::new(FF::PUBLIC, DF::Nothing));
    m.insert(Kw::Private, C::new(FF::PRIVATE, DF::Nothing));
    m.insert(Kw::Protected, C::new(FF::PROTECTED, DF::Nothing));

    m.insert(Kw::Coerce, C::new(FF::COERCE, DF::Nothing));

    ModifierConfig { modifiers: m }
});
