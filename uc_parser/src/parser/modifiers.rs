use std::collections::HashMap;

use bitflags::bitflags;
use once_cell::sync::Lazy;

use crate::lexer::Keyword as Kw;

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
    IdentModifiers(MC),
    /// `native(129)`
    NumberModifiers(MC),
}

type DF = DeclFollowups;
type MC = ModifierCount;

#[derive(Clone, Debug)]
pub struct ModifierConfig {
    modifiers: HashMap<Kw, DF>,
}

impl ModifierConfig {
    pub fn contains(&self, kw: Kw) -> bool {
        self.modifiers.contains_key(&kw)
    }

    pub fn get(&self, kw: Kw) -> Option<&DF> {
        self.modifiers.get(&kw)
    }
}

pub static CLASS_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(
        Kw::Native,
        DF::IdentModifiers(MC::ALLOW_NONE | MC::ALLOW_ONE),
    );

    modifiers.insert(Kw::Config, DF::IdentModifiers(MC::ALLOW_ONE));
    modifiers.insert(Kw::PerObjectConfig, DF::Nothing);
    modifiers.insert(Kw::Implements, DF::IdentModifiers(MC::ALLOW_MULTIPLE));

    modifiers.insert(Kw::Abstract, DF::Nothing);
    modifiers.insert(Kw::DependsOn, DF::IdentModifiers(MC::ALLOW_MULTIPLE));
    modifiers.insert(Kw::Transient, DF::Nothing);
    modifiers.insert(Kw::Deprecated, DF::Nothing);

    modifiers.insert(Kw::NativeReplication, DF::Nothing);

    modifiers.insert(Kw::NoExport, DF::Nothing);
    modifiers.insert(Kw::CollapseCategories, DF::Nothing);
    modifiers.insert(Kw::HideCategories, DF::IdentModifiers(MC::ALLOW_MULTIPLE));
    modifiers.insert(Kw::EditInlineNew, DF::Nothing);
    modifiers.insert(Kw::NotPlaceable, DF::Nothing);
    modifiers.insert(Kw::Placeable, DF::Nothing);
    modifiers.insert(Kw::ClassGroup, DF::IdentModifiers(MC::ALLOW_ONE));
    modifiers.insert(Kw::Inherits, DF::IdentModifiers(MC::ALLOW_MULTIPLE));
    modifiers.insert(Kw::ShowCategories, DF::IdentModifiers(MC::ALLOW_MULTIPLE));

    ModifierConfig { modifiers }
});

pub static INTERFACE_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(
        Kw::Native,
        DF::IdentModifiers(MC::ALLOW_NONE | MC::ALLOW_ONE),
    );

    modifiers.insert(Kw::DependsOn, DF::IdentModifiers(MC::ALLOW_MULTIPLE));

    ModifierConfig { modifiers }
});

pub static VAR_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(Kw::Native, DF::Nothing);
    modifiers.insert(
        Kw::Config,
        DF::IdentModifiers(MC::ALLOW_NONE | MC::ALLOW_ONE),
    );
    modifiers.insert(Kw::GlobalConfig, DF::Nothing);
    modifiers.insert(Kw::Localized, DF::Nothing);
    modifiers.insert(Kw::Const, DF::Nothing);
    modifiers.insert(Kw::EditConst, DF::Nothing);

    modifiers.insert(Kw::EditInline, DF::Nothing);

    modifiers.insert(Kw::NoExport, DF::Nothing);
    modifiers.insert(Kw::Transient, DF::Nothing);
    modifiers.insert(Kw::DuplicateTransient, DF::Nothing);
    modifiers.insert(Kw::DataBinding, DF::Nothing);
    modifiers.insert(Kw::Init, DF::Nothing);

    modifiers.insert(Kw::Public, DF::OptForeignBlock);
    modifiers.insert(Kw::Private, DF::OptForeignBlock);
    modifiers.insert(Kw::Protected, DF::OptForeignBlock);
    modifiers.insert(Kw::PrivateWrite, DF::OptForeignBlock);
    modifiers.insert(Kw::ProtectedWrite, DF::OptForeignBlock);

    ModifierConfig { modifiers }
});

pub static ARG_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(Kw::Coerce, DF::Nothing);
    modifiers.insert(Kw::Const, DF::Nothing);
    modifiers.insert(Kw::Optional, DF::Nothing);
    modifiers.insert(Kw::Skip, DF::Nothing);
    modifiers.insert(Kw::Out, DF::Nothing);
    modifiers.insert(Kw::Ref, DF::Nothing);

    ModifierConfig { modifiers }
});

pub static STRUCT_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(Kw::Native, DF::Nothing);
    modifiers.insert(Kw::Immutable, DF::Nothing);
    modifiers.insert(Kw::Transient, DF::Nothing);

    ModifierConfig { modifiers }
});

pub static FUNC_MODIFIERS: Lazy<ModifierConfig> = Lazy::new(|| {
    let mut modifiers = HashMap::new();

    modifiers.insert(Kw::Function, DF::Nothing);
    modifiers.insert(Kw::Event, DF::Nothing);
    modifiers.insert(
        Kw::Native,
        DF::NumberModifiers(MC::ALLOW_NONE | MC::ALLOW_ONE),
    );
    modifiers.insert(
        Kw::Operator,
        DF::NumberModifiers(MC::ALLOW_NONE | MC::ALLOW_ONE),
    );
    modifiers.insert(Kw::PreOperator, DF::Nothing);
    modifiers.insert(Kw::PostOperator, DF::Nothing);

    modifiers.insert(Kw::Static, DF::Nothing);
    modifiers.insert(Kw::Final, DF::Nothing);
    modifiers.insert(Kw::Exec, DF::Nothing);

    modifiers.insert(Kw::Simulated, DF::Nothing);
    modifiers.insert(Kw::Reliable, DF::Nothing);
    modifiers.insert(Kw::Client, DF::Nothing);
    modifiers.insert(Kw::Server, DF::Nothing);

    modifiers.insert(Kw::NoExport, DF::Nothing);

    modifiers.insert(Kw::Public, DF::Nothing);
    modifiers.insert(Kw::Private, DF::Nothing);
    modifiers.insert(Kw::Protected, DF::Nothing);

    modifiers.insert(Kw::Coerce, DF::Nothing);

    ModifierConfig { modifiers }
});
