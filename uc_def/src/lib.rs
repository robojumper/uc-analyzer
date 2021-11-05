use std::{fmt::Display, str::FromStr};

use bitflags::bitflags;

pub mod pretty;

bitflags! {
    pub struct ClassFlags: u32 {
        const NATIVE = 1 << 0;
        const CONFIG = 1 << 1;
    }

    pub struct InterfaceFlags: u32 {
        const NATIVE = 1 << 0;
    }

    pub struct PropFlags: u32 {
        const LOCALIZED = 1 << 0;
        const CONFIG = 1 << 1;
    }

    pub struct FuncFlags: u32 {
        const EVENT = 1 << 0;
        const SIMULATED = 1 << 1;
        const NATIVE = 1 << 2;
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(unicase::Ascii<String>);

impl FromStr for Identifier {
    type Err = <unicase::Ascii<String> as FromStr>::Err;
    fn from_str(t: &str) -> Result<Self, Self::Err> {
        Ok(Self(unicase::Ascii::from_str(t)?))
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.0.as_str(), f)
    }
}

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.0.as_str(), f)
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

#[derive(Debug)]
pub struct Hir<T> {
    pub header: ClassDef<T>,
    pub structs: Vec<StructDef<T>>,
    pub enums: Vec<EnumDef>,
    pub consts: Vec<ConstDef>,
    pub vars: Vec<VarDef<T>>,
    pub dels: Vec<DelegateDef<T>>,
    pub funcs: Vec<FuncDef<T>>,
}

#[derive(Debug)]
pub enum ClassHeader<T> {
    Class {
        extends: Option<T>,
        implements: Vec<T>,
        within: Option<T>,
        flags: ClassFlags,
    },
    Interface {
        extends: Option<T>,
    },
}

#[derive(Debug)]
pub struct ClassDef<T> {
    pub name: Identifier,
    pub kind: ClassHeader<T>,
}

#[derive(Debug)]
pub enum DimCount<T> {
    None,
    Number(u32),
    Complex(Vec<T>),
}

#[derive(Debug)]
pub enum Ty<T> {
    Simple(T),
    Array(T),
    Class(Option<T>),
    Delegate(Vec<T>),
}

#[derive(Debug)]
pub struct ConstDef {
    pub name: Identifier,
    pub val: ConstVal,
}

#[derive(Debug)]
pub enum ConstVal {
    Bool,
    Int,
    String,
    Name,
    Float,
    /// Enum value? Other const?
    ValueReference,
}

#[derive(Debug)]
pub struct VarInstance<T> {
    pub name: Identifier,
    pub count: DimCount<T>,
}

#[derive(Debug)]
pub struct VarDef<T> {
    pub ty: Ty<T>,
    pub names: Vec<VarInstance<T>>,
    pub flags: PropFlags,
}

#[derive(Debug)]
pub struct EnumDef {
    pub name: Identifier,
    pub variants: Vec<Identifier>,
}

#[derive(Debug)]
pub struct StructDef<T> {
    pub name: Identifier,
    pub extends: Option<T>,
    pub fields: Vec<VarDef<T>>,
}

#[derive(Debug)]
pub struct DelegateDef<T> {
    pub name: Identifier,
    pub sig: FuncSig<T>,
}

#[derive(Debug)]
pub struct FuncDef<T> {
    pub name: Identifier,
    pub overrides: Option<T>,
    pub flags: FuncFlags,
    pub sig: FuncSig<T>,
    pub body: Option<FuncBody<T>>,
}

#[derive(Debug)]
pub struct FuncSig<T> {
    pub ret_ty: Option<Ty<T>>,
    pub args: Vec<FuncArg<T>>,
}

#[derive(Debug)]
pub struct FuncArg<T> {
    pub ty: Ty<T>,
    pub name: Identifier,
    pub val: Option<ConstVal>,
}

#[derive(Debug)]
pub struct FuncBody<T> {
    pub locals: Vec<Local<T>>,
}

#[derive(Debug)]
pub struct Local<T> {
    pub ty: Option<T>,
    pub name: Identifier,
}
