//! The middle representation of a whole UnrealScript workspace.
//! This is where we can start doing name resolution, as `uc_parser`
//! and `uc_ast` intentionally only operate on single source files.

use std::{collections::HashMap, num::NonZeroU32};

use uc_def::{ClassFlags, InterfaceFlags};
use uc_name::Identifier;
use uc_types::Ty;

pub struct DefId(NonZeroU32);

pub struct GlobalNamespace {
    pub packages: HashMap<Identifier, DefId>,
    pub classes: HashMap<Identifier, DefId>,
    pub tys: HashMap<Identifier, Ty>,
    pub enum_vals: HashMap<Identifier, (DefId, DefId)>,
}

pub struct GlobalContext {
    pub namespace: GlobalNamespace,
    pub defs: Vec<Def>,
}

pub struct Package {
    pub name: Identifier,
    pub classes: HashMap<Identifier, DefId>,
}

pub struct Class {
    pub def_id: DefId,
    pub name: Identifier,
    pub extends: Option<DefId>,
    pub implements: Box<[DefId]>,
    pub package: DefId,

    pub self_ty: Ty,

    pub vars: HashMap<Identifier, DefId>,
    pub structs: HashMap<Identifier, DefId>,
    pub enums: HashMap<Identifier, DefId>,
    pub consts: HashMap<Identifier, DefId>,
    pub funcs: HashMap<Identifier, DefId>,

    pub flags: ClassFlags,
}

pub struct Interface {
    pub def_id: DefId,
    pub name: Identifier,
    pub extends: Option<DefId>,
    pub flags: InterfaceFlags,
}

pub enum Def {
    Package(Box<Package>),
    Class(Box<Class>),
    Interface(Box<Interface>),
    Enum(Box<Enum>),
}

pub struct Enum {
    pub def_id: DefId,
    pub name: Identifier,
    pub variants: HashMap<Identifier, DefId>,
}
