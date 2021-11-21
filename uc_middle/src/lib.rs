#![feature(inline_const)]

//! The middle representation of a whole UnrealScript workspace.

use std::{collections::HashMap, num::NonZeroU32};

use ty::Ty;
use uc_def::ClassFlags;
use uc_name::Identifier;

pub mod ty;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DefId(NonZeroU32);

#[derive(Debug)]
pub struct DefHierarchy {
    pub defs: Vec<Def>,
    pub packages: Box<[DefId]>,
}

impl Default for DefHierarchy {
    fn default() -> Self {
        Self::new()
    }
}

impl DefHierarchy {
    pub fn new() -> Self {
        Self {
            defs: vec![Def::None],
            packages: Box::new([]),
        }
    }

    /// Create an ID and a corresponding hole in the definitions vector.
    /// The hole will be filled by the result of `f`. `f` may access the
    /// passed `DefHierarchy` to insert more IDs, but code must ensure it
    /// dosn't try to access def IDs that haven't been filled yet.
    pub fn add_def<T, F: FnOnce(&mut Self, T, DefId) -> Def>(&mut self, t: T, f: F) -> DefId {
        let next_id = self.defs.len();
        if next_id > ty::MAX_DEF_ID as usize {
            panic!("too many definitions");
        }
        self.defs.push(Def::None);
        let def_id = DefId(NonZeroU32::new(next_id as u32).unwrap());
        let def = f(self, t, def_id);
        self.defs[next_id] = def;
        def_id
    }

    pub fn get_def(&self, def_id: DefId) -> &Def {
        &self.defs[def_id.0.get() as usize]
    }

    pub fn get_def_mut(&mut self, def_id: DefId) -> &mut Def {
        &mut self.defs[def_id.0.get() as usize]
    }
}

#[derive(Debug)]
pub struct Package {
    pub def_id: DefId,
    pub name: Identifier,
    pub classes: Box<[DefId]>,
}

#[derive(Debug)]
pub struct Class {
    pub def_id: DefId,
    pub name: Identifier,
    pub package: DefId,
    pub self_ty: Ty,
    pub flags: ClassFlags,

    pub extends: Option<DefId>,
    pub implements: Box<[DefId]>,
    pub within: Option<DefId>,

    pub vars: Box<[DefId]>,
    pub structs: Box<[DefId]>,
    pub enums: Box<[DefId]>,
    pub consts: Box<[DefId]>,
    pub funcs: Box<[DefId]>,
}

#[derive(Debug)]
pub struct Interface {
    pub def_id: DefId,
    pub name: Identifier,
    pub package: DefId,
    pub self_ty: Ty,
    pub flags: ClassFlags,

    pub extends: Option<DefId>,

    pub funcs: Box<[DefId]>,
}

#[derive(Debug)]
pub enum Def {
    None,
    Package(Box<Package>),
    Class(Box<Class>),
    Interface(Box<Interface>),
    Enum(Box<Enum>),
}

#[derive(Debug)]
pub struct Enum {
    pub def_id: DefId,
    pub name: Identifier,
    pub variants: HashMap<Identifier, DefId>,
}
