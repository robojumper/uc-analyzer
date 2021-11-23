#![feature(inline_const)]
#![feature(const_option)]

//! The middle representation of a whole UnrealScript workspace.

use std::num::NonZeroU32;

use ty::Ty;
use uc_def::{ArgFlags, ClassFlags, FuncFlags, Op, StructFlags, VarFlags};
use uc_name::Identifier;

pub mod ty;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DefId(NonZeroU32);

#[derive(Debug)]
pub struct Defs {
    pub defs: Vec<Def>,
}

impl Default for Defs {
    fn default() -> Self {
        Self::new()
    }
}

impl Defs {
    pub fn new() -> Self {
        Self {
            defs: vec![Def::None],
        }
    }

    /// Create an ID and a corresponding hole in the definitions vector.
    /// The hole will be filled by the result of `f`. `f` may access the
    /// passed `DefHierarchy` to insert more IDs, but code must ensure it
    /// dosn't try to access def IDs that haven't been filled yet.
    ///
    /// The AsMut<Self> thing is just here to not face the wrath of the
    /// borrow checker when trying to add more defs from within the closure
    /// even when Defs is stashed in a struct.
    pub fn add_def<T: AsMut<Self>, F: FnOnce(&mut T, DefId) -> Def>(t: &mut T, f: F) -> DefId {
        let next_id = t.as_mut().defs.len();
        if next_id > ty::MAX_DEF_ID as usize {
            panic!("too many definitions");
        }
        t.as_mut().defs.push(Def::None);
        let def_id = DefId(NonZeroU32::new(next_id as u32).unwrap());
        let def = f(t, def_id);
        t.as_mut().defs[next_id] = def;
        def_id
    }

    #[inline]
    pub fn get_def(&self, def_id: DefId) -> &Def {
        &self.defs[def_id.0.get() as usize]
    }

    #[inline]
    pub fn get_def_mut(&mut self, def_id: DefId) -> &mut Def {
        &mut self.defs[def_id.0.get() as usize]
    }

    pub fn get_class(&self, def_id: DefId) -> &Class {
        match self.get_def(def_id) {
            Def::Class(c) => c,
            _ => panic!("expected class"),
        }
    }

    pub fn get_class_mut(&mut self, def_id: DefId) -> &mut Class {
        match self.get_def_mut(def_id) {
            Def::Class(c) => c,
            _ => panic!("expected class"),
        }
    }

    pub fn get_struct(&self, def_id: DefId) -> &Struct {
        match self.get_def(def_id) {
            Def::Struct(c) => c,
            _ => panic!("expected struct"),
        }
    }

    pub fn get_struct_mut(&mut self, def_id: DefId) -> &mut Struct {
        match self.get_def_mut(def_id) {
            Def::Struct(c) => c,
            _ => panic!("expected struct"),
        }
    }
}

#[derive(Debug)]
pub enum Def {
    None,
    Package(Box<Package>),
    Class(Box<Class>),
    Enum(Box<Enum>),
    EnumVariant(Box<EnumVariant>),
    Struct(Box<Struct>),
    Var(Box<Var>),
    Operator(Box<Operator>),
    Funtion(Box<Function>),
    FuncArg(Box<FuncArg>),
}

#[derive(Debug)]
pub struct Package {
    pub def_id: DefId,
    pub name: Identifier,
    pub classes: Vec<DefId>,
}

#[derive(Debug)]
pub struct Class {
    pub def_id: DefId,
    pub name: Identifier,
    pub package: DefId,
    pub self_ty: Ty,
    pub flags: ClassFlags,

    pub kind: Option<ClassKind>,

    pub items: Vec<DefId>,
}

#[derive(Debug)]
pub enum ClassKind {
    Class {
        extends: Option<DefId>,
        implements: Box<[DefId]>,
        within: Option<DefId>,
    },
    Interface {
        extends: Option<DefId>,
    },
}

#[derive(Debug)]
pub struct Enum {
    pub def_id: DefId,
    pub owning_class: DefId,
    pub self_ty: Ty,
    pub name: Identifier,
    pub variants: Box<[DefId]>,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub def_id: DefId,
    pub owning_enum: DefId,
    pub name: Identifier,
    pub idx: u8,
}

#[derive(Debug)]
pub struct Struct {
    pub def_id: DefId,
    pub name: Identifier,
    pub owning_class: DefId,
    pub self_ty: Ty,
    pub flags: StructFlags,

    pub extends: Option<DefId>,

    pub vars: Box<[DefId]>,
}

#[derive(Debug)]
pub struct Var {
    pub def_id: DefId,
    pub name: Identifier,
    pub owner: DefId, // Class or Struct
    pub flags: VarFlags,

    pub sig: Option<VarSig>,
}

#[derive(Debug)]
pub struct Operator {
    pub def_id: DefId,
    pub op: Op,
    pub owning_class: DefId,
    pub flags: FuncFlags,

    pub sig: FuncSig,
}

#[derive(Debug)]
pub struct Function {
    pub def_id: DefId,
    pub name: Identifier,
    pub owner: DefId, // Class or State
    pub flags: FuncFlags,

    pub sig: Option<FuncSig>,
}

#[derive(Debug)]
pub struct FuncSig {
    pub ret_ty: Ty,
    pub args: Box<[DefId]>,
}

#[derive(Debug)]
pub struct FuncArg {
    pub def_id: DefId,
    pub name: Identifier,
    pub owner: DefId,
    pub flags: ArgFlags,

    pub sig: Option<VarSig>,
}

#[derive(Debug)]
pub struct VarSig {
    pub ty: Ty,
    pub dim: Option<u32>,
}
