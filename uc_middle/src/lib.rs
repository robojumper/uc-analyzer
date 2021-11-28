#![feature(inline_const_pat)]
#![feature(const_option)]

//! The middle representation of a whole UnrealScript workspace.

use std::{num::NonZeroU32, ops::ControlFlow};

use ty::Ty;
use uc_def::{ArgFlags, ClassFlags, FuncFlags, Op, StructFlags, VarFlags};
use uc_files::Span;
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

#[derive(Clone, Copy)]
enum ScopeWalkKind {
    Definitions,
    Access,
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
        let def = &self.defs[def_id.0.get() as usize];
        assert!(!matches!(def, Def::None));
        def
    }

    #[inline]
    pub fn get_def_mut(&mut self, def_id: DefId) -> &mut Def {
        let def = &mut self.defs[def_id.0.get() as usize];
        assert!(!matches!(def, Def::None));
        def
    }

    /// Whether the type `item` should be considered "in scope" in case
    /// of name conflicts. For classes, this means same package, for structs
    /// and enums, this means same class or superclass.
    /// Notably, types can always be used from anywhere. This is strictly
    /// a heuristic to formalize the "last declaration wins" behavior from UCC.
    pub fn ty_in_scope(&self, scope: DefId, item: DefId) -> bool {
        let def_scope = match self.get_def(item) {
            Def::Class(c) => return c.package == self.get_item_package(item),
            Def::Enum(e) => e.owning_class,
            Def::Struct(s) => s.owning_class,
            x => panic!("not valid for ty_in_scope: {:?}", x),
        };

        match self.walk_scopes_inner(scope, ScopeWalkKind::Definitions, &mut |id| {
            if id == def_scope {
                ControlFlow::Break(def_scope)
            } else {
                ControlFlow::Continue(())
            }
        }) {
            ControlFlow::Continue(()) => false,
            ControlFlow::Break(_) => true,
        }
    }

    pub fn get_item_package(&self, mut def_id: DefId) -> DefId {
        loop {
            match self.get_def(def_id) {
                Def::None => unreachable!(),
                Def::Class(c) => break c.package,
                Def::Enum(e) => def_id = e.owning_class,
                Def::Struct(s) => def_id = s.owning_class,
                Def::Package(p) => break p.def_id,
                Def::EnumVariant(v) => def_id = v.owning_enum,
                Def::Var(v) => def_id = v.owner,
                Def::Const(c) => def_id = c.owner,
                Def::State(s) => def_id = s.owner,
                Def::Operator(o) => def_id = o.owning_class,
                Def::Function(f) => def_id = f.owner,
                Def::FuncArg(a) => def_id = a.owner,
            }
        }
    }

    pub fn get_item_class(&self, mut def_id: DefId) -> DefId {
        loop {
            match self.get_def(def_id) {
                Def::Class(c) => break c.def_id,
                Def::Enum(e) => break e.owning_class,
                Def::Struct(s) => break s.owning_class,
                Def::EnumVariant(v) => def_id = v.owning_enum,
                Def::Var(v) => def_id = v.owner,
                Def::Const(c) => def_id = c.owner,
                Def::State(s) => def_id = s.owner,
                Def::Operator(o) => def_id = o.owning_class,
                Def::Function(f) => def_id = f.owner,
                Def::FuncArg(a) => def_id = a.owner,
                d => panic!("cannot get class: {:?}", d),
            }
        }
    }

    pub fn walk_defining_scopes<F: FnMut(DefId) -> ControlFlow<DefId>>(
        &self,
        def_id: DefId,
        mut f: F,
    ) -> Option<DefId> {
        match self.walk_scopes_inner(def_id, ScopeWalkKind::Definitions, &mut f) {
            ControlFlow::Break(d) => Some(d),
            ControlFlow::Continue(_) => None,
        }
    }

    /// The two main kinds of scope walks are the defining scope walks
    /// (to check for type and function definitions) and access scope
    /// walks (to check for variables and functions).
    fn walk_scopes_inner<F: FnMut(DefId) -> ControlFlow<DefId>>(
        &self,
        def_id: DefId,
        kind: ScopeWalkKind,
        cb: &mut F,
    ) -> ControlFlow<DefId> {
        match self.get_def(def_id) {
            Def::None => panic!("none def"),
            Def::Class(c) => {
                cb(c.def_id)?;
                match c.kind.as_ref().unwrap() {
                    ClassKind::Class {
                        extends, within, ..
                    } => {
                        if let Some(within) = within {
                            self.walk_scopes_inner(*within, kind, cb)?;
                        }
                        if let Some(extends) = extends {
                            self.walk_scopes_inner(*extends, kind, cb)?;
                        }
                    }
                    ClassKind::Interface { extends } => {
                        self.walk_scopes_inner(*extends, kind, cb)?;
                    }
                }
                if let ScopeWalkKind::Definitions = kind {
                    for &dependson in c.dependson.iter() {
                        self.walk_scopes_inner(dependson, kind, cb)?;
                    }
                }
            }
            Def::Enum(e) => self.walk_scopes_inner(e.owning_class, kind, cb)?,
            Def::Struct(s) => match kind {
                ScopeWalkKind::Access => {
                    if let Some(extends) = s.extends {
                        self.walk_scopes_inner(extends, kind, cb)?
                    }
                }
                ScopeWalkKind::Definitions => self.walk_scopes_inner(s.owning_class, kind, cb)?,
            },
            Def::State(s) => {
                cb(s.def_id)?;
                match kind {
                    ScopeWalkKind::Access => {
                        // TODO: Also walk extends
                        self.walk_scopes_inner(s.owner, kind, cb)?;
                    }
                    ScopeWalkKind::Definitions => {
                        self.walk_scopes_inner(s.owner, kind, cb)?;
                    }
                }
            }
            Def::Operator(o) => {
                self.walk_scopes_inner(o.owning_class, kind, cb)?;
            }
            Def::Function(f) => {
                self.walk_scopes_inner(f.owner, kind, cb)?;
            }
            Def::Var(v) => {
                self.walk_scopes_inner(v.owner, kind, cb)?;
            }
            x => panic!("not usable for scope walk {:?}", x),
        }
        ControlFlow::Continue(())
    }

    pub fn try_get_span(&self, def_id: DefId) -> Option<Span> {
        match self.get_def(def_id) {
            Def::Class(c) => c.span,
            Def::Enum(e) => e.span,
            Def::EnumVariant(v) => v.span,
            Def::Struct(s) => s.span,
            Def::Var(v) => v.span,
            Def::Const(c) => c.span,
            Def::State(s) => s.span,
            Def::Operator(o) => o.span,
            Def::Function(f) => f.span,
            Def::FuncArg(a) => a.span,
            _ => None,
        }
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

    pub fn get_const(&self, def_id: DefId) -> &Const {
        match self.get_def(def_id) {
            Def::Const(c) => c,
            _ => panic!("expected const"),
        }
    }

    pub fn get_const_mut(&mut self, def_id: DefId) -> &mut Const {
        match self.get_def_mut(def_id) {
            Def::Const(c) => c,
            _ => panic!("expected const"),
        }
    }

    pub fn get_var(&self, def_id: DefId) -> &Var {
        match self.get_def(def_id) {
            Def::Var(v) => v,
            _ => panic!("expected var"),
        }
    }

    pub fn get_var_mut(&mut self, def_id: DefId) -> &mut Var {
        match self.get_def_mut(def_id) {
            Def::Var(v) => v,
            _ => panic!("expected var"),
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

    pub fn get_enum(&self, def_id: DefId) -> &Enum {
        match self.get_def(def_id) {
            Def::Enum(c) => c,
            _ => panic!("expected enum"),
        }
    }

    pub fn get_enum_mut(&mut self, def_id: DefId) -> &mut Enum {
        match self.get_def_mut(def_id) {
            Def::Enum(c) => c,
            _ => panic!("expected enum"),
        }
    }

    pub fn get_variant(&self, def_id: DefId) -> &EnumVariant {
        match self.get_def(def_id) {
            Def::EnumVariant(v) => v,
            _ => panic!("expected variant"),
        }
    }

    pub fn get_variant_mut(&mut self, def_id: DefId) -> &mut EnumVariant {
        match self.get_def_mut(def_id) {
            Def::EnumVariant(v) => v,
            _ => panic!("expected variant"),
        }
    }

    pub fn get_func(&self, def_id: DefId) -> &Function {
        match self.get_def(def_id) {
            Def::Function(f) => f,
            _ => panic!("expected func"),
        }
    }

    pub fn get_func_mut(&mut self, def_id: DefId) -> &mut Function {
        match self.get_def_mut(def_id) {
            Def::Function(f) => f,
            _ => panic!("expected func"),
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
    Const(Box<Const>),
    State(Box<State>),
    Operator(Box<Operator>),
    Function(Box<Function>),
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
    pub span: Option<Span>,
    pub package: DefId,
    pub self_ty: Ty,
    pub flags: ClassFlags,

    pub dependson: Box<[DefId]>,
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
        extends: DefId,
    },
}

#[derive(Debug)]
pub struct Enum {
    pub def_id: DefId,
    pub name: Identifier,
    pub span: Option<Span>,
    pub owning_class: DefId,
    pub self_ty: Ty,
    pub variants: Box<[DefId]>,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub def_id: DefId,
    pub name: Identifier,
    pub span: Option<Span>,
    pub owning_enum: DefId,
    pub idx: u8,
}

#[derive(Debug)]
pub struct Struct {
    pub def_id: DefId,
    pub name: Identifier,
    pub span: Option<Span>,
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
    pub span: Option<Span>,
    pub owner: DefId, // Class or Struct
    pub flags: VarFlags,

    pub sig: Option<VarSig>,
}

#[derive(Debug)]
pub struct Const {
    pub def_id: DefId,
    pub name: Identifier,
    pub span: Option<Span>,
    pub owner: DefId, // Class
    pub val: ConstVal,
}

#[derive(Debug)]
pub enum ConstVal {
    Num(i32),
    Other,
}

#[derive(Debug)]
pub struct State {
    pub def_id: DefId,
    pub name: Identifier,
    pub span: Option<Span>,
    pub owner: DefId, // Class
    pub funcs: Box<[DefId]>,
    pub contents: Option<Statements>,
}

#[derive(Debug)]
pub struct Operator {
    pub def_id: DefId,
    pub op: Op,
    pub span: Option<Span>,
    pub owning_class: DefId,
    pub flags: FuncFlags,

    pub sig: Option<FuncSig>,
}

#[derive(Debug)]
pub struct Function {
    pub def_id: DefId,
    pub name: Identifier,
    pub span: Option<Span>,
    pub owner: DefId, // Class or State
    pub flags: FuncFlags,
    pub delegate_prop: Option<DefId>,

    pub sig: Option<FuncSig>,
    pub contents: Option<Statements>,
}

#[derive(Debug)]
pub struct FuncSig {
    pub ret_ty: Option<Ty>,
    pub args: Box<[DefId]>,
}

#[derive(Debug)]
pub struct FuncArg {
    pub def_id: DefId,
    pub name: Identifier,
    pub span: Option<Span>,
    pub owner: DefId,
    pub flags: ArgFlags,

    pub sig: Option<VarSig>,
}

#[derive(Debug)]
pub struct VarSig {
    pub ty: Ty,
    pub dim: Option<u32>,
}

#[derive(Debug)]
pub struct Statements {}
