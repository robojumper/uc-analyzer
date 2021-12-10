//! The middle representation of a whole UnrealScript workspace.

use std::{num::NonZeroU32, ops::ControlFlow};

use body::{Body, Literal};
use ty::Ty;
use uc_def::{ArgFlags, ClassFlags, FuncFlags, Op, StructFlags, VarFlags};
use uc_files::Span;
use uc_name::Identifier;

pub mod body;
pub mod ty;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefId(NonZeroU32);

#[derive(Debug)]
pub struct Defs {
    pub defs: Vec<DefSlot>,
}

impl Default for Defs {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy)]
pub enum ScopeWalkKind {
    Definitions,
    Access,
}

impl Defs {
    pub fn new() -> Self {
        Self { defs: vec![DefSlot::Empty] }
    }

    /// Create an ID and a corresponding hole in the definitions vector.
    /// The hole will be filled by the result of `f`. `f` may access the
    /// passed `DefHierarchy` to insert more IDs, but code must ensure it
    /// dosn't try to access def IDs that haven't been filled yet.
    ///
    /// The AsMut<Self> thing is just here to not face the wrath of the
    /// borrow checker when trying to add more defs from within the closure
    /// even when Defs is stashed in a struct.
    pub fn add_def<T: AsMut<Self>, F: FnOnce(&mut T, DefId) -> (DefKind, Option<Span>)>(
        t: &mut T,
        f: F,
    ) -> DefId {
        let next_id = t.as_mut().defs.len();
        if next_id > u32::MAX as usize {
            panic!("too many definitions");
        }
        t.as_mut().defs.push(DefSlot::Empty);
        let def_id = DefId(NonZeroU32::new(next_id as u32).unwrap());
        let (kind, span) = f(t, def_id);
        t.as_mut().defs[next_id] = DefSlot::Def(Def { id: def_id, span, kind });
        def_id
    }

    #[inline]
    pub fn get_def(&self, def_id: DefId) -> &Def {
        let def = &self.defs[def_id.0.get() as usize];
        match def {
            DefSlot::Empty => panic!("internal error"),
            DefSlot::Def(d) => d,
        }
    }

    #[inline]
    pub fn get_def_mut(&mut self, def_id: DefId) -> &mut Def {
        let def = &mut self.defs[def_id.0.get() as usize];
        match def {
            DefSlot::Empty => panic!("internal error"),
            DefSlot::Def(d) => d,
        }
    }

    /// Whether the type `item` should be considered "in scope" in case
    /// of name conflicts. For classes, this means same package, for structs
    /// and enums, this means same class or superclass.
    /// Notably, types can always be used from anywhere. This is strictly
    /// a heuristic to formalize the "last declaration wins" behavior from UCC.
    pub fn ty_in_scope(&self, scope: DefId, item: DefId) -> bool {
        let def_scope = match &self.get_def(item).kind {
            DefKind::Class(c) => return c.package == self.get_item_package(item),
            DefKind::Enum(e) => e.owning_class,
            DefKind::Struct(s) => s.owning_class,
            x => panic!("not valid for ty_in_scope: {:?}", x),
        };

        match self.walk_scopes_inner(scope, ScopeWalkKind::Definitions, &mut |id| {
            if id == def_scope { ControlFlow::Break(def_scope) } else { ControlFlow::Continue(()) }
        }) {
            ControlFlow::Continue(()) => false,
            ControlFlow::Break(_) => true,
        }
    }

    pub fn get_item_package(&self, mut def_id: DefId) -> DefId {
        loop {
            let def = self.get_def(def_id);
            match &def.kind {
                DefKind::Class(c) => break c.package,
                DefKind::Enum(e) => def_id = e.owning_class,
                DefKind::Struct(s) => def_id = s.owning_class,
                DefKind::Package(_) => break def.id,
                DefKind::EnumVariant(v) => def_id = v.owning_enum,
                DefKind::Var(v) => def_id = v.owner,
                DefKind::Const(c) => def_id = c.owner,
                DefKind::State(s) => def_id = s.owner,
                DefKind::Operator(o) => def_id = o.owning_class,
                DefKind::Function(f) => def_id = f.owner,
                DefKind::FuncArg(a) => def_id = a.owner,
                DefKind::Local(l) => def_id = l.owner,
            }
        }
    }

    pub fn get_item_class(&self, mut def_id: DefId) -> DefId {
        loop {
            let def = self.get_def(def_id);
            match &def.kind {
                DefKind::Class(_) => break def.id,
                DefKind::Enum(e) => break e.owning_class,
                DefKind::Struct(s) => break s.owning_class,
                DefKind::EnumVariant(v) => def_id = v.owning_enum,
                DefKind::Var(v) => def_id = v.owner,
                DefKind::Const(c) => def_id = c.owner,
                DefKind::State(s) => def_id = s.owner,
                DefKind::Operator(o) => def_id = o.owning_class,
                DefKind::Function(f) => def_id = f.owner,
                DefKind::FuncArg(a) => def_id = a.owner,
                DefKind::Local(l) => def_id = l.owner,
                d => panic!("cannot get class: {:?}", d),
            }
        }
    }

    pub fn walk_scopes<F: FnMut(DefId) -> ControlFlow<DefId>>(
        &self,
        def_id: DefId,
        kind: ScopeWalkKind,
        mut f: F,
    ) -> Option<DefId> {
        match self.walk_scopes_inner(def_id, kind, &mut f) {
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
        let def = self.get_def(def_id);
        match &def.kind {
            DefKind::Class(c) => {
                cb(def.id)?;
                match c.kind.as_ref().unwrap() {
                    ClassKind::Class { extends, within, .. } => {
                        if matches!(kind, ScopeWalkKind::Definitions) {
                            if let Some(within) = within {
                                self.walk_scopes_inner(*within, kind, cb)?;
                            }
                        }
                        if let Some(extends) = extends {
                            self.walk_scopes_inner(*extends, kind, cb)?;
                        }
                    }
                    ClassKind::Interface { extends } => {
                        if let Some(extends) = extends {
                            self.walk_scopes_inner(*extends, kind, cb)?;
                        }
                    }
                }
                if let ScopeWalkKind::Definitions = kind {
                    for &dependson in c.dependson.iter() {
                        self.walk_scopes_inner(dependson, kind, cb)?;
                    }
                }
            }
            DefKind::Enum(e) => self.walk_scopes_inner(e.owning_class, kind, cb)?,
            DefKind::Struct(s) => {
                cb(def.id)?;
                match kind {
                    ScopeWalkKind::Access => {
                        if let Some(extends) = s.extends {
                            self.walk_scopes_inner(extends, kind, cb)?
                        }
                    }
                    ScopeWalkKind::Definitions => {
                        self.walk_scopes_inner(s.owning_class, kind, cb)?
                    }
                }
            }
            DefKind::State(s) => {
                cb(def.id)?;
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
            DefKind::Operator(o) => {
                cb(def.id)?;
                self.walk_scopes_inner(o.owning_class, kind, cb)?;
            }
            DefKind::Function(f) => {
                cb(def.id)?;
                self.walk_scopes_inner(f.owner, kind, cb)?;
            }
            DefKind::Var(v) => {
                self.walk_scopes_inner(v.owner, kind, cb)?;
            }
            DefKind::Const(c) => {
                self.walk_scopes_inner(c.owner, kind, cb)?;
            }
            x => panic!("not usable for scope walk {:?}", x),
        }
        ControlFlow::Continue(())
    }

    pub fn try_get_span(&self, def_id: DefId) -> Option<Span> {
        self.get_def(def_id).span
    }

    pub fn get_class(&self, def_id: DefId) -> &Class {
        match &self.get_def(def_id).kind {
            DefKind::Class(c) => c,
            _ => panic!("expected class"),
        }
    }

    pub fn get_class_mut(&mut self, def_id: DefId) -> &mut Class {
        match &mut self.get_def_mut(def_id).kind {
            DefKind::Class(c) => c,
            _ => panic!("expected class"),
        }
    }

    pub fn get_const(&self, def_id: DefId) -> &Const {
        match &self.get_def(def_id).kind {
            DefKind::Const(c) => c,
            _ => panic!("expected const"),
        }
    }

    pub fn get_const_mut(&mut self, def_id: DefId) -> &mut Const {
        match &mut self.get_def_mut(def_id).kind {
            DefKind::Const(c) => c,
            _ => panic!("expected const"),
        }
    }

    pub fn get_var(&self, def_id: DefId) -> &Var {
        match &self.get_def(def_id).kind {
            DefKind::Var(v) => v,
            _ => panic!("expected var"),
        }
    }

    pub fn get_var_mut(&mut self, def_id: DefId) -> &mut Var {
        match &mut self.get_def_mut(def_id).kind {
            DefKind::Var(v) => v,
            _ => panic!("expected var"),
        }
    }

    pub fn get_struct(&self, def_id: DefId) -> &Struct {
        match &self.get_def(def_id).kind {
            DefKind::Struct(c) => c,
            _ => panic!("expected struct"),
        }
    }

    pub fn get_struct_mut(&mut self, def_id: DefId) -> &mut Struct {
        match &mut self.get_def_mut(def_id).kind {
            DefKind::Struct(c) => c,
            _ => panic!("expected struct"),
        }
    }

    pub fn get_enum(&self, def_id: DefId) -> &Enum {
        match &self.get_def(def_id).kind {
            DefKind::Enum(c) => c,
            _ => panic!("expected enum"),
        }
    }

    pub fn get_enum_mut(&mut self, def_id: DefId) -> &mut Enum {
        match &mut self.get_def_mut(def_id).kind {
            DefKind::Enum(c) => c,
            _ => panic!("expected enum"),
        }
    }

    pub fn get_variant(&self, def_id: DefId) -> &EnumVariant {
        match &self.get_def(def_id).kind {
            DefKind::EnumVariant(v) => v,
            _ => panic!("expected variant"),
        }
    }

    pub fn get_variant_mut(&mut self, def_id: DefId) -> &mut EnumVariant {
        match &mut self.get_def_mut(def_id).kind {
            DefKind::EnumVariant(v) => v,
            _ => panic!("expected variant"),
        }
    }

    pub fn get_func(&self, def_id: DefId) -> &Function {
        match &self.get_def(def_id).kind {
            DefKind::Function(f) => f,
            _ => panic!("expected func"),
        }
    }

    pub fn get_func_mut(&mut self, def_id: DefId) -> &mut Function {
        match &mut self.get_def_mut(def_id).kind {
            DefKind::Function(f) => f,
            _ => panic!("expected func"),
        }
    }

    pub fn get_op(&self, def_id: DefId) -> &Operator {
        match &self.get_def(def_id).kind {
            DefKind::Operator(o) => o,
            _ => panic!("expected op"),
        }
    }

    pub fn get_op_mut(&mut self, def_id: DefId) -> &mut Operator {
        match &mut self.get_def_mut(def_id).kind {
            DefKind::Operator(o) => o,
            _ => panic!("expected op"),
        }
    }

    pub fn get_arg(&self, def_id: DefId) -> &FuncArg {
        match &self.get_def(def_id).kind {
            DefKind::FuncArg(f) => f,
            _ => panic!("expected arg"),
        }
    }

    pub fn get_arg_mut(&mut self, def_id: DefId) -> &mut FuncArg {
        match &mut self.get_def_mut(def_id).kind {
            DefKind::FuncArg(f) => f,
            _ => panic!("expected arg"),
        }
    }
}

#[derive(Debug)]
pub enum DefSlot {
    Empty,
    Def(Def),
}

#[derive(Debug)]
pub struct Def {
    pub id: DefId,
    pub span: Option<Span>,
    pub kind: DefKind,
}

#[derive(Debug)]
pub enum DefKind {
    Package(Package),
    // This box halves the size of this enum
    Class(Box<Class>),
    Enum(Enum),
    EnumVariant(EnumVariant),
    Struct(Struct),
    Var(Var),
    Const(Const),
    State(State),
    Operator(Operator),
    Function(Function),
    FuncArg(FuncArg),
    Local(Local),
}

#[derive(Debug)]
pub struct Package {
    pub name: Identifier,
    pub classes: Vec<DefId>,
}

#[derive(Debug)]
pub struct Class {
    pub name: Identifier,
    pub package: DefId,
    pub self_ty: Ty,
    pub flags: ClassFlags,

    pub dependson: Box<[DefId]>,
    pub kind: Option<ClassKind>,

    pub items: Vec<DefId>,
}

#[derive(Debug)]
pub enum ClassKind {
    Class { extends: Option<DefId>, implements: Box<[DefId]>, within: Option<DefId> },
    Interface { extends: Option<DefId> },
}

#[derive(Debug)]
pub struct Enum {
    pub name: Identifier,
    pub owning_class: DefId,
    pub self_ty: Ty,
    pub variants: Box<[DefId]>,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: Identifier,
    pub owning_enum: DefId,
    pub idx: u8,
}

#[derive(Debug)]
pub struct Struct {
    pub name: Identifier,
    pub owning_class: DefId,
    pub self_ty: Ty,
    pub flags: StructFlags,

    pub extends: Option<DefId>,

    pub vars: Box<[DefId]>,
}

#[derive(Debug)]
pub struct Var {
    pub name: Identifier,
    pub owner: DefId, // Class or Struct
    pub flags: VarFlags,

    pub ty: Option<Ty>,
}

#[derive(Debug)]
pub struct Const {
    pub name: Identifier,
    pub owner: DefId, // Class
    pub val: ConstVal,
}

#[derive(Debug)]
pub enum ConstVal {
    Literal(Literal),
    Redirect(Identifier),
}

#[derive(Debug)]
pub struct State {
    pub name: Identifier,
    pub owner: DefId, // Class
    pub funcs: Box<[DefId]>,
    pub contents: Option<Body>,
}

#[derive(Debug)]
pub struct Operator {
    pub op: Op,
    pub owning_class: DefId,
    pub flags: FuncFlags,

    pub sig: FuncSig,
    pub contents: Option<FuncContents>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub owner: DefId, // Class or State
    pub flags: FuncFlags,
    pub delegate_prop: Option<DefId>,

    pub sig: FuncSig,
    pub contents: Option<FuncContents>,
}

#[derive(Debug)]
pub struct FuncSig {
    pub ret_ty: Option<Ty>,
    pub args: Box<[DefId]>,
}

#[derive(Debug)]
pub struct FuncArg {
    pub name: Identifier,
    pub owner: DefId,
    pub flags: ArgFlags,

    pub ty: Ty,
}

#[derive(Debug)]
pub struct Local {
    pub name: Identifier,
    pub owner: DefId,

    pub ty: Ty,
}

#[derive(Debug)]
pub struct FuncContents {
    pub locals: Box<[DefId]>,
    pub statements: Option<Body>,
}
