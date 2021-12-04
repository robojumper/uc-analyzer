use std::{
    collections::{hash_map::Entry, HashMap},
    ops::ControlFlow,
};

use uc_def::Op;
use uc_middle::{DefId, Defs, ScopeWalkKind};
use uc_name::Identifier;

pub type Result<T> = std::result::Result<T, ResolutionError>;

#[derive(Debug, Clone)]
pub enum ResolutionError {
    ExistsInExactScope,
    InvalidShadowing,
    InvalidAmbiguity(Vec<DefId>),
    NotFound,
}

/// Contains the contextual data needed for (delayed) name resolution.
#[derive(Debug, Default)]
pub struct ResolverContext {
    /// Name -> Package
    pub packages: HashMap<Identifier, DefId>,
    /// Package -> Name -> Class/Interface
    pub package_classes: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Name -> Enum/Struct/Class/Interface
    pub global_ty_defs: HashMap<Identifier, Vec<DefId>>,
    /// Name -> Enum variants
    pub global_values: HashMap<Identifier, Vec<DefId>>,
    /// Class/Struct -> Name -> Var
    pub scoped_vars: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Class/Struct -> Name -> Const
    pub scoped_consts: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Class -> Name -> Func
    pub scoped_funcs: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Class -> Op -> Func
    pub scoped_ops: HashMap<DefId, HashMap<Op, Vec<DefId>>>,
}

impl ResolverContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_package(&mut self, name: Identifier, package: DefId) -> Result<()> {
        match self.packages.entry(name) {
            Entry::Occupied(_) => return Err(ResolutionError::ExistsInExactScope),
            Entry::Vacant(e) => {
                e.insert(package);
            }
        }
        self.package_classes.insert(package, HashMap::default());
        Ok(())
    }

    pub fn add_class(&mut self, package: DefId, name: Identifier, class: DefId) -> Result<()> {
        match self.global_ty_defs.entry(name.clone()) {
            Entry::Occupied(mut e) => e.get_mut().push(class),
            Entry::Vacant(e) => {
                e.insert(vec![class]);
            }
        }
        self.package_classes
            .get_mut(&package)
            .unwrap()
            .insert(name, class);
        Ok(())
    }

    pub fn add_global_value(&mut self, name: Identifier, value: DefId) -> Result<()> {
        match self.global_values.entry(name) {
            Entry::Occupied(mut e) => e.get_mut().push(value),
            Entry::Vacant(e) => {
                e.insert(vec![value]);
            }
        }
        Ok(())
    }

    pub fn get_package(&mut self, name: &Identifier) -> Result<DefId> {
        self.packages
            .get(name)
            .copied()
            .ok_or(ResolutionError::NotFound)
    }

    pub fn get_global_value(&self, scope: DefId, defs: &Defs, name: &Identifier) -> Result<DefId> {
        match self.global_values.get(name).map(|v| &**v) {
            Some([single]) => Ok(*single),
            Some([multiple @ ..]) => {
                let matches = multiple
                    .iter()
                    .copied()
                    .filter(|&t| {
                        scope == defs.get_enum(defs.get_variant(t).owning_enum).owning_class
                    })
                    .collect::<Vec<_>>();
                match &*matches {
                    [] => Err(ResolutionError::InvalidAmbiguity(multiple.to_owned())),
                    [one] => Ok(*one),
                    [multiple_in_class @ ..] => Err(ResolutionError::InvalidAmbiguity(
                        multiple_in_class.to_owned(),
                    )),
                }
            }
            None => Err(ResolutionError::NotFound),
        }
    }

    pub fn get_ty(&self, scope: DefId, defs: &Defs, name: &Identifier) -> Result<DefId> {
        match self.global_ty_defs.get(name).map(|v| &**v) {
            Some([single]) => Ok(*single),
            Some([multiple @ ..]) => {
                let matches = multiple
                    .iter()
                    .copied()
                    .filter(|&item| defs.ty_in_scope(scope, item))
                    .collect::<Vec<_>>();
                match &*matches {
                    [] => Err(ResolutionError::InvalidAmbiguity(multiple.to_owned())),
                    [one] => Ok(*one),
                    [multiple_in_package @ ..] => Err(ResolutionError::InvalidAmbiguity(
                        multiple_in_package.to_owned(),
                    )),
                }
            }
            None => Err(ResolutionError::NotFound),
        }
    }

    /// Get a qualified type.
    pub fn get_ty_in(
        &self,
        scope: Option<DefId>,
        defs: &Defs,
        first: &Identifier,
        second: &Identifier,
    ) -> Result<DefId> {
        // If the first part is a package name, we want a class
        if let Some(pack_id) = self.packages.get(first) {
            return match self.package_classes.get(pack_id).unwrap().get(second) {
                Some(d) => Ok(*d),
                None => Err(ResolutionError::NotFound),
            };
        }

        // Otherwise, assume it's a class and a struct/enum
        let class = self.get_ty(scope.unwrap(), defs, first)?;
        self.get_ty(class, defs, second)
    }

    pub fn add_scoped_var(&mut self, scope: DefId, name: Identifier, var: DefId) -> Result<()> {
        let vars = self
            .scoped_vars
            .entry(scope)
            .or_insert_with(HashMap::default);
        match vars.entry(name) {
            Entry::Occupied(_) => return Err(ResolutionError::ExistsInExactScope),
            Entry::Vacant(e) => {
                e.insert(var);
            }
        }
        Ok(())
    }

    pub fn add_scoped_const(
        &mut self,
        scope: DefId,
        name: Identifier,
        const_id: DefId,
    ) -> Result<()> {
        let consts = self
            .scoped_consts
            .entry(scope)
            .or_insert_with(HashMap::default);
        match consts.entry(name) {
            Entry::Occupied(_) => return Err(ResolutionError::ExistsInExactScope),
            Entry::Vacant(e) => {
                e.insert(const_id);
            }
        }
        Ok(())
    }

    pub fn get_scoped_const(
        &self,
        scope: DefId,
        defs: &Defs,
        kind: ScopeWalkKind,
        const_name: &Identifier,
    ) -> Result<DefId> {
        match defs.walk_scopes(scope, kind, |def_id| {
            match self.scoped_consts.get(&def_id) {
                Some(consts) => match consts.get(const_name) {
                    Some(&d) => ControlFlow::Break(d),
                    None => ControlFlow::Continue(()),
                },
                None => ControlFlow::Continue(()),
            }
        }) {
            Some(d) => Ok(d),
            None => Err(ResolutionError::NotFound),
        }
    }

    pub fn add_scoped_ty(&mut self, name: Identifier, ty: DefId) {
        match self.global_ty_defs.entry(name) {
            Entry::Occupied(mut e) => e.get_mut().push(ty),
            Entry::Vacant(e) => {
                e.insert(vec![ty]);
            }
        }
    }

    pub fn add_scoped_func(&mut self, scope: DefId, name: Identifier, func: DefId) -> Result<()> {
        let funcs = self
            .scoped_funcs
            .entry(scope)
            .or_insert_with(HashMap::default);
        match funcs.entry(name) {
            Entry::Occupied(_) => return Err(ResolutionError::ExistsInExactScope),
            Entry::Vacant(e) => {
                e.insert(func);
            }
        }
        Ok(())
    }

    pub fn add_scoped_op(&mut self, scope: DefId, name: Op, op: DefId) -> Result<()> {
        let ops = self
            .scoped_ops
            .entry(scope)
            .or_insert_with(HashMap::default);
        match ops.entry(name) {
            Entry::Occupied(mut e) => e.get_mut().push(op),
            Entry::Vacant(e) => {
                e.insert(vec![op]);
            }
        }
        Ok(())
    }

    pub fn get_scoped_func(
        &self,
        scope: DefId,
        defs: &Defs,
        kind: ScopeWalkKind,
        func_name: &Identifier,
    ) -> Result<DefId> {
        match defs.walk_scopes(scope, kind, |def_id| match self.scoped_funcs.get(&def_id) {
            Some(funcs) => match funcs.get(func_name) {
                Some(&f) => ControlFlow::Break(f),
                None => ControlFlow::Continue(()),
            },
            None => ControlFlow::Continue(()),
        }) {
            Some(d) => Ok(d),
            None => Err(ResolutionError::NotFound),
        }
    }

    pub fn get_scoped_var(
        &self,
        scope: DefId,
        defs: &Defs,
        kind: ScopeWalkKind,
        var_name: &Identifier,
    ) -> Result<DefId> {
        match defs.walk_scopes(scope, kind, |def_id| match self.scoped_vars.get(&def_id) {
            Some(vars) => match vars.get(var_name) {
                Some(&d) => ControlFlow::Break(d),
                None => ControlFlow::Continue(()),
            },
            None => ControlFlow::Continue(()),
        }) {
            Some(d) => Ok(d),
            None => Err(ResolutionError::NotFound),
        }
    }
}
