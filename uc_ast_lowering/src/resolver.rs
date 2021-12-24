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
    /// Name -> Enum variants
    pub enum_values: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Class/Struct -> Name -> Var/Const/Func
    pub scoped_items: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Class -> Op -> Func
    pub scoped_ops: HashMap<DefId, HashMap<Op, Vec<DefId>>>,
    /// Class -> Name -> State
    pub scoped_states: HashMap<DefId, HashMap<Identifier, DefId>>,
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
        self.package_classes.get_mut(&package).unwrap().insert(name, class);
        Ok(())
    }

    pub fn add_enum_value(&mut self, name: Identifier, owner: DefId, value: DefId) -> Result<()> {
        match self.global_values.entry(name.clone()) {
            Entry::Occupied(mut e) => e.get_mut().push(value),
            Entry::Vacant(e) => {
                e.insert(vec![value]);
            }
        }
        let variants = self.enum_values.entry(owner).or_insert_with(HashMap::default);
        match variants.entry(name) {
            Entry::Occupied(_) => return Err(ResolutionError::ExistsInExactScope),
            Entry::Vacant(e) => {
                e.insert(value);
            }
        }
        Ok(())
    }

    pub fn get_package(&mut self, name: &Identifier) -> Result<DefId> {
        self.packages.get(name).copied().ok_or(ResolutionError::NotFound)
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
                    [multiple_in_class @ ..] => {
                        Err(ResolutionError::InvalidAmbiguity(multiple_in_class.to_owned()))
                    }
                }
            }
            None => Err(ResolutionError::NotFound),
        }
    }

    pub fn get_enum_value(&self, enum_id: DefId, name: &Identifier) -> Result<DefId> {
        match self.enum_values.get(&enum_id) {
            Some(vals) => vals.get(name).copied().ok_or(ResolutionError::NotFound),
            None => Err(ResolutionError::NotFound), // Enum has no variants?
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
                    [multiple_in_package @ ..] => {
                        Err(ResolutionError::InvalidAmbiguity(multiple_in_package.to_owned()))
                    }
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

    pub fn add_scoped_item(&mut self, scope: DefId, name: Identifier, item: DefId) -> Result<()> {
        let vars = self.scoped_items.entry(scope).or_insert_with(HashMap::default);
        match vars.entry(name) {
            Entry::Occupied(_) => return Err(ResolutionError::ExistsInExactScope),
            Entry::Vacant(e) => {
                e.insert(item);
            }
        }
        Ok(())
    }

    pub fn add_scoped_ty(&mut self, scope: DefId, name: Identifier, ty: DefId) -> Result<()> {
        self.add_scoped_item(scope, name.clone(), ty)?;
        match self.global_ty_defs.entry(name) {
            Entry::Occupied(mut e) => e.get_mut().push(ty),
            Entry::Vacant(e) => {
                e.insert(vec![ty]);
            }
        }
        Ok(())
    }

    pub fn add_scoped_state(
        &mut self,
        scope: DefId,
        name: Identifier,
        state_def: DefId,
    ) -> Result<()> {
        let states = self.scoped_states.entry(scope).or_insert_with(HashMap::default);
        match states.entry(name) {
            Entry::Occupied(_) => return Err(ResolutionError::ExistsInExactScope),
            Entry::Vacant(e) => {
                e.insert(state_def);
            }
        }
        Ok(())
    }

    pub fn add_scoped_op(&mut self, scope: DefId, name: Op, op: DefId) -> Result<()> {
        let ops = self.scoped_ops.entry(scope).or_insert_with(HashMap::default);
        match ops.entry(name) {
            Entry::Occupied(mut e) => e.get_mut().push(op),
            Entry::Vacant(e) => {
                e.insert(vec![op]);
            }
        }
        Ok(())
    }

    pub fn get_scoped_item<F: Fn(DefId) -> bool>(
        &self,
        scope: DefId,
        defs: &Defs,
        kind: ScopeWalkKind,
        name: &Identifier,
        pred: F,
    ) -> Result<DefId> {
        match defs.walk_scopes(scope, kind, |def_id| match self.scoped_items.get(&def_id) {
            Some(items) => match items.get(name) {
                Some(&d) if pred(d) => ControlFlow::Break(d),
                Some(_) => ControlFlow::Continue(()),
                None => ControlFlow::Continue(()),
            },
            None => ControlFlow::Continue(()),
        }) {
            Some(d) => Ok(d),
            None => Err(ResolutionError::NotFound),
        }
    }

    pub fn get_scoped_state(
        &self,
        scope: DefId,
        defs: &Defs,
        kind: ScopeWalkKind,
        name: &Identifier,
    ) -> Result<DefId> {
        match defs.walk_scopes(scope, kind, |def_id| match self.scoped_states.get(&def_id) {
            Some(items) => match items.get(name) {
                Some(&d) => ControlFlow::Break(d),
                None => ControlFlow::Continue(()),
            },
            None => ControlFlow::Continue(()),
        }) {
            Some(d) => Ok(d),
            None => Err(ResolutionError::NotFound),
        }
    }

    pub fn collect_scoped_ops(
        &self,
        scope: DefId,
        defs: &Defs,
        kind: ScopeWalkKind,
        op: Op,
    ) -> Vec<DefId> {
        let mut candidate_ops = vec![];
        defs.walk_scopes(scope, kind, |def_id| match self.scoped_ops.get(&def_id) {
            Some(ops) => match ops.get(&op) {
                Some(d) => {
                    candidate_ops.extend_from_slice(&**d);
                    ControlFlow::Continue(())
                }
                None => ControlFlow::Continue(()),
            },
            None => ControlFlow::Continue(()),
        });
        candidate_ops
    }
}
