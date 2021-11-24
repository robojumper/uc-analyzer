use std::collections::{hash_map::Entry, HashMap};

use uc_def::Op;
use uc_middle::{DefId, Defs};
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
    packages: HashMap<Identifier, DefId>,
    /// Package -> Name -> Class/Interface
    package_classes: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Name -> Enum/Struct/Class/Interface
    global_ty_defs: HashMap<Identifier, Vec<DefId>>,
    /// Class -> Name -> Struct/Enum
    scoped_ty_defs: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Name -> Enum variants
    global_values: HashMap<Identifier, DefId>,
    /// Class/Struct -> Name -> Var
    scoped_vars: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Class -> Name -> Func
    scoped_funcs: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Class -> Op -> Func
    scoped_ops: HashMap<DefId, HashMap<Op, Vec<DefId>>>,
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

    pub fn get_ty(&self, current_package: DefId, defs: &Defs, name: &Identifier) -> Result<DefId> {
        match self.global_ty_defs.get(name).map(|v| &**v) {
            Some([single]) => Ok(*single),
            Some([multiple @ ..]) => {
                let matches = multiple
                    .iter()
                    .copied()
                    .filter(|&t| current_package == defs.get_package_of_ty(t))
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
        current_package: DefId,
        defs: &Defs,
        class: &Identifier,
        name: &Identifier,
    ) -> Result<DefId> {
        // We pretty much disallow multiple types of the same name in the same package, so
        // we can cheat a little bit here by simply looking at the owning package of the class
        let class = self.get_ty(current_package, defs, class)?;
        let pack = defs.get_package_of_ty(class);
        self.get_ty(pack, defs, name)
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

    pub fn add_scoped_ty(&mut self, scope: DefId, name: Identifier, ty: DefId) -> Result<()> {
        match self.global_ty_defs.entry(name.clone()) {
            Entry::Occupied(mut e) => e.get_mut().push(ty),
            Entry::Vacant(e) => {
                e.insert(vec![ty]);
            }
        }
        let tys = self
            .scoped_ty_defs
            .entry(scope)
            .or_insert_with(HashMap::default);
        match tys.entry(name) {
            Entry::Occupied(_) => return Err(ResolutionError::ExistsInExactScope),
            Entry::Vacant(e) => {
                e.insert(ty);
            }
        }
        Ok(())
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
}
