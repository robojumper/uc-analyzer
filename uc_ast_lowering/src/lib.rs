//! Lower the AST to definitions, where things refer to each other
//! through indices instead of definitions.
//!
//! UCC requires a particular compilation order (both in terms of
//! package order and class order) in order to be able to resolve
//! things when they appear. In order to do that, UCC parses the classes
//! in multiple phases, with small fixup phases inbetween.
//!
//! * Structs/enums , classes/interfaces are in a global namespace.
//! * Delegates use the local/recursive superclass function namespace,
//!   but can explicitly mention a class.
//!
//! It should be noted that the uc_middle output of this package
//! doesn't actually contain any resolution information anymore.
//! The [`LoweringContext`] internal to this package contains all
//! the contextual information needed to resolve things, but the
//! DefHierarchy is ideally fully resolved.
//!

use std::collections::HashMap;

use uc_ast::Hir;
use uc_middle::{
    ty::Ty, Class, ClassKind, Def, DefId, Defs, Enum, EnumVariant, Package, Struct, Var,
};
use uc_name::Identifier;

#[derive(Debug)]
pub struct LoweringInput {
    pub packages: HashMap<Identifier, LoweringInputPackage>,
}

#[derive(Debug)]
pub struct LoweringInputPackage {
    pub files: HashMap<Identifier, Hir>,
}

/// Contains the contextual data needed for (delayed) name resolution.
#[derive(Debug, Default)]
struct ResolverContext {
    /// Name -> Package
    packages: HashMap<Identifier, DefId>,
    /// Package -> Name -> Class/Interface
    package_classes: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Name -> Class/Interface
    classes_interfaces: HashMap<Identifier, DefId>,
    /// Name -> Enum/Struct/Class/Interface
    global_ty_defs: HashMap<Identifier, DefId>,
    /// Class -> Name -> Struct/Enum
    scoped_ty_defs: HashMap<DefId, HashMap<Identifier, DefId>>,
    /// Name -> Enum variants
    global_values: HashMap<Identifier, DefId>,
    /// Class/Struct -> Name -> Var
    scoped_vars: HashMap<DefId, HashMap<Identifier, DefId>>,
}

#[derive(Debug, Default)]
struct HirBackrefs<'hir> {
    files: HashMap<DefId, &'hir uc_ast::Hir>,
    structs: HashMap<DefId, &'hir uc_ast::StructDef>,
    enums: HashMap<DefId, &'hir uc_ast::EnumDef>,
    vars: HashMap<DefId, (&'hir uc_ast::VarDef, usize)>,
    consts: HashMap<DefId, &'hir uc_ast::ConstDef>,
    funcs: HashMap<DefId, &'hir uc_ast::FuncDef>,
    states: HashMap<DefId, &'hir uc_ast::StateDef>,
    args: HashMap<DefId, &'hir uc_ast::FuncArg>,
    locals: HashMap<DefId, (&'hir uc_ast::LocalDef, usize)>,
}

#[derive(Debug)]
struct LoweringContext<'defs> {
    defs: &'defs mut Defs,
    resolver: ResolverContext,
}

impl AsMut<Defs> for LoweringContext<'_> {
    fn as_mut(&mut self) -> &'_ mut Defs {
        &mut self.defs
    }
}

impl<'defs> LoweringContext<'defs> {
    fn add_def<F: FnOnce(&mut Self, DefId) -> Def>(&mut self, f: F) -> DefId {
        Defs::add_def(self, f)
    }

    fn lookup_class(&self, name: &Identifier) -> DefId {
        *self.resolver.classes_interfaces.get(name).unwrap()
    }

    fn lookup_global_struct(&self, name: &Identifier) -> DefId {
        let def_id = *self.resolver.global_ty_defs.get(name).unwrap();
        assert!(matches!(self.defs.get_def(def_id), Def::Struct(_)));
        def_id
    }

    fn lookup_scoped_ty(&self, scope_class: &Identifier, ty_name: &Identifier) -> DefId {
        let class = self.lookup_class(scope_class);
        self.lookup_ty_in_scope(class, ty_name)
    }

    fn lookup_ty_in_scope(&self, mut scope: DefId, ty_name: &Identifier) -> DefId {
        loop {
            match self
                .resolver
                .scoped_ty_defs
                .get(&scope)
                .unwrap()
                .get(ty_name)
            {
                Some(&d) => break d,
                None => {
                    scope = match self.parent_scope(scope) {
                        Some(s) => s,
                        None => panic!("ty not found"),
                    }
                }
            }
        }
    }

    fn parent_scope(&self, scope: DefId) -> Option<DefId> {
        match self.defs.get_def(scope) {
            Def::Class(c) => match c.kind.as_ref().unwrap() {
                ClassKind::Class { extends, .. } => *extends,
                ClassKind::Interface { extends } => *extends,
            },
            Def::Struct(c) => Some(c.owning_class),
            _ => panic!("not a scope"),
        }
    }

    fn run(&mut self, input: &LoweringInput) {
        let mut backrefs = HirBackrefs::default();
        self.create_packages_class_frames(&input.packages, &mut backrefs);
        self.discover_items(&mut backrefs);
        self.fixup_extends(&backrefs);
    }

    /// Create DefIds for packages and classes
    fn create_packages_class_frames<'hir>(
        &mut self,
        packages: &'hir HashMap<Identifier, LoweringInputPackage>,
        backrefs: &mut HirBackrefs<'hir>,
    ) {
        let _package_defs = packages
            .iter()
            .map(|(pack_name, package)| {
                self.add_def(|this, pack_id| {
                    this.resolver.packages.insert(pack_name.clone(), pack_id);
                    Def::Package(Box::new(Package {
                        def_id: pack_id,
                        name: pack_name.clone(),
                        classes: package
                            .files
                            .iter()
                            .map(|(file_name, hir)| {
                                let def_id = this.lower_class(file_name, hir, pack_id);
                                backrefs.files.insert(def_id, hir);
                                def_id
                            })
                            .collect(),
                    }))
                })
            })
            .collect::<Vec<_>>();
    }

    fn lower_class<'hir>(
        &mut self,
        file_name: &Identifier,
        hir: &'hir Hir,
        pack_id: DefId,
    ) -> DefId {
        self.add_def(|this, file_id| {
            this.resolver
                .classes_interfaces
                .insert(file_name.clone(), file_id);

            let ty = match hir.header.kind {
                uc_ast::ClassHeader::Class { .. } => Ty::object_from(file_id),
                uc_ast::ClassHeader::Interface { .. } => Ty::interface_from(file_id),
            };
            Def::Class(Box::new(Class {
                def_id: file_id,
                name: file_name.clone(),
                package: pack_id,
                self_ty: ty,
                flags: hir.header.mods.flags,
                kind: None,
                items: vec![],
            }))
        })
    }

    /// Lower enums, create DefIDs for structs, enums, consts, functions, vars
    fn discover_items<'hir>(&mut self, backrefs: &'hir mut HirBackrefs) {
        for (&class_id, &hir) in &backrefs.files {
            let mut resolving = HashMap::new();
            let var_ids = hir
                .vars
                .iter()
                .flat_map(|var_def| {
                    let var_dids =
                        self.lower_var(class_id, var_def, &mut backrefs.vars, &mut resolving);
                    var_dids.to_vec()
                })
                .collect::<Box<[_]>>();
            self.resolver.scoped_vars.insert(class_id, resolving);
            self.defs
                .get_class_mut(class_id)
                .items
                .extend_from_slice(&var_ids);

            let enums = hir
                .enums
                .iter()
                .map(|enum_def| {
                    let lowered = self.lower_enum(class_id, enum_def);
                    backrefs.enums.insert(lowered.1, enum_def);
                    lowered
                })
                .collect::<Vec<_>>();
            self.defs
                .get_class_mut(class_id)
                .items
                .extend(enums.iter().map(|(_, id)| *id));
            self.resolver
                .scoped_ty_defs
                .entry(class_id)
                .or_insert_with(HashMap::default)
                .extend(enums.into_iter());

            let structs = hir
                .structs
                .iter()
                .map(|struct_def| {
                    let (name, def_id, resolution) =
                        self.lower_struct(class_id, struct_def, &mut backrefs.vars);
                    backrefs.structs.insert(def_id, struct_def);
                    self.resolver.scoped_vars.insert(def_id, resolution);
                    (name, def_id)
                })
                .collect::<Vec<_>>();
            self.defs
                .get_class_mut(class_id)
                .items
                .extend(structs.iter().map(|(_, id)| *id));
            self.resolver
                .scoped_ty_defs
                .entry(class_id)
                .or_insert_with(HashMap::default)
                .extend(structs.into_iter());
        }
    }

    fn fixup_extends<'hir>(&mut self, backrefs: &'hir HirBackrefs) {
        for (&class_id, &hir) in &backrefs.files {
            match &hir.header.kind {
                uc_ast::ClassHeader::Class {
                    extends,
                    implements,
                    within,
                } => {
                    let extends = extends.as_ref().map(|n| self.lookup_class(n));
                    let implements = implements.iter().map(|n| self.lookup_class(n)).collect();
                    let within = within.as_ref().map(|n| self.lookup_class(n));
                    let c = self.defs.get_class_mut(class_id);
                    c.kind = Some(ClassKind::Class {
                        extends,
                        implements,
                        within,
                    })
                }
                uc_ast::ClassHeader::Interface { extends } => {
                    let extends = extends.as_ref().map(|n| self.lookup_class(n));
                    let c = self.defs.get_class_mut(class_id);
                    c.kind = Some(ClassKind::Interface { extends })
                }
            }
        }

        for (&struct_id, &struct_def) in &backrefs.structs {
            let extends = match &struct_def.extends.as_deref() {
                None => None,
                Some([name]) => Some(self.lookup_global_struct(name)),
                Some([class, name]) => Some(self.lookup_scoped_ty(class, name)),
                Some([..]) => panic!("too many name parts"),
            };
            self.defs.get_struct_mut(struct_id).extends = extends;
        }
    }

    fn lower_enum(&mut self, class_id: DefId, en: &uc_ast::EnumDef) -> (Identifier, DefId) {
        let def_id = self.add_def(|this, enum_id| {
            this.resolver
                .global_ty_defs
                .insert(en.name.clone(), enum_id);
            Def::Enum(Box::new(Enum {
                def_id: enum_id,
                owning_class: class_id,
                self_ty: Ty::enum_from(enum_id),
                name: en.name.clone(),
                variants: en
                    .variants
                    .iter()
                    .enumerate()
                    .map(|(idx, name)| {
                        this.add_def(|_, var_id| {
                            Def::EnumVariant(Box::new(EnumVariant {
                                def_id: var_id,
                                owning_enum: enum_id,
                                name: name.clone(),
                                idx: idx.try_into().expect("too many variants"),
                            }))
                        })
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            }))
        });
        let name = en.name.clone();
        (name, def_id)
    }

    fn lower_struct<'hir>(
        &mut self,
        class_id: DefId,
        struct_def: &'hir uc_ast::StructDef,
        vars: &mut HashMap<DefId, (&'hir uc_ast::VarDef, usize)>,
    ) -> (Identifier, DefId, HashMap<Identifier, DefId>) {
        let mut resolving = HashMap::new();
        let struct_id = self.add_def(|this, struct_id| {
            this.resolver
                .global_ty_defs
                .insert(struct_def.name.clone(), struct_id);

            let var_ids = struct_def
                .fields
                .iter()
                .flat_map(|var_def| {
                    let var_dids = this.lower_var(struct_id, var_def, vars, &mut resolving);
                    var_dids.to_vec()
                })
                .collect::<Box<[_]>>();

            Def::Struct(Box::new(Struct {
                def_id: struct_id,
                owning_class: class_id,
                name: struct_def.name.clone(),
                self_ty: Ty::struct_from(struct_id),
                flags: struct_def.mods.flags,
                extends: None,
                vars: var_ids,
            }))
        });
        let struct_name = struct_def.name.clone();
        (struct_name, struct_id, resolving)
    }

    fn lower_var<'hir>(
        &mut self,
        owner_id: DefId,
        var_def: &'hir uc_ast::VarDef,
        vars: &mut HashMap<DefId, (&'hir uc_ast::VarDef, usize)>,
        resolving: &mut HashMap<Identifier, DefId>,
    ) -> Vec<DefId> {
        var_def
            .names
            .iter()
            .enumerate()
            .map(|(idx, inst)| {
                self.add_def(|_, var_id| {
                    vars.insert(var_id, (var_def, idx));
                    resolving.insert(inst.name.clone(), var_id);
                    Def::Var(Box::new(Var {
                        def_id: var_id,
                        name: inst.name.clone(),
                        owner: owner_id,
                        flags: var_def.mods.flags,
                        sig: None,
                    }))
                })
            })
            .collect()
    }
}

pub fn lower(input: LoweringInput) -> Defs {
    let mut defs = Defs::new();

    let mut l_ctx = LoweringContext {
        defs: &mut defs,
        resolver: ResolverContext::default(),
    };

    l_ctx.run(&input);

    println!("{:?}", &defs);

    defs
}

// Todo: This needs to take into account the environment when looking at delegates
fn ast_ty_to_ty(ctx: &Defs, ty: uc_ast::Ty) -> Ty {
    match ty {
        uc_ast::Ty::Simple(ident) => {
            if ident == *"int" {
                Ty::INT
            } else if ident == *"float" {
                Ty::FLOAT
            } else if ident == *"bool" {
                Ty::BOOL
            } else if ident == *"byte" {
                Ty::BYTE
            } else if ident == *"string" {
                Ty::STRING
            } else if ident == *"name" {
                Ty::NAME
            } else {
                todo!()
            }
        }
        uc_ast::Ty::Qualified(_) => todo!(),
        uc_ast::Ty::Array(arr_ty) => {
            // This is filtered in the parser
            assert!(!matches!(&*arr_ty, uc_ast::Ty::Array(_)));
            Ty::array_from(ast_ty_to_ty(ctx, *arr_ty))
        }
        uc_ast::Ty::Class(_) => todo!(),
        uc_ast::Ty::Delegate(_) => todo!(),
    }
}
