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

use std::{collections::HashMap, str::FromStr};

use resolver::ResolverContext;
use uc_ast::{DimCount, Hir};
use uc_def::{ArgFlags, ClassFlags, FuncFlags, StructFlags, VarFlags};
use uc_files::Span;
use uc_middle::{
    ty::Ty, Class, ClassKind, Const, ConstVal, DefId, DefKind, Defs, Enum, EnumVariant, FuncArg,
    FuncContents, FuncSig, Function, Local, Operator, Package, ScopeWalkKind, State, Struct, Var,
};
use uc_name::Identifier;

mod body;
pub mod resolver;

#[derive(Debug)]
pub struct LoweringInput {
    pub packages: HashMap<Identifier, LoweringInputPackage>,
}

#[derive(Debug)]
pub struct LoweringInputPackage {
    pub files: HashMap<Identifier, Hir>,
}

#[derive(Debug, Default)]
struct HirBackrefs<'hir> {
    files: HashMap<DefId, &'hir uc_ast::Hir>,
    structs: HashMap<DefId, &'hir uc_ast::StructDef>,
    enums: HashMap<DefId, &'hir uc_ast::EnumDef>,
    vars: HashMap<DefId, (&'hir uc_ast::VarDef, usize)>,
    consts: HashMap<DefId, &'hir uc_ast::ConstDef>,
    funcs: HashMap<DefId, &'hir uc_ast::FuncDef>,
    ops: HashMap<DefId, &'hir uc_ast::FuncDef>,
    states: HashMap<DefId, &'hir uc_ast::StateDef>,
}

#[derive(Debug, Default)]
pub struct SpecialItems {
    /// The DefId of the Core.Object class
    pub object_id: Option<DefId>,
    /// The DefId of the Core.Interface class
    pub interface_id: Option<DefId>,
    /// The DefId of the synthetically generated __DynArrayIterator iterator
    pub dyn_array_iterator: Option<DefId>,
    /// Side table for native iterator functions
    pub iterator_table: Vec<DefId>,
}

#[derive(Debug)]
struct LoweringContext<'defs> {
    defs: &'defs mut Defs,
    special_items: SpecialItems,
    resolver: ResolverContext,
}

impl AsMut<Defs> for LoweringContext<'_> {
    fn as_mut(&mut self) -> &'_ mut Defs {
        self.defs
    }
}

impl<'defs> LoweringContext<'defs> {
    fn add_def<F: FnOnce(&mut Self, DefId) -> (DefKind, Option<Span>)>(&mut self, f: F) -> DefId {
        Defs::add_def(self, f)
    }

    fn run(&mut self, input: &LoweringInput) {
        let mut backrefs = HirBackrefs::default();
        self.create_packages_class_frames(&input.packages, &mut backrefs);
        self.discover_items(&mut backrefs);
        self.fixup_extends(&backrefs);
        self.add_builtin_items();
        self.resolve_sigs(&backrefs);
        //println!("{:?}", self.defs);
        self.lower_bodies(&backrefs.funcs);
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
                    this.resolver
                        .add_package(pack_name.clone(), pack_id)
                        .unwrap();
                    (
                        DefKind::Package(Package {
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
                        }),
                        None,
                    )
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
                .add_class(pack_id, file_name.clone(), file_id)
                .unwrap();

            let ty = match hir.header.kind {
                uc_ast::ClassHeader::Class { .. } => Ty::object_from(file_id),
                uc_ast::ClassHeader::Interface { .. } => Ty::interface_from(file_id),
            };
            (
                DefKind::Class(Box::new(Class {
                    name: file_name.clone(),
                    package: pack_id,
                    self_ty: ty,
                    flags: hir.header.mods.flags,
                    dependson: Box::new([]),
                    kind: None,
                    items: vec![],
                })),
                Some(hir.header.span),
            )
        })
    }

    /// Lower enums, create DefIDs for structs, enums, consts, functions, vars
    fn discover_items<'hir>(&mut self, backrefs: &'hir mut HirBackrefs) {
        for (&class_id, &hir) in &backrefs.files {
            let const_ids = hir
                .consts
                .iter()
                .map(|const_def| self.lower_const(class_id, const_def, &mut backrefs.consts))
                .collect::<Box<[_]>>();
            self.defs
                .get_class_mut(class_id)
                .items
                .extend_from_slice(&const_ids);

            let var_ids = hir
                .vars
                .iter()
                .flat_map(|var_def| self.lower_var(class_id, var_def, &mut backrefs.vars))
                .collect::<Box<[_]>>();
            self.defs
                .get_class_mut(class_id)
                .items
                .extend_from_slice(&var_ids);

            let enums = hir
                .enums
                .iter()
                .map(|enum_def| {
                    let lowered = self.lower_enum(class_id, enum_def);
                    backrefs.enums.insert(lowered, enum_def);
                    lowered
                })
                .collect::<Vec<_>>();
            self.defs
                .get_class_mut(class_id)
                .items
                .extend_from_slice(&enums);

            let structs = hir
                .structs
                .iter()
                .map(|struct_def| {
                    let def_id = self.lower_struct(class_id, struct_def, &mut backrefs.vars);
                    backrefs.structs.insert(def_id, struct_def);
                    def_id
                })
                .collect::<Vec<_>>();
            self.defs
                .get_class_mut(class_id)
                .items
                .extend_from_slice(&structs);

            let funcs = hir
                .funcs
                .iter()
                .map(|func_def| {
                    self.lower_func(class_id, func_def, &mut backrefs.funcs, &mut backrefs.ops)
                })
                .collect::<Vec<_>>();
            self.defs
                .get_class_mut(class_id)
                .items
                .extend_from_slice(&funcs);

            let states = hir
                .states
                .iter()
                .map(|state_def| {
                    self.lower_state(
                        class_id,
                        state_def,
                        &mut backrefs.states,
                        &mut backrefs.funcs,
                    )
                })
                .collect::<Vec<_>>();
            self.defs
                .get_class_mut(class_id)
                .items
                .extend_from_slice(&states);
        }
    }

    fn add_builtin_items(&mut self) {
        // Add the classes that exist in native engine code, but aren't
        // declared in UnrealScript. This list is incomplete and may require
        // further additions.

        let get_package = |this: &mut Self, package: &str| {
            let name = Identifier::from_str(package).unwrap();
            this.resolver
                .get_package(&name)
                .unwrap_or_else(|e| panic!("{} package missing: {:?}", package, e))
        };

        let get_class = |this: &mut Self, package: &str, class: &str| {
            let pack = Identifier::from_str(package).unwrap();
            let cls = Identifier::from_str(class).unwrap();
            this.resolver
                .get_ty_in(None, this.defs, &pack, &cls)
                .expect("Object missing")
        };

        let core = get_package(self, "Core");
        let engine = get_package(self, "Engine");
        let unrealed = get_package(self, "UnrealEd");

        let object_id = get_class(self, "Core", "Object");
        self.special_items.object_id = Some(object_id);

        let add_class = |this: &mut Self, package, class_name: &str, parent| {
            let ident = Identifier::from_str(class_name).unwrap();
            this.add_def(|this, class_id| {
                this.resolver
                    .add_class(package, ident.clone(), class_id)
                    .unwrap();
                (
                    DefKind::Class(Box::new(Class {
                        name: ident,
                        package,
                        self_ty: Ty::object_from(class_id),
                        flags: ClassFlags::empty(),
                        dependson: Box::new([]),
                        kind: Some(ClassKind::Class {
                            extends: Some(parent),
                            implements: Box::new([]),
                            within: None,
                        }),
                        items: vec![],
                    })),
                    None,
                )
            })
        };

        add_class(self, core, "Function", object_id);
        add_class(self, core, "Package", object_id);
        add_class(self, core, "Polys", object_id);
        add_class(self, core, "Property", object_id);

        let static_mesh_id = add_class(self, engine, "StaticMesh", object_id);
        add_class(self, engine, "FracturedStaticMesh", static_mesh_id);

        let level_base_id = add_class(self, engine, "LevelBase", object_id);
        add_class(self, engine, "Level", level_base_id);
        add_class(self, engine, "PendingLevel", level_base_id);

        add_class(self, engine, "Client", object_id);
        add_class(self, engine, "Model", object_id);
        add_class(self, engine, "ShadowMap1D", object_id);
        add_class(self, engine, "World", object_id);

        add_class(self, unrealed, "ByteCodeSerializer", object_id);
        add_class(self, unrealed, "TransBuffer", object_id);
        add_class(self, unrealed, "TextBuffer", object_id);

        let player_id = get_class(self, "Engine", "Player");
        let net_conn_id = add_class(self, engine, "NetConnection", player_id);
        add_class(self, engine, "ChildConnection", net_conn_id);

        self.add_def(|this, struct_id| {
            let map = Identifier::from_str("Map").unwrap();
            this.resolver.add_scoped_ty(map.clone(), object_id);
            (
                DefKind::Struct(Struct {
                    name: map,
                    owning_class: object_id,
                    self_ty: Ty::struct_from(struct_id),
                    flags: StructFlags::empty(),
                    extends: None,
                    vars: Box::new([]),
                }),
                None,
            )
        });

        // For consistency, add the native iterator as a function
        let iterator_id = self.add_def(|this, func_id| {
            let iterator_idx = this.special_items.iterator_table.len().try_into().unwrap();
            this.special_items.iterator_table.push(func_id);
            let array_arg = this.add_def(|_, _| {
                (
                    DefKind::FuncArg(FuncArg {
                        name: Identifier::from_str("array_value").unwrap(),
                        owner: func_id,
                        flags: ArgFlags::OUT | ArgFlags::CONST,
                        ty: Ty::dyn_array_from(Ty::PLACEHOLDER),
                    }),
                    None,
                )
            });
            let val_arg = this.add_def(|_, _| {
                (
                    DefKind::FuncArg(FuncArg {
                        name: Identifier::from_str("element").unwrap(),
                        owner: func_id,
                        flags: ArgFlags::OUTONLY,
                        ty: Ty::PLACEHOLDER,
                    }),
                    None,
                )
            });
            let idx_arg = this.add_def(|_, _| {
                (
                    DefKind::FuncArg(FuncArg {
                        name: Identifier::from_str("idx").unwrap(),
                        owner: func_id,
                        flags: ArgFlags::OUTONLY | ArgFlags::OPTIONAL,
                        ty: Ty::INT,
                    }),
                    None,
                )
            });
            (
                DefKind::Function(Function {
                    name: Identifier::from_str("__DynArrayIterator").unwrap(),
                    owner: object_id,
                    flags: FuncFlags::NATIVE | FuncFlags::ITERATOR | FuncFlags::STATIC,
                    delegate_prop: None,
                    sig: Some(FuncSig {
                        ret_ty: Some(Ty::iterator(Ty::PLACEHOLDER, iterator_idx)),
                        args: Box::new([array_arg, val_arg, idx_arg]),
                    }),
                    contents: None,
                }),
                None,
            )
        });
        self.special_items.dyn_array_iterator = Some(iterator_id);
    }

    fn fixup_extends<'hir>(&mut self, backrefs: &'hir HirBackrefs) {
        for (&class_id, &hir) in &backrefs.files {
            match &hir.header.kind {
                uc_ast::ClassHeader::Class {
                    extends,
                    implements,
                    within,
                } => {
                    let extends = extends
                        .as_ref()
                        .map(|n| self.resolver.get_ty(class_id, self.defs, n).unwrap());
                    let implements = implements
                        .iter()
                        .map(|n| self.resolver.get_ty(class_id, self.defs, n).unwrap())
                        .collect();
                    let within = within
                        .as_ref()
                        .map(|n| self.resolver.get_ty(class_id, self.defs, n).unwrap());
                    let c = self.defs.get_class_mut(class_id);
                    c.kind = Some(ClassKind::Class {
                        extends,
                        implements,
                        within,
                    })
                }
                uc_ast::ClassHeader::Interface { extends } => {
                    // Honor an explicit extend on the interface. Otherwise extend `Interface`,
                    let extends = extends
                        .as_ref()
                        .map(|n| self.resolver.get_ty(class_id, self.defs, n).unwrap())
                        .or_else(|| {
                            if &hir.header.name == "Interface" {
                                self.special_items.interface_id = Some(class_id);
                                None
                                /*self.resolver.get_ty_in(
                                    None,
                                    self.defs,
                                    &Identifier::from_str("Core").unwrap(),
                                    &Identifier::from_str("Object").unwrap(),
                                )*/
                            } else {
                                Some(
                                    self.resolver
                                        .get_ty(
                                            class_id,
                                            self.defs,
                                            &Identifier::from_str("Interface").unwrap(),
                                        )
                                        .unwrap(),
                                )
                            }
                        });
                    let c = self.defs.get_class_mut(class_id);
                    c.kind = Some(ClassKind::Interface { extends })
                }
            }
            let dependson = hir
                .header
                .dependson
                .iter()
                .map(|n| self.resolver.get_ty(class_id, self.defs, n).unwrap())
                .collect();
            self.defs.get_class_mut(class_id).dependson = dependson;
        }

        for (&struct_id, &struct_def) in &backrefs.structs {
            let extends = match &struct_def.extends.as_deref() {
                None => None,
                Some([name]) => Some(self.resolver.get_ty(struct_id, self.defs, name).unwrap()),
                Some([class, name]) => Some(
                    self.resolver
                        .get_ty_in(Some(struct_id), self.defs, class, name)
                        .unwrap(),
                ),
                Some([..]) => panic!("too many name parts"),
            };
            self.defs.get_struct_mut(struct_id).extends = extends;
        }
    }

    fn resolve_dim(&self, dim: &uc_ast::DimCount, scope: DefId) -> u16 {
        match dim {
            DimCount::Complex(n) => match &**n {
                [single] => {
                    if let Ok(const_id) = self.resolver.get_scoped_const(
                        scope,
                        self.defs,
                        ScopeWalkKind::Definitions,
                        single,
                    ) {
                        let def = self.defs.get_const(const_id);
                        match &def.val {
                            ConstVal::Num(n) => *n as u16,
                            ConstVal::Other => panic!("invalid const value"),
                        }
                    } else if let Ok(en_def) = self.resolver.get_ty(scope, self.defs, single) {
                        self.defs.get_enum(en_def).variants.len() as u16
                    } else {
                        panic!("invalid const static array bound")
                    }
                }
                [en, enum_count] => {
                    // IDGAF just stop throwing errors AAA
                    if enum_count != "EnumCount" && !enum_count.as_ref().ends_with("_MAX") {
                        panic!("yet another way to specify enums: {}", enum_count)
                    }
                    let en_def = self
                        .resolver
                        .get_ty(scope, self.defs, en)
                        .unwrap_or_else(|e| panic!("failed to find enum {:?}: {:?}", en, e));
                    self.defs.get_enum(en_def).variants.len() as u16
                }
                x => panic!("invalid static array length specification: {:?}", x),
            },
            DimCount::Number(n) => *n as u16,
            DimCount::None => panic!(),
        }
    }

    fn resolve_sigs<'hir>(&mut self, backrefs: &HirBackrefs<'hir>) {
        for var_ref in &backrefs.vars {
            let inner_ty = self.decode_ast_ty(&var_ref.1 .0.ty, *var_ref.0).unwrap();
            let dim = &var_ref.1 .0.names[var_ref.1 .1].count;
            let ty = match dim {
                DimCount::None => inner_ty,
                x => Ty::stat_array_from(inner_ty, self.resolve_dim(x, *var_ref.0)),
            };
            self.defs.get_var_mut(*var_ref.0).ty = Some(ty);
        }

        for (&func_id, &func_ref) in &backrefs.funcs {
            let mut ret_ty = func_ref
                .sig
                .ret_ty
                .as_ref()
                .map(|ty| self.decode_ast_ty(ty, func_id).unwrap());
            let args = func_ref
                .sig
                .args
                .iter()
                .map(|arg| {
                    self.add_def(|this, arg_id| {
                        this.resolver
                            .add_scoped_var(func_id, arg.name.clone(), arg_id)
                            .expect("duplicate arg");
                        (
                            DefKind::FuncArg(FuncArg {
                                name: arg.name.clone(),
                                owner: func_id,
                                flags: arg.mods.flags,
                                ty: this.decode_ast_ty(&arg.ty, func_id).unwrap(),
                            }),
                            Some(arg.span),
                        )
                    })
                })
                .collect::<Box<_>>();

            // Special treatment for native iterators, where the second out object type
            // is determined by the class in the first argument
            if func_ref.mods.flags.contains(FuncFlags::ITERATOR) {
                assert!(func_ref.mods.flags.contains(FuncFlags::NATIVE));
                let iterator_idx = self.special_items.iterator_table.len().try_into().unwrap();
                self.special_items.iterator_table.push(func_id);
                if args.len() >= 2
                    && self.defs.get_arg(args[0]).ty.is_class()
                    && self.defs.get_arg(args[1]).ty.is_object()
                {
                    self.defs.get_arg_mut(args[1]).ty = Ty::PLACEHOLDER;
                    ret_ty = Some(Ty::iterator(Ty::PLACEHOLDER, iterator_idx));
                }
            }
            self.defs.get_func_mut(func_id).sig = Some(FuncSig { ret_ty, args });

            if let Some(body) = &func_ref.body {
                let locals = body
                    .locals
                    .iter()
                    .flat_map(|local| {
                        let local_ty = self.decode_ast_ty(&local.ty, func_id).unwrap();
                        let mut insts = vec![];
                        for inst in &local.names {
                            let inst_id = self.add_def(|this, local_id| {
                                this.resolver
                                    .add_scoped_var(func_id, inst.name.clone(), local_id)
                                    .expect("duplicate local");
                                let ty = match &inst.count {
                                    DimCount::None => local_ty,
                                    x => {
                                        Ty::stat_array_from(local_ty, this.resolve_dim(x, func_id))
                                    }
                                };
                                (
                                    DefKind::Local(Local {
                                        name: inst.name.clone(),
                                        owner: func_id,
                                        ty,
                                    }),
                                    Some(inst.span),
                                )
                            });
                            insts.push(inst_id);
                        }
                        insts
                    })
                    .collect::<Box<_>>();
                self.defs.get_func_mut(func_id).contents = Some(FuncContents {
                    locals,
                    statements: None,
                });
            }
        }
    }

    fn lower_enum(&mut self, class_id: DefId, enum_def: &uc_ast::EnumDef) -> DefId {
        self.add_def(|this, enum_id| {
            this.resolver.add_scoped_ty(enum_def.name.clone(), enum_id);
            (
                DefKind::Enum(Enum {
                    owning_class: class_id,
                    self_ty: Ty::enum_from(enum_id),
                    name: enum_def.name.clone(),
                    variants: enum_def
                        .variants
                        .iter()
                        .enumerate()
                        .map(|(idx, &(span, ref name))| {
                            this.add_def(|this, var_id| {
                                this.resolver
                                    .add_global_value(name.clone(), var_id)
                                    .unwrap_or_else(|e| panic!("conflict in {}: {:?}", name, e));
                                (
                                    DefKind::EnumVariant(EnumVariant {
                                        owning_enum: enum_id,
                                        name: name.clone(),
                                        idx: idx.try_into().expect("too many variants"),
                                    }),
                                    Some(span),
                                )
                            })
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                }),
                Some(enum_def.span),
            )
        })
    }

    fn lower_struct<'hir>(
        &mut self,
        class_id: DefId,
        struct_def: &'hir uc_ast::StructDef,
        vars: &mut HashMap<DefId, (&'hir uc_ast::VarDef, usize)>,
    ) -> DefId {
        self.add_def(|this, struct_id| {
            this.resolver
                .add_scoped_ty(struct_def.name.clone(), struct_id);

            let var_ids = struct_def
                .fields
                .iter()
                .flat_map(|var_def| this.lower_var(struct_id, var_def, vars))
                .collect::<Box<[_]>>();

            (
                DefKind::Struct(Struct {
                    owning_class: class_id,
                    name: struct_def.name.clone(),
                    self_ty: Ty::struct_from(struct_id),
                    flags: struct_def.mods.flags,
                    extends: None,
                    vars: var_ids,
                }),
                Some(struct_def.span),
            )
        })
    }

    fn lower_var<'hir>(
        &mut self,
        owner_id: DefId,
        var_def: &'hir uc_ast::VarDef,
        vars: &mut HashMap<DefId, (&'hir uc_ast::VarDef, usize)>,
    ) -> Vec<DefId> {
        var_def
            .names
            .iter()
            .enumerate()
            .map(|(idx, inst)| {
                self.add_def(|this, var_id| {
                    this.resolver
                        .add_scoped_var(owner_id, inst.name.clone(), var_id)
                        .unwrap();
                    vars.insert(var_id, (var_def, idx));
                    (
                        DefKind::Var(Var {
                            name: inst.name.clone(),
                            owner: owner_id,
                            flags: var_def.mods.flags,
                            ty: None,
                        }),
                        Some(var_def.span),
                    )
                })
            })
            .collect()
    }

    fn lower_const<'hir>(
        &mut self,
        owner_id: DefId,
        const_def: &'hir uc_ast::ConstDef,
        consts: &mut HashMap<DefId, &'hir uc_ast::ConstDef>,
    ) -> DefId {
        self.add_def(|this, const_id| {
            this.resolver
                .add_scoped_const(owner_id, const_def.name.clone(), const_id)
                .unwrap();
            consts.insert(const_id, const_def);
            let val = match &const_def.val {
                uc_ast::ConstVal::Int(i) => ConstVal::Num(*i),
                _ => ConstVal::Other,
            };
            (
                DefKind::Const(Const {
                    name: const_def.name.clone(),
                    owner: owner_id,
                    val,
                }),
                Some(const_def.span),
            )
        })
    }

    /// Create a DefId for the function name, its args and locals, and not much more.
    /// As a special case, delegates create two DefIds, because they desugar like this:
    /// ```text
    /// delegate bool OnCompletedDelegate(int arg) {
    ///     return false;
    /// }
    /// // ...
    /// OnCompletedDelegate = SomeFunction;
    ///
    /// // desugars to:
    ///
    /// var delegate<OnCompletedDelegate> __Delegate_OnCompletedDelegate;
    /// function bool OnCompletedDelegate(int arg) {
    ///     if (self.__Delegate_OnCompletedDelegate != none) {
    ///         return (self.__Delegate_OnCompletedDelegate)(arg);
    ///     }
    ///     // default body
    ///     return false;
    /// }
    /// // ...
    /// __Delegate_OnCompletedDelegate = SomeFunction;
    /// ```
    fn lower_func<'hir>(
        &mut self,
        owner_id: DefId,
        func_def: &'hir uc_ast::FuncDef,
        funcs: &mut HashMap<DefId, &'hir uc_ast::FuncDef>,
        ops: &mut HashMap<DefId, &'hir uc_ast::FuncDef>,
    ) -> DefId {
        self.add_def(|this, func_id| {
            if func_def
                .mods
                .flags
                .intersects(FuncFlags::OPERATOR | FuncFlags::PREOPERATOR | FuncFlags::POSTOPERATOR)
            {
                assert!(func_def.mods.flags.contains(FuncFlags::STATIC));
                let op = match &func_def.name {
                    uc_ast::FuncName::Oper(op) => *op,
                    uc_ast::FuncName::Iden(i) => {
                        panic!("op with regular function name {} not supported", i)
                    }
                };

                let op_def = (
                    DefKind::Operator(Operator {
                        op,
                        owning_class: owner_id,
                        flags: func_def.mods.flags,
                        sig: None,
                    }),
                    Some(func_def.span),
                );
                this.resolver.add_scoped_op(owner_id, op, func_id).unwrap();
                ops.insert(func_id, func_def);
                op_def
            } else {
                let func_name = match &func_def.name {
                    uc_ast::FuncName::Oper(_) => {
                        panic!("operator flag missing from {:?}", func_def.name)
                    }
                    uc_ast::FuncName::Iden(i) => i.clone(),
                };

                let var_id = if func_def.mods.flags.contains(FuncFlags::DELEGATE) {
                    Some(this.add_def(|this, var_id| {
                        let name =
                            Identifier::from_str(&("__delegate_".to_owned() + func_name.as_ref()))
                                .unwrap();
                        let var = (
                            DefKind::Var(Var {
                                name: name.clone(),
                                owner: owner_id,
                                flags: VarFlags::empty(),
                                ty: Some(Ty::delegate_from(func_id)),
                            }),
                            Some(func_def.span),
                        );
                        this.resolver
                            .add_scoped_var(owner_id, name, var_id)
                            .unwrap();
                        var
                    }))
                } else {
                    None
                };

                this.resolver
                    .add_scoped_func(owner_id, func_name.clone(), func_id)
                    .unwrap();
                funcs.insert(func_id, func_def);
                (
                    DefKind::Function(Function {
                        name: func_name,
                        owner: owner_id,
                        flags: func_def.mods.flags,
                        delegate_prop: var_id,
                        sig: None,
                        contents: None,
                    }),
                    Some(func_def.span),
                )
            }
        })
    }

    fn lower_state<'hir>(
        &mut self,
        owner_id: DefId,
        state_def: &'hir uc_ast::StateDef,
        states: &mut HashMap<DefId, &'hir uc_ast::StateDef>,
        funcs: &mut HashMap<DefId, &'hir uc_ast::FuncDef>,
    ) -> DefId {
        self.add_def(|this, state_id| {
            states.insert(state_id, state_def);
            let mut ops = HashMap::default();
            let funcs = state_def
                .funcs
                .iter()
                .map(|func_def| this.lower_func(state_id, func_def, funcs, &mut ops))
                .collect();
            assert!(ops.is_empty());

            (
                DefKind::State(State {
                    name: state_def.name.clone(),
                    owner: owner_id,
                    funcs,
                    contents: None,
                }),
                Some(state_def.span),
            )
        })
    }

    fn lower_bodies<'hir>(&mut self, funcs: &HashMap<DefId, &'hir uc_ast::FuncDef>) {
        for (&did, &def) in funcs {
            if let Some(body) = &def.body {
                let body = self.lower_body(did, &body.statements);
                dbg!(&body);
                self.defs
                    .get_func_mut(did)
                    .contents
                    .as_mut()
                    .unwrap()
                    .statements = Some(body);
            }
        }
    }

    fn decode_simple_ty(&self, ident: &Identifier, scope: DefId) -> Option<Ty> {
        if ident == "int" {
            Some(Ty::INT)
        } else if ident == "float" {
            Some(Ty::FLOAT)
        } else if ident == "bool" {
            Some(Ty::BOOL)
        } else if ident == "byte" {
            Some(Ty::BYTE)
        } else if ident == "string" {
            Some(Ty::STRING)
        } else if ident == "name" {
            Some(Ty::NAME)
        } else {
            let ty = self.resolver.get_ty(scope, self.defs, ident).ok()?;
            match &self.defs.get_def(ty).kind {
                DefKind::Class(c) => match c.kind.as_ref().unwrap() {
                    ClassKind::Class { .. } => Some(Ty::object_from(ty)),
                    ClassKind::Interface { .. } => Some(Ty::interface_from(ty)),
                },
                DefKind::Enum(_) => Some(Ty::enum_from(ty)),
                DefKind::Struct(_) => Some(Ty::struct_from(ty)),
                _ => unreachable!(),
            }
        }
    }

    fn decode_ast_ty(&self, ty: &uc_ast::Ty, scope: DefId) -> Option<Ty> {
        match ty {
            uc_ast::Ty::Simple(ident) => self.decode_simple_ty(ident, scope),
            uc_ast::Ty::Qualified(parts) => match &**parts {
                [class, ty] => {
                    let ty = self
                        .resolver
                        .get_ty_in(Some(scope), self.defs, class, ty)
                        .unwrap_or_else(|e| panic!("failed to find ty {:?}: {:?}", ty, e));
                    match &self.defs.get_def(ty).kind {
                        DefKind::Enum(_) => Some(Ty::enum_from(ty)),
                        DefKind::Struct(_) => Some(Ty::struct_from(ty)),
                        _ => panic!("not a valid qualified ty"),
                    }
                }
                x => panic!("invalid qualified type {:?}", x),
            },
            uc_ast::Ty::Array(arr_ty) => {
                // This is filtered in the parser
                assert!(!matches!(&**arr_ty, uc_ast::Ty::Array(_)));
                Some(Ty::dyn_array_from(
                    self.decode_ast_ty(arr_ty, scope).unwrap(),
                ))
            }
            uc_ast::Ty::Class(ident) => {
                let ty = self
                    .resolver
                    .get_ty(scope, self.defs, ident)
                    .unwrap_or_else(|e| panic!("failed to find ty {:?}: {:?}", ident, e));
                match &self.defs.get_def(ty).kind {
                    DefKind::Class(c) => match c.kind.as_ref().unwrap() {
                        ClassKind::Class { .. } => Some(Ty::class_from(ty)),
                        ClassKind::Interface { .. } => Some(Ty::interface_from(ty)),
                    },
                    _ => panic!("not a class"),
                }
            }
            uc_ast::Ty::Delegate(parts) => {
                //let class_scope = self.defs.get_function_defining(scope);
                match &**parts {
                    [func_name] => {
                        let func = self
                            .resolver
                            .get_scoped_func(
                                scope,
                                self.defs,
                                ScopeWalkKind::Definitions,
                                func_name,
                            )
                            .unwrap_or_else(|e| {
                                panic!("failed to find ty {:?}: {:?}", func_name, e)
                            });
                        Some(Ty::delegate_from(func))
                    }
                    [class, func_name] => {
                        let class_def = self
                            .resolver
                            .get_ty(scope, self.defs, class)
                            .unwrap_or_else(|e| {
                                panic!("failed to find ty {:?}: {:?}", func_name, e)
                            });
                        let func = self
                            .resolver
                            .get_scoped_func(
                                class_def,
                                self.defs,
                                ScopeWalkKind::Definitions,
                                func_name,
                            )
                            .unwrap_or_else(|e| {
                                panic!("failed to find ty {:?}: {:?}", func_name, e)
                            });
                        Some(Ty::delegate_from(func))
                    }
                    x => panic!("invalid qualified type {:?}", x),
                }
            }
        }
    }
}

pub fn lower(input: LoweringInput) -> (Defs, ResolverContext) {
    let mut defs = Defs::new();

    let mut l_ctx = LoweringContext {
        defs: &mut defs,
        special_items: SpecialItems::default(),
        resolver: ResolverContext::default(),
    };

    l_ctx.run(&input);

    let resolver = l_ctx.resolver;
    (defs, resolver)
}
