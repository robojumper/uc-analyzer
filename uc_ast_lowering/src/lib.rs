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

use std::{collections::HashMap, str::FromStr};

use resolver::{ResolutionError, ResolverContext};
use uc_ast::Hir;
use uc_def::{FuncFlags, VarFlags};
use uc_middle::{
    ty::Ty, Class, ClassKind, Const, Def, DefId, Defs, Enum, EnumVariant, Function, Operator,
    Package, State, Struct, Var, VarSig,
};
use uc_name::Identifier;

mod resolver;

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
        self.defs
    }
}

impl<'defs> LoweringContext<'defs> {
    fn add_def<F: FnOnce(&mut Self, DefId) -> Def>(&mut self, f: F) -> DefId {
        Defs::add_def(self, f)
    }

    fn err_panic(&self, err: ResolutionError) {
        match err {
            ResolutionError::ExistsInExactScope => todo!(),
            ResolutionError::InvalidShadowing => todo!(),
            ResolutionError::InvalidAmbiguity(defs) => panic!("{:?}", defs),
            ResolutionError::NotFound => todo!(),
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
                    this.resolver
                        .add_package(pack_name.clone(), pack_id)
                        .unwrap();
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
                .add_class(pack_id, file_name.clone(), file_id)
                .unwrap();

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

    fn fixup_extends<'hir>(&mut self, backrefs: &'hir HirBackrefs) {
        for (&class_id, &hir) in &backrefs.files {
            let package = self.defs.get_package_of_ty(class_id);
            match &hir.header.kind {
                uc_ast::ClassHeader::Class {
                    extends,
                    implements,
                    within,
                } => {
                    let extends = extends
                        .as_ref()
                        .map(|n| self.resolver.get_ty(package, self.defs, n).unwrap());
                    let implements = implements
                        .iter()
                        .map(|n| self.resolver.get_ty(package, self.defs, n).unwrap())
                        .collect();
                    let within = within
                        .as_ref()
                        .map(|n| self.resolver.get_ty(package, self.defs, n).unwrap());
                    let c = self.defs.get_class_mut(class_id);
                    c.kind = Some(ClassKind::Class {
                        extends,
                        implements,
                        within,
                    })
                }
                uc_ast::ClassHeader::Interface { extends } => {
                    let extends = extends
                        .as_ref()
                        .map(|n| self.resolver.get_ty(package, self.defs, n).unwrap());
                    let c = self.defs.get_class_mut(class_id);
                    c.kind = Some(ClassKind::Interface { extends })
                }
            }
        }

        for (&struct_id, &struct_def) in &backrefs.structs {
            let package = self.defs.get_package_of_ty(struct_id);
            let extends = match &struct_def.extends.as_deref() {
                None => None,
                Some([name]) => Some(self.resolver.get_ty(package, self.defs, name).unwrap()),
                Some([class, name]) => Some(
                    self.resolver
                        .get_ty_in(package, self.defs, class, name)
                        .unwrap(),
                ),
                Some([..]) => panic!("too many name parts"),
            };
            self.defs.get_struct_mut(struct_id).extends = extends;
        }
    }

    fn lower_enum(&mut self, class_id: DefId, en: &uc_ast::EnumDef) -> DefId {
        self.add_def(|this, enum_id| {
            this.resolver
                .add_scoped_ty(class_id, en.name.clone(), enum_id)
                .unwrap_or_else(|e| this.err_panic(e));
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
                        this.add_def(|this, var_id| {
                            this.resolver
                                .add_global_value(name.clone(), var_id)
                                .unwrap_or_else(|e| panic!("conflict: {}", name));
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
                .add_scoped_ty(class_id, struct_def.name.clone(), struct_id)
                .unwrap();

            let var_ids = struct_def
                .fields
                .iter()
                .flat_map(|var_def| this.lower_var(struct_id, var_def, vars))
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
            Def::Const(Box::new(Const {
                def_id: const_id,
                name: const_def.name.clone(),
                owner: owner_id,
            }))
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
    ///         return self.__Delegate_OnCompletedDelegate(arg);
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
            funcs.insert(func_id, func_def);

            if func_def
                .mods
                .flags
                .intersects(FuncFlags::OPERATOR | FuncFlags::PREOPERATOR | FuncFlags::POSTOPERATOR)
            {
                let op = match &func_def.name {
                    uc_ast::FuncName::Oper(op) => *op,
                    uc_ast::FuncName::Iden(i) => {
                        panic!("op with regular function name {} not supported", i)
                    }
                };

                let op_def = Def::Operator(Box::new(Operator {
                    def_id: func_id,
                    op,
                    owning_class: owner_id,
                    flags: func_def.mods.flags,
                    sig: None,
                }));
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
                        let var = Def::Var(Box::new(Var {
                            def_id: var_id,
                            name: name.clone(),
                            owner: owner_id,
                            flags: VarFlags::empty(),
                            sig: Some(VarSig {
                                ty: Ty::delegate_from(func_id),
                                dim: None,
                            }),
                        }));
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
                Def::Function(Box::new(Function {
                    def_id: func_id,
                    name: func_name,
                    owner: owner_id,
                    flags: func_def.mods.flags,
                    delegate_prop: var_id,
                    sig: None,
                    contents: None,
                }))
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

            Def::State(Box::new(State {
                def_id: state_id,
                name: state_def.name.clone(),
                owner: owner_id,
                funcs,
                contents: None,
            }))
        })
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
fn ast_ty_to_ty(ctx: &Defs, ty: uc_ast::Ty, scope: DefId) -> Ty {
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
            Ty::array_from(ast_ty_to_ty(ctx, *arr_ty, scope))
        }
        uc_ast::Ty::Class(_) => todo!(),
        uc_ast::Ty::Delegate(_) => todo!(),
    }
}
