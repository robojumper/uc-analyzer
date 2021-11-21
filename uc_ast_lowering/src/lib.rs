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

use std::collections::HashMap;

use uc_ast::Hir;
use uc_middle::{ty::Ty, Class, Def, DefHierarchy, DefId, Enum, EnumVariant, Interface, Package};
use uc_name::Identifier;

pub struct LoweringInput {
    pub packages: HashMap<Identifier, LoweringInputPackage>,
}

pub struct LoweringInputPackage {
    pub files: HashMap<Identifier, Hir>,
}

/// Contains the contextual data needed for (delayed) name resolution.
/// The general idea is that every lowering step consumes some input data
/// (or residuals), creates DefIds for it, and stashes the rest of the data
/// that couldn't be lowered in form of residuals.
struct LoweringContext {
    packages: HashMap<Identifier, DefId>,
    classes_interfaces: HashMap<Identifier, DefId>,
}

/// For example, these class modifiers refer to other classes we might now have
/// visited yet, so we consume the header, create the class/interface defs with
/// name, ty, flags, and stash these modifiers as identifiers in a ClassResidual.
/// The next lowering step simply looks at these residuals and resolves them.
struct ClassResidual<'a> {
    def_id: DefId,
    extends: Option<Identifier>,
    implements: Vec<Identifier>,
    within: Option<Identifier>,

    enums: &'a [uc_ast::EnumDef],
}

impl LoweringContext {
    fn lookup_class(&self, name: &Identifier) -> DefId {
        *self.classes_interfaces.get(name).unwrap()
    }

    fn lower_class<'a>(
        &mut self,
        defs: &mut DefHierarchy,
        residuals: &mut Vec<ClassResidual<'a>>,
        file_name: &Identifier,
        hir: &'a Hir,
        pack_id: DefId,
    ) -> DefId {
        defs.add_def(self, |_, l_ctx, file_id| {
            let (ret, residual) = match &hir.header.kind {
                uc_ast::ClassHeader::Class {
                    extends,
                    implements,
                    within,
                } => (
                    Def::Class(Box::new(Class {
                        def_id: file_id,
                        name: file_name.clone(),
                        package: pack_id,
                        self_ty: Ty::object_from(file_id),
                        flags: hir.header.mods.flags,
                        extends: None,
                        implements: Box::new([]),
                        within: None,
                        vars: Box::new([]),
                        structs: Box::new([]),
                        enums: Box::new([]),
                        consts: Box::new([]),
                        funcs: Box::new([]),
                    })),
                    ClassResidual {
                        def_id: file_id,
                        extends: extends.clone(),
                        implements: implements.clone(),
                        within: within.clone(),
                        enums: &hir.enums,
                    },
                ),
                uc_ast::ClassHeader::Interface { extends } => (
                    Def::Interface(Box::new(Interface {
                        def_id: file_id,
                        name: file_name.clone(),
                        package: pack_id,
                        self_ty: Ty::interface_from(file_id),
                        flags: hir.header.mods.flags,
                        extends: None,
                        funcs: Box::new([]),
                    })),
                    ClassResidual {
                        def_id: file_id,
                        extends: extends.clone(),
                        implements: vec![],
                        within: None,
                        enums: &[],
                    },
                ),
            };
            l_ctx.classes_interfaces.insert(file_name.clone(), file_id);
            residuals.push(residual);
            ret
        })
    }

    fn lower_enum(
        &mut self,
        defs: &mut DefHierarchy,
        class_id: DefId,
        en: &uc_ast::EnumDef,
    ) -> DefId {
        defs.add_def(self, |defs, l_ctx, enum_id| {
            Def::Enum(Box::new(Enum {
                def_id: enum_id,
                owning_class: class_id,
                name: en.name.clone(),
                variants: en
                    .variants
                    .iter()
                    .enumerate()
                    .map(|(idx, name)| {
                        defs.add_def(&mut *l_ctx, |_, _, var_id| {
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
}

pub fn lower(input: LoweringInput) -> DefHierarchy {
    let mut defs = DefHierarchy::new();
    let mut l_ctx = LoweringContext {
        packages: HashMap::new(),
        classes_interfaces: HashMap::new(),
    };

    let mut class_residuals = vec![];

    // Create the packages and classes
    let package_defs = input
        .packages
        .iter()
        .map(|(pack_name, package)| {
            defs.add_def(&mut l_ctx, |defs, l_ctx, pack_id| {
                l_ctx.packages.insert(pack_name.clone(), pack_id);
                Def::Package(Box::new(Package {
                    def_id: pack_id,
                    name: pack_name.clone(),
                    classes: package
                        .files
                        .iter()
                        .map(|(file_name, hir)| {
                            l_ctx.lower_class(defs, &mut class_residuals, file_name, hir, pack_id)
                        })
                        .collect(),
                }))
            })
        })
        .collect::<Vec<_>>();
    defs.packages = package_defs.into_boxed_slice();

    class_residuals.iter().for_each(|res| {
        // Fix up superclasses and interface implementations
        let extends = res.extends.as_ref().map(|e| l_ctx.lookup_class(e));
        let implements = res
            .implements
            .iter()
            .map(|e| l_ctx.lookup_class(e))
            .collect();
        let within = res.within.as_ref().map(|e| l_ctx.lookup_class(e));
        // Lower enums
        let enums = res
            .enums
            .iter()
            .map(|e| l_ctx.lower_enum(&mut defs, res.def_id, e))
            .collect::<Vec<_>>();

        match &mut defs.get_def_mut(res.def_id) {
            Def::Class(def) => {
                def.extends = extends;
                def.implements = implements;
                def.within = within;
                def.enums = enums.into_boxed_slice();
            }
            Def::Interface(def) => {
                def.extends = extends;
            }
            _ => unreachable!(),
        }
    });

    println!("{:?}", &defs);

    defs
}

// Todo: This needs to take into account the environment when looking at delegates
fn ast_ty_to_ty(ctx: &DefHierarchy, ty: uc_ast::Ty) -> Ty {
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
