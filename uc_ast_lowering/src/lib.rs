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
use uc_middle::{ty::Ty, Class, Def, DefHierarchy, DefId, Interface, Package};
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
    classes_interfaces: HashMap<Identifier, (DefId, ClassResidual)>,
}

/// For example, these class modifiers refer to other classes we might now have
/// visited yet, so we consume the header, create the class/interface defs with
/// name, ty, flags, and stash these modifiers as identifiers in a ClassResidual.
/// The next lowering step simply looks at these residuals and resolves them.
struct ClassResidual {
    extends: Option<Identifier>,
    implements: Vec<Identifier>,
    within: Option<Identifier>,
}

impl LoweringContext {
    fn lower_class(
        &mut self,
        defs: &mut DefHierarchy,
        file_name: &Identifier,
        hir: &Hir,
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
                        extends: extends.clone(),
                        implements: implements.clone(),
                        within: within.clone(),
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
                        extends: extends.clone(),
                        implements: vec![],
                        within: None,
                    },
                ),
            };
            l_ctx
                .classes_interfaces
                .insert(file_name.clone(), (file_id, residual));
            ret
        })
    }
}

pub fn lower(input: LoweringInput) -> DefHierarchy {
    let mut defs = DefHierarchy::new();
    let mut l_ctx = LoweringContext {
        packages: HashMap::new(),
        classes_interfaces: HashMap::new(),
    };

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
                        .map(|(file_name, hir)| l_ctx.lower_class(defs, file_name, hir, pack_id))
                        .collect(),
                }))
            })
        })
        .collect::<Vec<_>>();
    defs.packages = package_defs.into_boxed_slice();

    // Fix up superclasses and interface implementations
    // FIXME: Lots of repetition here
    l_ctx
        .classes_interfaces
        .values()
        .for_each(|(d, residual)| match &mut defs.get_def_mut(*d) {
            Def::Class(def) => {
                def.extends = residual
                    .extends
                    .as_ref()
                    .map(|e| l_ctx.classes_interfaces.get(e).unwrap().0);
                def.implements = residual
                    .implements
                    .iter()
                    .map(|e| l_ctx.classes_interfaces.get(e).unwrap().0)
                    .collect();
                def.within = residual
                    .within
                    .as_ref()
                    .map(|e| l_ctx.classes_interfaces.get(e).unwrap().0);
            }
            Def::Interface(def) => {
                def.extends = residual
                    .extends
                    .as_ref()
                    .map(|e| l_ctx.classes_interfaces.get(e).unwrap().0);
            }
            _ => unreachable!(),
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
