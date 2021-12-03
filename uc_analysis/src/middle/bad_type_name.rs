use std::str::FromStr;

use uc_ast_lowering::resolver::ResolverContext;
use uc_files::{ErrorReport, Fragment, Sources};
use uc_middle::Defs;
use uc_parser::{parser, Keyword};

pub fn run(defs: &Defs, resolver: &ResolverContext, _: &Sources) -> Vec<ErrorReport> {
    let mut errs = vec![];
    // Report type name ambiguities
    for (name, ids) in &resolver.global_ty_defs {
        if ids.len() > 1 {
            let fragments = ids
                .iter()
                .map(|&d| defs.try_get_span(d))
                .flatten()
                .map(|s| Fragment {
                    full_text: s,
                    inlay_messages: vec![("defined here".to_owned(), s)],
                })
                .collect::<Vec<_>>();
            let num = if fragments.len() == 1 {
                ("this type", "conflicts")
            } else {
                ("these types", "conflict")
            };
            let msg = if fragments.len() != ids.len() {
                format!("{} {} with a builtin type", num.0, num.1)
            } else {
                format!(
                    "{} have the same name and can't always be disambiguated",
                    num.0
                )
            };
            errs.push(ErrorReport {
                code: "type-name-conflict",
                msg,
                fragments,
            })
        }

        for &def_id in ids {
            if let Ok(kw) = Keyword::from_str(name.as_ref()) {
                if let Some(span) = defs.try_get_span(def_id) {
                    if parser::modifiers::VAR_MODIFIERS.contains(kw) {
                        errs.push(ErrorReport {
                            code: "unnameable-type",
                            msg: "type name is var modifier, so it can't ever be stored in a var"
                                .to_owned(),
                            fragments: vec![Fragment {
                                full_text: span,
                                inlay_messages: vec![("type declared here".to_owned(), span)],
                            }],
                        })
                    }

                    if parser::modifiers::FUNC_MODIFIERS.contains(kw) {
                        errs.push(ErrorReport {
                            code: "unnameable-type",
                            msg: "type name is func modifier, so it can't ever be returned from a func".to_owned(),
                            fragments: vec![Fragment { full_text: span, inlay_messages: vec![("type declared here".to_owned(), span)] }],
                        })
                    }
                }
            }
        }
    }
    errs
}
