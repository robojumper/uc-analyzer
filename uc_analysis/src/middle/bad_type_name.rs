use uc_ast_lowering::resolver::ResolverContext;
use uc_files::{ErrorReport, Fragment, Sources};
use uc_middle::Defs;

pub fn run(defs: &Defs, resolver: &ResolverContext, sources: &Sources) -> Vec<ErrorReport> {
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
    }
    errs
}
