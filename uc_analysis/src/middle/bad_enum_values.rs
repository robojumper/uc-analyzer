use uc_ast_lowering::resolver::ResolverContext;
use uc_files::{ErrorCode, ErrorReport, Fragment, Level, Sources};
use uc_middle::Defs;

pub fn run(defs: &Defs, resolver: &ResolverContext, _: &Sources) -> Vec<ErrorReport> {
    let mut errs = vec![];
    // Report ambiguities in enum value names
    for ids in resolver.global_values.values() {
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

            let msg = "enum values have the same name".to_owned();
            errs.push(ErrorReport {
                code: ErrorCode { msg: "enum-value-conflict", level: Level::Warning, priority: 4 },
                msg,
                fragments,
            })
        }
    }
    errs
}
