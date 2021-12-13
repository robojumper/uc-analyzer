use std::str::FromStr;

use uc_files::{ErrorReport, Fragment, Sources};
use uc_middle::Defs;

enum BranchGuarantee {
    Always,
    Maybe,
    Never,
}

struct BranchOptions {
    falls: BranchGuarantee,
    breaks: BranchGuarantee,
    continues: BranchGuarantee,
    returns: BranchGuarantee,
}

pub fn run(defs: &Defs, _: &Sources) -> Vec<ErrorReport> {
    let mut errs = vec![];

    // Finds statements that are referenced in a function body,
    // but never executed due to an return, break, or continue

    errs
}
