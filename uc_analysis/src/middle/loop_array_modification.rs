use uc_files::{ErrorReport, Fragment, Sources, Span};
use uc_middle::{
    body::{BlockId, Body, ExprId, StatementKind, StmtId},
    DefKind, Defs,
};

struct Modification {
    lop: StmtId,
    modf: ExprId,
}

struct LoopArrayModificationFinder<'a> {
    body: &'a Body,
    errs: Vec<Modification>,
}

struct LoopFinder<'a> {
    body: &'a Body,
}

pub fn run(defs: &Defs, _: &Sources) -> Vec<ErrorReport> {
    let mut errs = vec![];

    // Finds loops that run over arrays and modify the array in their body

    for def in defs.iter() {
        let (span, stmts, must_return) = match &def.kind {
            DefKind::State(s) => (def.span, &s.contents, false),
            DefKind::Operator(o) => match &o.contents {
                Some(c) => (def.span, &c.statements, o.sig.ret_ty.is_some()),
                None => continue,
            },
            DefKind::Function(f) => match &f.contents {
                Some(c) => (def.span, &c.statements, f.sig.ret_ty.is_some()),
                None => continue,
            },
            _ => continue,
        };

        let stmts = match stmts {
            Some(stmts) => stmts,
            _ => continue,
        };
    }

    errs
}
