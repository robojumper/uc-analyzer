use uc_ast::{
    visit::{self, Visitor},
    Block, Hir, Statement, StatementKind,
};
use uc_files::{ErrorReport, Sources, Span};

struct MisleadingIndentVisitor<'a> {
    errs: Vec<MisleadingIndent>,
    sources: &'a Sources,
}

pub struct MisleadingIndent {
    /// The misleadingly indented statement
    pub affected_statement: Span,
    /// The if, else, for, while, foreach guard's kind and span
    pub guard: (&'static str, Span),
}

pub fn run(hir: &Hir, sources: &Sources) -> Vec<ErrorReport> {
    let mut visitor = MisleadingIndentVisitor {
        errs: vec![],
        sources,
    };
    visitor.visit_hir(hir);
    visitor
        .errs
        .iter()
        .map(|err| {
            let first_msg = (
                format!("this {} statement (+ guarded statement)...", err.guard.0),
                err.guard.1,
            );
            let second_msg = (
                "...looks like it guards this statement".to_owned(),
                err.affected_statement,
            );

            ErrorReport {
                code: "misleading-indent",
                full_text: Span {
                    start: err.guard.1.start,
                    end: err.affected_statement.end,
                },
                msg: "misleading indentation".to_owned(),
                inlay_messages: vec![first_msg, second_msg],
            }
        })
        .collect()
}

fn indent_count(mut line: &[u8]) -> u32 {
    let mut count = 0;
    loop {
        match line.first() {
            Some(b'\t') => count = (count & !0b11) + 4,
            Some(b' ') => count += 1,
            Some(_) | None => break count,
        }
        line = &line[1..];
    }
}

impl Visitor for MisleadingIndentVisitor<'_> {
    const VISIT_EXPRS: bool = false;

    fn visit_statements(&mut self, stmts: &[Statement]) {
        visit::walk_statements(self, stmts);

        for ss in stmts.windows(2) {
            let s1 = &ss[0];
            let s2 = &ss[1];

            let (kind, then) = match &s1.kind {
                StatementKind::IfStatement { then, .. } => ("if", then),
                StatementKind::ForStatement { run, .. } => ("for", run),
                StatementKind::ForeachStatement { run, .. } => ("foreach", run),
                StatementKind::WhileStatement { run, .. } => ("while", run),
                _ => continue,
            };

            self.perform_check(kind, then, s1.span, s2);
        }
    }
}

impl MisleadingIndentVisitor<'_> {
    fn perform_check(
        &mut self,
        kind: &'static str,
        then: &Block,
        guard_span: Span,
        next_stmt: &Statement,
    ) {
        if !then.from_single_stmt {
            // has braces
            return;
        }

        let l1 = self.sources.lookup_line(guard_span.start).unwrap();
        let l2 = self.sources.lookup_line(next_stmt.span.start).unwrap();

        if indent_count(l2.as_bytes()) > indent_count(l1.as_bytes()) {
            self.errs.push(MisleadingIndent {
                affected_statement: next_stmt.span,
                guard: (kind, guard_span),
            });
        }
    }
}
