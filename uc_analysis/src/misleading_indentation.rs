use uc_ast::{
    visit::{self, Visitor},
    Block, Hir, Statement, StatementKind,
};
use uc_files::{Sources, Span};

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

pub fn visit_hir(hir: &'_ Hir, sources: &'_ Sources) -> Vec<MisleadingIndent> {
    let mut visitor = MisleadingIndentVisitor {
        errs: vec![],
        sources,
    };
    for func in &hir.funcs {
        if let Some(body) = &func.body {
            visitor.visit_statements(&body.statements);
        }
    }
    for state in &hir.states {
        visitor.visit_statements(&state.statements);
    }

    visitor.errs
}

fn indent_count(mut line: &[u8]) -> u32 {
    let mut count = 0;
    loop {
        match line.first() {
            Some(b'\t') => count += 4,
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
