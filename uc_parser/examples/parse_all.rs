use core::panic;
use std::fs;
use std::str::FromStr;

use uc_files::{ErrorReport, Sources, Span};
use uc_name::Identifier;
use uc_parser::{lexer, parser};
use walkdir::{DirEntry, WalkDir};

fn is_uc(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| s.ends_with(".uc") || s.ends_with(".UC"))
        .unwrap_or(false)
}

fn main() {
    let dir = std::env::args().nth(1).expect("missing directory");
    let preprocessed = if dir.contains("PreProcessedFiles") {
        true
    } else if dir.contains("Development") {
        false
    } else {
        panic!("Don't know if preprocessed or not.");
    };

    let walker = WalkDir::new(dir).into_iter();
    let mut hirs = vec![];
    let mut sources = Sources::new();

    for entry in walker {
        let entry = match entry {
            Ok(d) => d,
            Err(e) => {
                eprintln!("{:?}", e);
                continue;
            }
        };

        if !is_uc(&entry) {
            continue;
        }

        let name = entry.file_name().to_str().unwrap();

        let contents = match fs::read(entry.path()) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("{:?}: I/O Error {:?}", entry.path(), e);
                continue;
            }
        };

        if !preprocessed && contents.contains(&b'`') {
            continue; // will be expanded
        }

        let id = sources
            .add_file(
                Identifier::from_str(&name[0..name.find('.').unwrap()]).unwrap(),
                &contents,
            )
            .expect("invalid encoding");

        //eprintln!("{}", name);

        let lexer = lexer::Lexer::new(&sources, id);
        let (hir, errs) = parser::parse(lexer);
        if !errs.is_empty() {
            dbg!(&name);
            dbg!(&errs);
            panic!();
        }
        /*
        let out = std::io::stdout();
        let mut out = out.lock();
        uc_ast::pretty::format_hir(&hir, &mut out, uc_ast::pretty::IdentifierFormat).unwrap();
        */

        let ambiguous_new = uc_analysis::ambiguous_new_template::visit_hir(&hir);
        for err in ambiguous_new {
            let err = ErrorReport {
                full_text: err.new_expr,
                msg: "new with function call is ambiguous".to_owned(),
                inlay_messages: vec![(
                    "this could be a function call or a field reference with template arguments"
                        .to_owned(),
                    err.cls_expr,
                )],
            };
            sources.emit_err(&err);
        }

        let uneffectful_errs = uc_analysis::uneffectful_stmt::visit_hir(&hir);
        for err in uneffectful_errs {
            let err = ErrorReport {
                full_text: err.1,
                msg: "expression statement has no effect".to_owned(),
                inlay_messages: vec![(err.0.to_owned(), err.1)],
            };
            sources.emit_err(&err);
        }

        let missing_breaks = uc_analysis::missing_break::visit_hir(&hir, &sources);
        for err in missing_breaks {
            let first_msg = ("control flow from this label...".to_owned(), err.from_label);
            let mut second_msg = (
                "...implicitly falls through to this label".to_owned(),
                err.to_label,
            );
            if err.and_then_executes.is_none() {
                second_msg.0 += " (which has no statements and may fall further)";
            }
            let mut inlay_messages = vec![first_msg, second_msg];
            let mut full_text = Span {
                start: err.from_label.start,
                end: err.to_label.end,
            };

            if let Some(e) = err.and_then_executes {
                full_text.end = e.end;
                inlay_messages.push(("and executes these statements".to_owned(), e))
            }

            let err = ErrorReport {
                full_text,
                msg: "implicit fallthrough".to_owned(),
                inlay_messages,
            };
            sources.emit_err(&err);
        }

        let dangling_elses = uc_analysis::dangling_else::visit_hir(&hir);
        for err in dangling_elses {
            let err = ErrorReport {
                full_text: err.whole_thing,
                msg: "if if else is ambiguous".to_owned(),
                inlay_messages: vec![("this one".to_owned(), err.whole_thing)],
            };
            sources.emit_err(&err);
        }

        hirs.push(hir);
    }
    let mut buffer = String::new();
    let stdin = std::io::stdin(); // We get `Stdin` here.
    stdin.read_line(&mut buffer).unwrap();
}
