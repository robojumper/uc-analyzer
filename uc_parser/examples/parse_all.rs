use core::panic;
use std::fs;
use std::str::FromStr;

use uc_analysis::{
    ambiguous_new_template, dangling_else, misleading_indentation, missing_break, uneffectful_stmt,
};
use uc_files::Sources;
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
    let preprocessed = if dir.contains("PreProcessedFiles") || dir.contains("TestFiles") {
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

        let mut errs = vec![];
        errs.extend(ambiguous_new_template::run(&hir, &sources));
        errs.extend(dangling_else::run(&hir, &sources));
        errs.extend(misleading_indentation::run(&hir, &sources));
        errs.extend(missing_break::run(&hir, &sources));
        errs.extend(uneffectful_stmt::run(&hir, &sources));

        errs.iter().for_each(|e| sources.emit_err(e));

        hirs.push(hir);
    }
    let mut buffer = String::new();
    let stdin = std::io::stdin(); // We get `Stdin` here.
    stdin.read_line(&mut buffer).unwrap();
}
