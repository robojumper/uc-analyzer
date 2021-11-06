use core::panic;
use std::fs::read_to_string;
use std::io;

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
    let walker = WalkDir::new(dir).into_iter();
    for entry in walker {
        let entry = match entry {
            Ok(d) => d,
            Err(e) => {
                println!("{:?}", e);
                continue;
            }
        };

        eprintln!("{}", entry.path().display());

        if !is_uc(&entry) {
            continue;
        }

        let contents = match read_to_string(entry.path()) {
            Ok(c) => c,
            Err(e) if e.kind() == io::ErrorKind::InvalidData => {
                eprintln!("{:?}: Invalid UTF-8 - {:?}", entry.path(), e);
                continue;
            }
            Err(e) => {
                eprintln!("{:?}: I/O Error {:?}", entry.path(), e);
                continue;
            }
        };

        let lexer = lexer::Lexer::new(&contents);
        let (hir, errs) = parser::parse(lexer);
        dbg!(&errs);
        if !errs.is_empty() {
            panic!();
        }
        let out = std::io::stdout();
        let mut out = out.lock();
        uc_def::pretty::format_hir(&hir, &mut out, uc_def::pretty::IdentifierFormat).unwrap();
    }
}
