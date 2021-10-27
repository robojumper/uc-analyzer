use std::fs::read_to_string;
use std::io;

use uc_parser::lexer;
use walkdir::{DirEntry, WalkDir};

fn is_uc(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| /*s.ends_with(".uc") ||*/ s.ends_with(".uci"))
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

        let mut lexer = lexer::Lexer::new(&contents);
        //println!("{:?}", entry.path());
        while let Some(token) = lexer.next() {
            match token.kind {
                lexer::TokenKind::Error(_) => eprintln!(
                    "{:?} - {:?} (...{:?}...)",
                    entry.file_name(),
                    token,
                    &contents[token.span.start - 3..token.span.end + 3]
                ),
                lexer::TokenKind::Incomplete(_, _) => eprintln!(
                    "{:?} - {:?} (...{:?}...)",
                    entry.file_name(),
                    token,
                    &contents[token.span.start - 3..token.span.end + 3]
                ),
                _ => {}
            }
            /*
            println!(
                "{:?} - {}",
                token,
                &contents[token.span.start..token.span.end]
            );
            */
        }
    }
}
