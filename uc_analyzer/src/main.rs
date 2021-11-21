use std::io;
use std::str::FromStr;
use std::{fs, path::PathBuf};

use uc_analysis::{
    ambiguous_new_template, dangling_else, misleading_indentation, missing_break, uneffectful_stmt,
};
use uc_files::{FileId, Sources};
use uc_name::Identifier;
use uc_parser::{lexer, parser};

struct Package {
    name: Identifier,
    files: Vec<FileId>,
}

fn main() {
    let dir = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .expect("missing directory");
    let expanded_dir = std::env::args()
        .nth(2)
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            let mut exp = dir.clone(); // Src
            exp.pop(); // Development
            exp.pop(); // SDK
            exp.push("XComGame");
            exp.push("PreprocessedFiles");
            exp
        });

    let mut hirs = vec![];
    let mut sources = Sources::new();
    let mut packages = vec![];

    let packages_dir = fs::read_dir(dir).expect("failed to open sources folder");

    for package_dir in packages_dir {
        let package_dir = package_dir.expect("failed to open entry");
        if !package_dir
            .file_type()
            .expect("failed to get file type")
            .is_dir()
        {
            continue;
        }

        let mut package = Package {
            name: package_dir
                .file_name()
                .to_str()
                .and_then(|n| Identifier::from_str(n).ok())
                .expect("non-ascii package name"),
            files: vec![],
        };

        let classes = {
            let mut path = package_dir.path();
            path.push("Classes");
            match fs::read_dir(path) {
                Ok(p) => p,
                Err(e) if e.kind() == io::ErrorKind::NotFound => continue,
                x @ Err(_) => x.expect("failed to open Classes folder"),
            }
        };

        for class in classes {
            let class = class.expect("failed to open file");
            if !class
                .file_type()
                .expect("failed to get file type")
                .is_file()
            {
                continue;
            }
            if !matches!(
                class.path().extension().and_then(|s| s.to_str()),
                Some("uc" | "UC")
            ) {
                continue;
            }

            let mut path = class.path();
            let mut contents = fs::read(&path).expect("failed to load file");
            if contents.contains(&b'`') {
                // The file contains the macro replacement character, so we expect an expanded version
                // in <expanded_dir>/<Package>/<Class>.uc -- There's no Classes folder!
                path = expanded_dir.clone();
                path.push(package_dir.file_name());
                path.push(class.file_name());
                contents = fs::read(&path).unwrap_or_else(|e| panic!("failed to read expanded file {:?}: {:?}", path, e));
            }

            let id = sources
                .add_file(
                    path.file_stem()
                        .and_then(|n| n.to_str())
                        .and_then(|n| Identifier::from_str(n).ok())
                        .unwrap(),
                    &contents,
                    path,
                )
                .expect("invalid encoding");
            package.files.push(id);
        }

        packages.push(package);
    }

    for p in &packages {
        for &f in &p.files {
            let lexer = lexer::Lexer::new(&sources, f);
            let (hir, errs) = parser::parse(lexer);
            if !errs.is_empty() {
                dbg!(sources.file_name(f));
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
    }
}
