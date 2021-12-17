use std::io;
use std::str::FromStr;
use std::{fs, path::PathBuf};

use uc_analysis::ast::{
    ambiguous_new_template, ambiguous_ternary_op, dangling_else, misleading_indentation,
    missing_break, never_loop, uneffectful_stmt,
};
use uc_analysis::middle::{bad_enum_values, bad_type_name, unreachable_code};
use uc_ast::Hir;
use uc_ast_lowering::{BodyError, BodyErrorKind, LoweringInput, LoweringInputPackage};
use uc_files::{ErrorReport, FileId, Fragment, Sources};
use uc_middle::DefKind;
use uc_name::Identifier;
use uc_parser::{lexer, parser};

struct Package {
    name: Identifier,
    files: Vec<FileId>,
    hirs: Vec<Hir>,
}

fn main() {
    let dir = std::env::args().nth(1).map(PathBuf::from).expect("missing directory");
    let expanded_dir = std::env::args().nth(2).map(PathBuf::from).unwrap_or_else(|| {
        let mut exp = dir.clone(); // Src
        exp.pop(); // Development
        exp.pop(); // SDK
        exp.push("XComGame");
        exp.push("PreprocessedFiles");
        exp
    });

    let mut sources = Sources::new();
    let mut packages = vec![];

    let packages_dir = fs::read_dir(dir).expect("failed to open sources folder");

    for package_dir in packages_dir {
        let package_dir = package_dir.expect("failed to open entry");
        if !package_dir.file_type().expect("failed to get file type").is_dir() {
            continue;
        }

        let mut package = Package {
            name: package_dir
                .file_name()
                .to_str()
                .and_then(|n| Identifier::from_str(n).ok())
                .expect("non-ascii package name"),
            files: vec![],
            hirs: vec![],
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
            if !class.file_type().expect("failed to get file type").is_file() {
                continue;
            }
            if !matches!(class.path().extension().and_then(|s| s.to_str()), Some("uc" | "UC")) {
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
                contents = fs::read(&path)
                    .unwrap_or_else(|e| panic!("failed to read expanded file {:?}: {:?}", path, e));
            }

            let id = sources
                .add_file(
                    path.file_stem().and_then(|n| n.to_str()).map(|n| n.to_owned()).unwrap(),
                    &contents,
                    path,
                )
                .expect("invalid encoding");
            package.files.push(id);
        }

        packages.push(package);
    }

    for p in &mut packages {
        for &f in &p.files {
            let lexer = lexer::Lexer::new(&sources, f);
            let (hir, errs) = parser::parse(lexer);
            if !errs.is_empty() {
                for err in &errs {
                    let highlight = match err.err.expected_token {
                        Some(e) => format!("expected {:?}", e),
                        None => "this token".to_owned(),
                    };
                    let rep = ErrorReport {
                        code: "parser-error",
                        msg: err.err.error_message.to_owned(),
                        fragments: vec![Fragment {
                            full_text: err.err.bad_token.as_ref().unwrap().span,
                            inlay_messages: vec![(
                                highlight,
                                err.err.bad_token.as_ref().unwrap().span,
                            )],
                        }],
                    };
                    sources.emit_err(&rep);
                }
                panic!()
            }

            let mut errs = vec![];
            /*
            errs.extend(ambiguous_new_template::run(&hir, &sources));
            errs.extend(ambiguous_ternary_op::run(&hir, &sources));
            errs.extend(dangling_else::run(&hir, &sources));
            errs.extend(misleading_indentation::run(&hir, &sources));
            errs.extend(missing_break::run(&hir, &sources));
            errs.extend(never_loop::run(&hir, &sources));
            errs.extend(uneffectful_stmt::run(&hir, &sources));
            */
            errs.iter().for_each(|e| sources.emit_err(e));

            /*
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            uc_ast::pretty::format_hir(&hir, &mut handle).unwrap();
            */
            p.hirs.push(hir);
        }
    }

    let input = LoweringInput {
        packages: packages
            .into_iter()
            .map(|p| {
                let pack = LoweringInputPackage {
                    files: p.hirs.into_iter().map(|h| (h.header.name.clone(), h)).collect(),
                };
                (p.name, pack)
            })
            .collect(),
    };

    let (defs, resolver, body_errs) = uc_ast_lowering::lower(input);

    body_errs.iter().for_each(|b| {
        let msg = match &b.kind {
            BodyErrorKind::TyMismatch { expected, found } => format!(
                "type mismatch: expected {}, found {}",
                defs.format_ty(*expected),
                defs.format_ty(*found)
            ),
            BodyErrorKind::MissingCast { to, from } => {
                format!("missing cast from {} to {}", defs.format_ty(*from), defs.format_ty(*to))
            }
            BodyErrorKind::InvalidCast { to, from } => {
                format!("cannot cast from {} to {}", defs.format_ty(*from), defs.format_ty(*to))
            }
            x => format!("{:?}", x),
        };
        let err = ErrorReport {
            code: "body-lowering-error",
            msg,
            fragments: vec![Fragment {
                full_text: b.span,
                inlay_messages: vec![("here".to_owned(), b.span)],
            }],
        };
        sources.emit_err(&err);
    });

    let mut errs = vec![];
    /*
    errs.extend(bad_type_name::run(&defs, &resolver, &sources));
    errs.extend(bad_enum_values::run(&defs, &resolver, &sources));
    */
    errs.extend(unreachable_code::run(&defs, &sources));
    errs.iter().for_each(|e| sources.emit_err(e));

    /*
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    let mut class_defs = defs
        .iter()
        .filter_map(|d| match &d.kind {
            DefKind::Class(_) => Some(d.id),
            _ => None,
        })
        .collect::<Vec<_>>();
    class_defs.sort_unstable_by_key(|&d| &defs.get_class(d).name);
    for c in class_defs {
        uc_middle::pretty::format_file(&defs, c, &mut handle).unwrap();
    }
    */
}
