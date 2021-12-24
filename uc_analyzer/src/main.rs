use std::io;
use std::str::FromStr;
use std::{fs, path::PathBuf};

use uc_analysis::ast::{
    ambiguous_new_template, ambiguous_ternary_op, dangling_else, misleading_indentation,
    missing_break, never_loop, uneffectful_stmt,
};
use uc_analysis::middle::{bad_enum_values, bad_type_name, unreachable_code};
use uc_ast::Hir;
use uc_ast_lowering::{BodyErrorKind, IntEnumCorrespondence, LoweringInput, LoweringInputPackage};
use uc_files::{ErrorCode, ErrorReport, FileId, Fragment, Level, Sources};
use uc_name::Identifier;
use uc_parser::{lexer, parser};

use structopt::StructOpt;

struct Package {
    name: Identifier,
    files: Vec<FileId>,
    hirs: Vec<Hir>,
}

#[derive(StructOpt, Debug)]
#[structopt(name = "uc_analyzer")]
struct Opt {
    /// The Src folder
    #[structopt(long)]
    path: PathBuf,

    /// The PreprocessedFiles folder containing
    /// macro expansion results. Defaults to
    /// <path>/../../XComGame/PreProcessedFiles
    #[structopt(long)]
    expanded: Option<PathBuf>,

    /// Whether to emit lints for base game files.
    #[structopt(long)]
    ignore_base: bool,
}

const BASE_GAME_PACKAGES: &[&str] = &[
    "AkAudio",
    "Core",
    "DLC_1",
    "DLC_2",
    "DLC_3",
    "Engine",
    "GameFramework",
    "GfxUI",
    "GfxUIEditor",
    "IpDrv",
    "OnlineSubsystemSteamworks",
    "TLE",
    "UnrealEd",
    "XComEditor",
    "XComGame",
];

fn main() {
    let args = Opt::from_args();

    let dir = args.path;
    let expanded_dir = args.expanded.unwrap_or_else(|| {
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

        let package_name = package_dir
            .file_name()
            .to_str()
            .and_then(|n| Identifier::from_str(n).ok())
            .expect("non-ascii package name");

        let mut package = Package { name: package_name.clone(), files: vec![], hirs: vec![] };

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
                    package_name.as_ref().to_owned(),
                    &contents,
                    path,
                )
                .expect("invalid encoding");
            package.files.push(id);
        }

        packages.push(package);
    }

    let mut errs = vec![];

    for p in &mut packages {
        for &f in &p.files {
            let lexer = lexer::Lexer::new(&sources, f);
            let (hir, p_errs) = parser::parse(lexer);
            if !p_errs.is_empty() {
                for err in &p_errs {
                    let highlight = match err.err.expected_token {
                        Some(e) => format!("expected {:?}", e),
                        None => "this token".to_owned(),
                    };
                    let rep = ErrorReport {
                        code: ErrorCode { msg: "parser-error", level: Level::Error, priority: 1 },
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
            }

            errs.extend(ambiguous_new_template::run(&hir, &sources));
            errs.extend(ambiguous_ternary_op::run(&hir, &sources));
            errs.extend(dangling_else::run(&hir, &sources));
            errs.extend(misleading_indentation::run(&hir, &sources));
            errs.extend(missing_break::run(&hir, &sources));
            errs.extend(never_loop::run(&hir, &sources));
            errs.extend(uneffectful_stmt::run(&hir, &sources));

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
        let (msg, hint) = match &b.kind {
            BodyErrorKind::EnumIntValue { expected, found, corresponding_value } => {
                let hint = match corresponding_value {
                    IntEnumCorrespondence::Variant(num, var) => {
                        let var = defs.get_variant(*var);
                        format!("{} corresponds to {}", num, var.name)
                    }
                    IntEnumCorrespondence::NoVariant(num) => {
                        format!("{} does not correspond to an enum variant", num)
                    }
                    IntEnumCorrespondence::ComplexExpression => {
                        "this expression may or may not correspond to an enum variant".to_owned()
                    }
                };
                (
                    format!(
                        "type mismatch: expected {}, found {}",
                        defs.format_ty(*expected),
                        defs.format_ty(*found)
                    ),
                    hint,
                )
            }
            BodyErrorKind::TyMismatch { expected, found } => (
                format!(
                    "type mismatch: expected {}, found {}",
                    defs.format_ty(*expected),
                    defs.format_ty(*found)
                ),
                "here".to_owned(),
            ),
            BodyErrorKind::MissingCast { to, from } => (
                format!("missing cast from {} to {}", defs.format_ty(*from), defs.format_ty(*to)),
                "here".to_owned(),
            ),
            BodyErrorKind::InvalidCast { to, from } => (
                format!("cannot cast from {} to {}", defs.format_ty(*from), defs.format_ty(*to)),
                "here".to_owned(),
            ),
            x => (format!("{:?}", x), "here".to_owned()),
        };
        let err = ErrorReport {
            code: ErrorCode { msg: "body-lowering-error", level: Level::Error, priority: 2 },
            msg,
            fragments: vec![Fragment { full_text: b.span, inlay_messages: vec![(hint, b.span)] }],
        };
        errs.push(err);
    });

    errs.extend(bad_type_name::run(&defs, &resolver, &sources));
    errs.extend(bad_enum_values::run(&defs, &resolver, &sources));

    errs.extend(unreachable_code::run(&defs, &sources));

    if args.ignore_base {
        errs.retain(|e| {
            e.fragments.iter().any(|fr| {
                let fid_a = sources.lookup_file(fr.full_text.start).expect("bad error message");
                let pack = sources.file_package(fid_a);
                !BASE_GAME_PACKAGES.iter().any(|&p| p.eq_ignore_ascii_case(pack))
            })
        });
    }

    errs.sort_by_cached_key(|e| {
        let fid_a = sources.lookup_file(e.fragments[0].full_text.start).expect("bad error message");
        sources.file_name(fid_a)
    });
    errs.sort_by_key(|e| e.code.priority);

    errs.iter().for_each(|e| sources.emit_err(e));
}
