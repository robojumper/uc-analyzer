//! The UnrealScript parser, for now reading expanded files from
//! `make -intermediate`.

pub mod lexer;
//pub mod preprocessor;
//mod arc_owned_chars;

use std::fmt::Display;

use uc_files::Files;

#[derive(Clone, Copy, Debug)]
pub struct Span<F: Files> {
    pub file_id: F::FileId,
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Identifier(unicase::Ascii<String>);

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
