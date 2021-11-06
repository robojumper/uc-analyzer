//! The UnrealScript parser, for now reading expanded files from
//! `make -intermediate`.

pub mod lexer;
pub mod parser;

//pub mod preprocessor;
//mod arc_owned_chars;

use uc_files::Files;

#[derive(Clone, Copy, Debug)]
pub struct Span<F: Files> {
    pub file_id: F::FileId,
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Copy, Debug)]
pub enum NumberLiteral {
    Int(i32),
    Float(f32),
}

impl NumberLiteral {
    pub fn expect_int(&self) -> Result<i32, String> {
        match self {
            NumberLiteral::Int(i) => Ok(*i),
            NumberLiteral::Float(_) => Err("not an integer".to_owned()),
        }
    }
}
