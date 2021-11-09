//! The UnrealScript parser, for now reading expanded files from
//! `make -intermediate`.

use std::{borrow::Cow, str};

pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, Copy)]
pub enum InputError {
    Utf16BomBadLength,
    Utf16UnpairedSurrogate,
}

#[derive(Debug, Clone, Copy)]
enum ByteOrder {
    LE,
    BE,
}

pub fn fix_defects<'a>(file_name: &str, input: &'a [u8]) -> Result<Cow<'a, [u8]>, InputError> {
    if file_name.eq_ignore_ascii_case("UISimpleCommodityScreen.uc") {
        let bad_end = b"}	ConfirmButtonY = 0";
        assert!(input.ends_with(bad_end));
        Ok(Cow::Borrowed(&input[0..input.len() - bad_end.len() + 1]))
    } else if file_name.eq_ignore_ascii_case("UIUFOAttack.uc") {
        let bad_end = b"}}";
        assert!(input.ends_with(bad_end));
        Ok(Cow::Borrowed(&input[0..input.len() - bad_end.len() + 1]))
    } else if input.starts_with(&[0xFF, 0xFE]) {
        Ok(Cow::Owned(decode_utf16_from_u8s(&input[2..], ByteOrder::LE)?.into_bytes()))
    } else if input.starts_with(&[0xFE, 0xFF]) {
        Ok(Cow::Owned(decode_utf16_from_u8s(&input[2..], ByteOrder::BE)?.into_bytes()))
    } else {
        Ok(Cow::Borrowed(input))
    }
}

fn decode_utf16_from_u8s(input: &[u8], order: ByteOrder) -> Result<String, InputError> {
    if input.len() % 2 != 0 {
        return Err(InputError::Utf16BomBadLength);
    }
    let len = input.len() / 2;
    let iter = (0..len).map(|i| u16::from_le_bytes(
        match order {
            ByteOrder::LE => [input[2 * i], input[2 * i + 1]],
            ByteOrder::BE => [input[2 * i + 1], input[2 * i]],
        }
    ));

    char::decode_utf16(iter)
        .collect::<Result<String, _>>()
        .map_err(|_| InputError::Utf16UnpairedSurrogate)
}
