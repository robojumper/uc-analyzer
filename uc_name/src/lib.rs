//! Case-insensitive ASCII strings.
//! The Unreal Engine 3 has the notion of "Names", which are case-insensitive,
//! ascii-only, and interned. The engine uses them for large parts of its
//! virtual machine + execution environment and compiler. This crate implements
//! such a type and explores strategies to optimize performance and memory
//! footprint.

use std::{fmt::Display, str::FromStr};

#[derive(Clone, Copy, Debug)]
pub struct NotAscii;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(unicase::Ascii<Box<str>>);

impl FromStr for Identifier {
    type Err = NotAscii;
    fn from_str(t: &str) -> Result<Self, Self::Err> {
        if !t.is_ascii() {
            Err(NotAscii)
        } else {
            Ok(Self(unicase::Ascii::new(t.to_owned().into_boxed_str())))
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.0.as_ref(), f)
    }
}

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.0.as_ref(), f)
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
