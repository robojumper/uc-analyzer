//! Case-insensitive ASCII strings.
//! The Unreal Engine 3 has the notion of "Names", which are case-insensitive,
//! ascii-only, and interned. The engine uses them for large parts of its
//! virtual machine + execution environment and compiler. This crate implements
//! such a type and explores strategies to optimize performance and memory
//! footprint.

use std::{fmt::Display, str::FromStr};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(unicase::Ascii<String>);

impl FromStr for Identifier {
    type Err = <unicase::Ascii<String> as FromStr>::Err;
    fn from_str(t: &str) -> Result<Self, Self::Err> {
        Ok(Self(unicase::Ascii::from_str(t)?))
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.0.as_str(), f)
    }
}

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.0.as_str(), f)
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
