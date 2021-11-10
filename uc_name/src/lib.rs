//! Case-insensitive ASCII strings.
//! The Unreal Engine 3 has the notion of "Names", which are case-insensitive,
//! ascii-only, and interned. The engine uses them for large parts of its
//! virtual machine + execution environment and compiler. This crate implements
//! such a type and explores strategies to optimize performance and memory
//! footprint.

use std::{cmp::Ordering, fmt::Display, hash::Hasher, str::FromStr};

#[derive(Clone, Copy, Debug)]
pub struct Ascii<S: AsRef<str>>(S);

impl<S: AsRef<str>> Ascii<S> {
    #[inline]
    pub fn new(value: S) -> Result<Self, NotAscii> {
        if !value.as_ref().is_ascii() {
            Err(NotAscii)
        } else {
            Ok(Self(value))
        }
    }
}

impl<S: AsRef<str>> AsRef<str> for Ascii<S> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<S: AsRef<str>> PartialEq for Ascii<S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq_ignore_ascii_case(other.as_ref())
    }
}

impl<S: AsRef<str>> Eq for Ascii<S> {}

impl<T: AsRef<str>> PartialOrd for Ascii<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: AsRef<str>> Ord for Ascii<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        let self_chars = self.as_ref().chars().map(|c| c.to_ascii_lowercase());
        let other_chars = other.as_ref().chars().map(|c| c.to_ascii_lowercase());
        self_chars.cmp(other_chars)
    }
}

impl<S: AsRef<str>> std::hash::Hash for Ascii<S> {
    #[inline]
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        for byte in self.as_ref().bytes().map(|b| b.to_ascii_lowercase()) {
            hasher.write_u8(byte);
        }
        hasher.write_u8(0xFF);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NotAscii;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(Ascii<Box<str>>);

impl FromStr for Identifier {
    type Err = NotAscii;

    #[inline]
    fn from_str(t: &str) -> Result<Self, Self::Err> {
        Ok(Self(Ascii::new(t.to_owned().into_boxed_str())?))
    }
}

impl Display for Identifier {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.0.as_ref(), f)
    }
}

impl std::fmt::Debug for Identifier {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.0.as_ref(), f)
    }
}

impl AsRef<str> for Identifier {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}
