use std::{iter::FusedIterator, mem, str::Chars, sync::Arc};

#[derive(Debug)]
pub struct ArcOwnedChars {
    chars: Chars<'static>,
    text: Arc<str>,
}

impl ArcOwnedChars {
    pub fn from_arc_str(text: Arc<str>) -> Self {
        let chars = text.chars();

        // SAFETY: Outside of this module, `text` can only be dropped by
        // dropping the `ArcOwnedChars`, which means there's no way to access
        // the laundered Chars<'static> after dropping the backing string.
        let laundered_chars = unsafe { mem::transmute::<Chars<'_>, Chars<'static>>(chars) };

        Self {
            text,
            chars: laundered_chars,
        }
    }

    pub fn as_str(&self) -> &str {
        self.chars.as_str()
    }
}

impl Iterator for ArcOwnedChars {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.chars.size_hint()
    }
}

impl FusedIterator for ArcOwnedChars {}
