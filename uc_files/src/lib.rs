use std::{borrow::Cow, cmp::Ordering, collections::HashMap};

use uc_name::Identifier;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FileId(u32);

#[derive(Default, Debug)]
pub struct Sources {
    data: Vec<u8>,
    metadata: Vec<SourceFileMetadata>,
    files_names: HashMap<Identifier, FileId>,
}

#[derive(Debug)]
struct SourceFileMetadata {
    name: Identifier,
    span: Span,
}

#[derive(Copy, Clone, Debug)]
pub enum LookupError {
    InvalidBytePos,
    NonUtf8,
}

#[derive(Debug, Clone, Copy)]
pub enum InputError {
    Utf16BomBadLength,
    Utf16UnpairedSurrogate,
}

impl Sources {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(&mut self, name: Identifier, data: &[u8]) -> Result<FileId, InputError> {
        let data = fix_defects(name.as_ref(), data)?;

        if self.metadata.len() == u32::MAX as usize {
            panic!("too many files")
        }
        if (self.data.len() + 1).saturating_add(data.len()) > u32::MAX as usize {
            panic!("too much data")
        }

        let start = self.data.len() as u32;
        self.data.extend_from_slice(&*data);
        let end = self.data.len() as u32;
        self.data.push(0u8);

        let span = Span { start, end };

        let file_id = FileId(self.metadata.len() as u32);
        self.metadata.push(SourceFileMetadata {
            name: name.clone(),
            span,
        });

        self.files_names.insert(name, file_id);
        Ok(file_id)
    }

    #[inline]
    pub fn file_span(&self, f_id: FileId) -> Span {
        self.metadata[f_id.0 as usize].span
    }

    pub fn lookup_file(&self, byte_pos: u32) -> Result<FileId, LookupError> {
        if byte_pos > self.data.len() as u32 {
            return Err(LookupError::InvalidBytePos);
        }
        Ok(FileId(
            self.metadata
                .binary_search_by(|data| {
                    if data.span.start > byte_pos {
                        Ordering::Greater
                    } else if data.span.end < byte_pos {
                        Ordering::Less
                    } else {
                        assert!(byte_pos >= data.span.start && byte_pos <= data.span.end);
                        Ordering::Equal
                    }
                })
                .unwrap() as u32,
        ))
    }

    #[inline]
    pub fn lookup_bytes(&self, span: Span) -> &[u8] {
        &self.data[span.start as usize..span.end as usize]
    }

    #[inline]
    pub fn file_name(&self, f_id: FileId) -> &Identifier {
        &self.metadata[f_id.0 as usize].name
    }

    pub fn lookup_str(&self, span: Span) -> Result<&str, LookupError> {
        std::str::from_utf8(self.lookup_bytes(span)).map_err(|_| LookupError::NonUtf8)
    }
}

#[derive(Debug, Clone, Copy)]
enum ByteOrder {
    LE,
    BE,
}

pub fn fix_defects<'a>(file_name: &str, input: &'a [u8]) -> Result<Cow<'a, [u8]>, InputError> {
    if file_name.eq_ignore_ascii_case("UISimpleCommodityScreen") {
        let bad_end = b"}	ConfirmButtonY = 0";
        assert!(input.ends_with(bad_end));
        Ok(Cow::Borrowed(&input[0..input.len() - bad_end.len() + 1]))
    } else if file_name.eq_ignore_ascii_case("UIUFOAttack") {
        let bad_end = b"}}";
        assert!(input.ends_with(bad_end));
        Ok(Cow::Borrowed(&input[0..input.len() - bad_end.len() + 1]))
    } else if input.starts_with(&[0xFF, 0xFE]) {
        Ok(Cow::Owned(
            decode_utf16_from_u8s(&input[2..], ByteOrder::LE)?.into_bytes(),
        ))
    } else if input.starts_with(&[0xFE, 0xFF]) {
        Ok(Cow::Owned(
            decode_utf16_from_u8s(&input[2..], ByteOrder::BE)?.into_bytes(),
        ))
    } else {
        Ok(Cow::Borrowed(input))
    }
}

fn decode_utf16_from_u8s(input: &[u8], order: ByteOrder) -> Result<String, InputError> {
    if input.len() % 2 != 0 {
        return Err(InputError::Utf16BomBadLength);
    }
    let len = input.len() / 2;
    let iter = (0..len).map(|i| {
        u16::from_le_bytes(match order {
            ByteOrder::LE => [input[2 * i], input[2 * i + 1]],
            ByteOrder::BE => [input[2 * i + 1], input[2 * i]],
        })
    });

    char::decode_utf16(iter)
        .collect::<Result<String, _>>()
        .map_err(|_| InputError::Utf16UnpairedSurrogate)
}
