use std::{borrow::Cow, cmp::Ordering, collections::HashMap, path::PathBuf};

use annotate_snippets::{
    display_list::{self, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
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
    path: PathBuf,
    span: Span,
    line_heads: Vec<u32>,
}

#[derive(Copy, Clone, Debug)]
pub enum LookupError {
    CrossFileSpan,
    InvalidBytePos,
    NonUtf8,
}

#[derive(Debug, Clone, Copy)]
pub enum InputError {
    Utf16BomBadLength,
    Utf16UnpairedSurrogate,
}

pub struct ErrorReport {
    pub code: &'static str,
    pub msg: String,
    pub full_text: Span,
    pub inlay_messages: Vec<(String, Span)>,
}

impl Sources {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(
        &mut self,
        name: Identifier,
        data: &[u8],
        path: PathBuf,
    ) -> Result<FileId, InputError> {
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
        let line_heads = std::iter::once(start)
            .chain(data.iter().enumerate().filter_map(|(idx, &b)| {
                if b == b'\n' {
                    Some(idx as u32 + start + 1)
                } else {
                    None
                }
            }))
            .collect();
        self.metadata.push(SourceFileMetadata {
            name: name.clone(),
            path,
            span,
            line_heads,
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
                    if byte_pos < data.span.start {
                        Ordering::Greater
                    } else if data.span.end < byte_pos {
                        Ordering::Less
                    } else {
                        assert!(data.span.start <= byte_pos && byte_pos <= data.span.end);
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

    #[inline]
    pub fn lookup_str(&self, span: Span) -> Result<&str, LookupError> {
        std::str::from_utf8(self.lookup_bytes(span)).map_err(|_| LookupError::NonUtf8)
    }

    pub fn emit_err(&self, err: &ErrorReport) {
        let full_span = err.full_text;
        let (fid, source, line_start, remapped_span) = self.lookup_err_ctx(full_span).unwrap();

        // replace tabs with 4 spaces and adjust spans
        let tab_indices = source
            .bytes()
            .enumerate()
            .filter_map(|(idx, b)| if b == b'\t' { Some(idx) } else { None })
            .collect::<Vec<_>>();

        let new_pos = |b_pos: u32| {
            let num_prev_tabs = tab_indices
                .binary_search(&(b_pos as usize))
                .map_or_else(|x| x, |x| x);
            b_pos + (num_prev_tabs as u32 * 3)
        };

        let source = source.replace('\t', "    ");

        let annotations = err
            .inlay_messages
            .iter()
            .map(|(msg, span)| {
                assert!(span.start >= full_span.start && span.end <= full_span.end);
                let new_span = (
                    new_pos(span.start - full_span.start + remapped_span.start) as usize,
                    new_pos(span.end - full_span.start + remapped_span.start) as usize,
                );
                SourceAnnotation {
                    range: new_span,
                    label: msg,
                    annotation_type: AnnotationType::Warning,
                }
            })
            .collect();

        let slices = vec![Slice {
            source: &*source,
            line_start,
            origin: Some(self.metadata[fid.0 as usize].name.as_ref()),
            annotations,
            fold: false,
        }];
        let snippet = Snippet {
            title: Some(Annotation {
                annotation_type: AnnotationType::Warning,
                id: Some(err.code),
                label: Some(&err.msg),
            }),
            footer: vec![],
            slices,
            opt: FormatOptions::default(),
        };

        eprintln!("{}", display_list::DisplayList::from(snippet));
    }

    /// -> FileID, data, first line number, remapped span
    fn lookup_err_ctx(&self, span: Span) -> Result<(FileId, &str, usize, Span), LookupError> {
        let fid_a = self.lookup_file(span.start)?;
        let fid_b = self.lookup_file(span.end)?;
        if fid_a != fid_b {
            return Err(LookupError::CrossFileSpan);
        }

        let meta = &self.metadata[fid_a.0 as usize];

        let line_idx_a = meta
            .line_heads
            .binary_search(&span.start)
            .map_or_else(|x| x - 1, |x| x);
        let line_idx_b = meta
            .line_heads
            .binary_search(&span.end)
            .map_or_else(|x| x - 1, |x| x);

        let start_idx = meta.line_heads[line_idx_a];
        let end_idx = meta
            .line_heads
            .get(line_idx_b + 1)
            .copied()
            .unwrap_or(meta.span.end + 1)
            - 1;

        let remapped_span = Span {
            start: span.start - start_idx,
            end: span.end - start_idx,
        };

        // FIXME
        let str = self
            .lookup_str(Span {
                start: start_idx,
                end: end_idx,
            })
            .unwrap();
        Ok((fid_a, str, line_idx_a + 1, remapped_span))
    }

    pub fn lookup_line(&self, byte_pos: u32) -> Result<&str, LookupError> {
        let fid = self.lookup_file(byte_pos)?;

        let meta = &self.metadata[fid.0 as usize];

        let line_idx = meta
            .line_heads
            .binary_search(&byte_pos)
            .map_or_else(|x| x - 1, |x| x);

        let start_idx = meta.line_heads[line_idx];
        let end_idx = meta
            .line_heads
            .get(line_idx + 1)
            .copied()
            .unwrap_or(meta.span.end + 1)
            - 1;

        // FIXME
        let str = self
            .lookup_str(Span {
                start: start_idx,
                end: end_idx,
            })
            .unwrap();
        Ok(str)
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
