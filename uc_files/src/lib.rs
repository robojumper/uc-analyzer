use std::{borrow::Cow, cmp::Ordering, collections::HashMap, mem, num::NonZeroU32, path::PathBuf};

use annotate_snippets::{
    display_list::{self, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct BytePos(NonZeroU32);

impl BytePos {
    #[cfg_attr(debug_assertions, track_caller)]
    pub fn new(x: u32) -> Self {
        Self(NonZeroU32::new(x).unwrap())
    }

    pub fn get(self) -> u32 {
        self.0.get()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    pub start: BytePos,
    pub end: BytePos,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FileId(u32);

#[derive(Default, Debug)]
pub struct Sources {
    data: Vec<u8>,
    metadata: Vec<SourceFileMetadata>,
    files_names: HashMap<String, FileId>,
}

#[derive(Debug)]
struct SourceFileMetadata {
    name: String,
    _path: PathBuf,
    span: Span,
    line_heads: Vec<BytePos>,
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
    pub fragments: Vec<Fragment>,
}

pub struct Fragment {
    pub full_text: Span,
    pub inlay_messages: Vec<(String, Span)>,
}

/// Work around lifetime issues in annotate-snippets.
/// <https://github.com/rust-lang/annotate-snippets-rs/issues/13> is
/// considering using Cow instead of &str, which would fix this.
struct DecodedFragment<'a> {
    pub source: String,
    pub line_start: usize,
    pub origin: Option<&'a str>,
    pub annotations: Vec<SourceAnnotation<'a>>,
}

impl Sources {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(
        &mut self,
        name: String,
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

        self.data.push(0u8);
        let start = BytePos::new(self.data.len() as u32);
        self.data.extend_from_slice(&*data);
        let end = BytePos::new(self.data.len() as u32);

        let span = Span { start, end };

        let file_id = FileId(self.metadata.len() as u32);
        let line_heads = std::iter::once(start)
            .chain(data.iter().enumerate().filter_map(|(idx, &b)| {
                if b == b'\n' { Some(BytePos::new(idx as u32 + start.get() + 1)) } else { None }
            }))
            .collect();
        self.metadata.push(SourceFileMetadata {
            name: name.clone(),
            _path: path,
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

    pub fn lookup_file(&self, byte_pos: BytePos) -> Result<FileId, LookupError> {
        if byte_pos.get() > self.data.len() as u32 {
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
        &self.data[span.start.get() as usize..span.end.get() as usize]
    }

    #[inline]
    pub fn file_name(&self, f_id: FileId) -> &str {
        &self.metadata[f_id.0 as usize].name
    }

    #[inline]
    pub fn lookup_str(&self, span: Span) -> Result<&str, LookupError> {
        std::str::from_utf8(self.lookup_bytes(span)).map_err(|_| LookupError::NonUtf8)
    }

    fn parse_slice<'a>(&'a self, fragment: &'a Fragment) -> DecodedFragment<'a> {
        let full_span = fragment.full_text;
        let (fid, source, line_start, remapped_span) = self.lookup_err_ctx(full_span).unwrap();

        // replace tabs with 4 spaces and adjust spans
        let tab_indices = source
            .bytes()
            .enumerate()
            .filter_map(|(idx, b)| if b == b'\t' { Some(idx) } else { None })
            .collect::<Vec<_>>();

        let new_pos = |b_pos: u32| {
            let num_prev_tabs =
                tab_indices.binary_search(&(b_pos as usize)).map_or_else(|x| x, |x| x);
            b_pos + (num_prev_tabs as u32 * 3)
        };

        let source = source.replace('\t', "    ");

        let annotations = fragment
            .inlay_messages
            .iter()
            .map(|(msg, span)| {
                assert!(span.start >= full_span.start && span.end <= full_span.end);
                let new_span = (
                    new_pos(span.start.get() - full_span.start.get() + remapped_span.0) as usize,
                    new_pos(span.end.get() - full_span.start.get() + remapped_span.0) as usize,
                );
                SourceAnnotation {
                    range: new_span,
                    label: msg,
                    annotation_type: AnnotationType::Warning,
                }
            })
            .collect();

        DecodedFragment {
            source,
            line_start,
            origin: Some(self.metadata[fid.0 as usize].name.as_ref()),
            annotations,
        }
    }

    pub fn emit_err(&self, err: &ErrorReport) {
        let mut fragments = err.fragments.iter().map(|f| self.parse_slice(f)).collect::<Vec<_>>();
        let slices = fragments
            .iter_mut()
            .map(|f| Slice {
                source: &f.source,
                line_start: f.line_start,
                origin: f.origin,
                annotations: mem::take(&mut f.annotations),
                fold: false,
            })
            .collect();
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

    /// -> FileID, data, first line number, byte positions within data
    #[allow(clippy::type_complexity)]
    fn lookup_err_ctx(&self, span: Span) -> Result<(FileId, &str, usize, (u32, u32)), LookupError> {
        let fid_a = self.lookup_file(span.start)?;
        let fid_b = self.lookup_file(span.end)?;
        if fid_a != fid_b {
            return Err(LookupError::CrossFileSpan);
        }

        let meta = &self.metadata[fid_a.0 as usize];

        let line_idx_a = meta.line_heads.binary_search(&span.start).map_or_else(|x| x - 1, |x| x);
        let line_idx_b = meta.line_heads.binary_search(&span.end).map_or_else(|x| x - 1, |x| x);

        let start_idx = meta.line_heads[line_idx_a];
        let end_idx = BytePos::new(
            meta.line_heads
                .get(line_idx_b + 1)
                .copied()
                .unwrap_or_else(|| BytePos::new(meta.span.end.get() + 1))
                .get()
                - 1,
        );

        let new_start = span.start.get() - start_idx.get();
        let new_end = span.end.get() - start_idx.get();

        // FIXME
        let str = self.lookup_str(Span { start: start_idx, end: end_idx }).unwrap();
        Ok((fid_a, str, line_idx_a + 1, (new_start, new_end)))
    }

    pub fn lookup_line(&self, byte_pos: BytePos) -> Result<&str, LookupError> {
        let fid = self.lookup_file(byte_pos)?;

        let meta = &self.metadata[fid.0 as usize];

        let line_idx = meta.line_heads.binary_search(&byte_pos).map_or_else(|x| x - 1, |x| x);

        let start_idx = meta.line_heads[line_idx];
        let end_idx = BytePos::new(
            meta.line_heads
                .get(line_idx + 1)
                .copied()
                .unwrap_or_else(|| BytePos::new(meta.span.end.get() + 1))
                .get()
                - 1,
        );

        // FIXME
        let str = self.lookup_str(Span { start: start_idx, end: end_idx }).unwrap();
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
