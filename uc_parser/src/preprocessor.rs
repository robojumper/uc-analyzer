//! An implementation of an UnrealScript preprocessor.
//! The UCC preprocessor eagerly (and recursively, in the case of
//! macro arguments) expands source files and macros, losing quite
//! a bit of span information -- mapping of expanded output to source --
//! in the process (in fact, the entire compiler relies on the fact
//! that macro expansion preserves line numbers, and macros containing \n
//! cause error messages in the wrong lines). This preprocessor
//! tries to preserve as much information as possible.
//! 
//! The unfortunate thing is that macros themselves can affect how
//! the preprocessor expands future things. A simple example is that
//! 
//! ```text
//! `if(0)
//!     `ThisMacroDoesntExist
//! `endif
//! ```
//! expands just fine, since the preprocessor just ignores the contents
//! of the block.
//! 
//! A more complex, but unrealistic example is
//! 
//! ```text
//! `define MyMacro 5 // comment
//! 
//! MyInt = `MyMacro;
//! ```
//! 
//! Interesting test case:
//! 
//! ```text
//! `define `if(`cond)if (`cond) `{endif}LogInternal(`msg`if(`tag),`tag`endif)
//! 
//! `define ShowEnum(enum,expr,name) "`if(`name)`name`else`expr`endif:'"$GetEnum(Enum'`enum',`expr)$"'"
//! 
//! `log(`location @ `ShowEnum(EnumType, MyExpr, MyName),,'MyLogFile');
//! ```

use std::{collections::HashMap, marker::PhantomData, ops::ControlFlow};

use uc_files::Files;

use crate::{arc_owned_chars::ArcOwnedChars, Identifier, Span};

/// A fragment of source code that begins and ends in the same file.
/// It might be interrupted by a macro definition, preprocessor defines
/// or macro invocations, but this data structure contains all the information
/// needed to map two positions in the expanded source code to the most
/// useful span in any of the files that participate in lexing and parsing
/// the current file.
///
/// This tree structure can be traversed in order to render the source file
/// in expanded form, and to find the aforementioned lowest complete span.
#[derive(Clone, Debug)]
pub struct SourceFragment<F: Files> {
    /// Start position of the current fragment
    expn_start: usize,
    /// Exclusive end position of the current fragment
    expn_end: usize,
    /// All children fragments
    children: Vec<FragmentContents<F>>,
    /// Parent fragment
    parent_fragment: Option<FragmentRef<F>>,
    /// Original source code
    span: Span<F>,
}

#[derive(Clone, Debug)]
enum FragmentContents<F: Files> {
    /// No fragmentation going on. `span` 1:1 contains the characters.
    WholeText,
    /// A macro was called.
    MacroCall {
        replacement: FragmentRef<F>,
        macro_span: Span<F>,
        macro_name: Identifier,
    },
    /// A macro contained a backslash followed by an n, causing a newline.
    MacroNewline,
}

#[derive(Clone, Copy, Debug)]
struct FragmentRef<F: Files>(usize, std::marker::PhantomData<F>);

#[derive(Clone, Debug)]
pub struct FragmentedFile<F: Files> {
    fragments: Vec<SourceFragment<F>>,
    root_fragment: FragmentRef<F>,
}

trait CharacterSource: std::fmt::Debug {
    fn peek(&self) -> Option<char>;
    fn bump(&mut self);
    fn pos(&self) -> usize;
    fn is_macro_text(&self) -> bool;
}

struct SpanTracker {
    source: Box<dyn CharacterSource>,
}

#[derive(Debug)]
struct FileParser<F: Files> {
    stack: Vec<Box<dyn CharacterSource>>,
    fragments: Vec<SourceFragment<F>>,
}

#[derive(Debug)]
struct PlaintextSource<F: Files> {
    chars: ArcOwnedChars,
    span: Span<F>,
    is_macro: bool,
}

struct ExpansionEnv<F: Files> {
    macro_defs: Vec<MacroDef<F>>,
    macro_table: HashMap<Identifier, usize>,
}

pub enum MacroDef<F: Files> {
    DefinedMacro(),
    MacroArg(),
}

impl<F: Files> FileParser<F> {}

pub fn parse_file<I, F: Files<FileId = I>>(files: &mut F, file: I) -> FragmentedFile<F> {
    let unexp_contents = files.get_file(file);
    let mut parser = FileParser {
        stack: vec![],
        fragments: vec![],
    };

    FragmentedFile {
        fragments: parser.fragments,
        root_fragment: FragmentRef(parser.fragments.len() - 1, PhantomData::default()),
    }
}

impl<F: Files> FragmentedFile<F> {
    pub fn lookup(&self, expn_start: usize, expn_end: usize) -> (Span<F>, Vec<&Span<F>>) {
        let mut spans = vec![];
        match self.lookup_internal(self.root_fragment, expn_start, expn_end, &mut spans) {
            ControlFlow::Continue(_) => panic!(
                "expn_start={}, expn_end={} not in expanded file",
                expn_start, expn_end
            ),
            ControlFlow::Break(s) => (s, spans),
        }
    }

    pub fn lookup_internal(
        &self,
        fragment: FragmentRef<F>,
        expn_start: usize,
        expn_end: usize,
        spans: &mut Vec<&Span<F>>,
    ) -> ControlFlow<Span<F>, ()> {
        let frag = self.fragments[fragment.0];
        todo!();
    }
}
