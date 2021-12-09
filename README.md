# uc-analyzer

## Goals

* Analyze a complete XCOM 2 mod project with more sophisticated analysis than UCC.
* Understand XCOM 2 mod project setups enough to analyze the Community Highlander, Lwotc, CI
  projects.

## Non-Goals

* Mirroring the behavior of UCC exactly.
  * We want to catch errors UCC doesn't, not the ones it does.

## Crates

* `uc_analyzer`: Main executable. Currently does whatever I'm working on.
* `uc_analysis`: Librarified lints, used by the main executable.
* `uc_files`: Simple file storage, span lookup and error reporting through `annotate-snippets`.
  * I'd like to use `codespan` for this but it can't handle non-UTF-8 text (see below).
* `uc_name`: Encapsulated case-insensitive ASCII string.
* `uc_def`: Core definitions likely to be relevant in many crates, such as bitflags and operators.
* `uc_ast`: Definition and pretty-printing of the UnrealScript AST.
* `uc_parser`: Lexer and parser that produces this AST.
* `uc_middle`: Fully resolved UnrealScript workspace.
* `uc_ast_lowering`: Lowers and resolves AST to middle representation.

## Encodings

UCC accepts ASCII, extended ASCII, UTF-8, and UTF-16 with a BOM.
A BOM puts UCC into wide char mode, while a lack of a
BOM puts the UCC into extended ASCII mode, i.e. any non-ASCII UTF-8
codepoints are simply interpreted as 2, 3, or 4 extended ASCII characters.

We solve this problem by converting BOM'ed UTF-16 to UTF-8, and just feeding
raw bytes to the lexer. The lexer will reject all non-ASCII characters that
appear outside of comments and strings.

## Defects

The compiler accepts some pretty clearly invalid syntax and has some
questionable behavior, not all of which I want to accept in this
implementation. The different behavior is documented.

* `defaultproperties` blocks may be not closed, or may contain arbitrary
  bytes after the closing brace on the same line.
  * we reject this
* Overloaded operators may have a different priority from other
  implementations of the same operator.
  * we ignore precedence annotations and hardcode sane ones.
* Arbitrary new operators can be defined as long as their name is a valid
  token.
  * we only recognize the sigil operators, `cross`, `dot`, and `clockwisefrom`.
* Conflicts between type names are allowed and UCC just picks the one
  loaded/compiled later.
  * we, in the case of ambiguity, prefer a type from the definition hierarchy,
    if there are none or more than one, error.
* `new CallOrVar(ArgOrTemplate)` can, syntactically, be a single-argument
  function returning a class, or be a local property followed by the object
  template for the `new` operator.
  * aaaaaaaahh

## TODO Lints

* Operating on returned arrays directly (e.g. `GetSomeArray()[0]` causes VM issues)
* Never assigned out variable
* Dead assignment
* Access to const through member variable syntax