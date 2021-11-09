# uc-analyzer

## Goals

* Analyze a complete XCOM 2 mod project with more sophisticated analysis than UCC.
* Understand XCOM 2 mod project setups enough to analyze the Community Highlander, Lwotc, CI
  projects.

## Non-Goals

* Mirroring the behavior of UCC exactly.
  * We want to catch errors UCC doesn't, not the ones it does.

## Encodings

UCC accepts ASCII, extended ASCII, UTF-8, and UTF-16 with a BOM.
A BOM puts UCC into wide char mode, while a lack of a
BOM puts the UCC into extended ASCII mode, i.e. any non-ASCII UTF-8
codepoints are simply interpreted as 2, 3, or 4 extended ASCII characters.

We solve this problem by converting BOM'ed UTF-16 to UTF-8, and just feeding
raw bytes to the lexer. The lexer will reject all non-ASCII characters that
appear outside of comments and strings.
