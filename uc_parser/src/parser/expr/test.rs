#![cfg(test)]

use uc_def::pretty;

use crate::{lexer::Lexer, parser::Parser};

#[test]
fn object_913() {
    let text = "InMin + (InMax - InMin) * FRand()";
    let pretty = "(InMin + ((InMax - InMin) * FRand()))";
    assert_parse_pretty(text, pretty);
}

#[test]
fn object_1331() {
    let text = "Mid(Text,Idx+1,Len(Text))";
    let pretty = "Mid(Text, (Idx + <Number>), Len(Text))";
    assert_parse_pretty(text, pretty);
}

#[test]
fn object_1515() {
    let text = "(Range.Y == Range.X) ? Range.X : (Value - Range.X) / (Range.Y - Range.X)";
    let pretty = "((Range.Y == Range.X) ? Range.X : ((Value - Range.X) / (Range.Y - Range.X)))";
    assert_parse_pretty(text, pretty);
}

fn assert_parse_pretty(text: &str, pretty: &str) {
    let lex = Lexer::new(text);
    let mut p = Parser::new(lex);
    let expr = p.parse_base_expression().unwrap();
    let mut out = vec![];

    pretty::format_base_expr(&expr, &mut out, pretty::IdentifierFormat).unwrap();
    let parse_pretty = std::str::from_utf8(&out).unwrap();
    assert_eq!(parse_pretty, pretty)
}