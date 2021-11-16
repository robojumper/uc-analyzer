#![cfg(test)]

use std::str::FromStr;

use uc_ast::pretty;
use uc_files::Sources;
use uc_name::Identifier;

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

#[test]
fn class_metacast() {
    let text = "class<Actor>(SomeObject)";
    // Definitely not "(class < Actor) > SomeObject"
    let pretty = "class<Actor>(SomeObject)";
    assert_parse_pretty(text, pretty);
}

#[test]
fn class_simplecast() {
    let text = "Class(SomeObject)";
    // Just a regular function call
    let pretty = "Class(SomeObject)";
    assert_parse_pretty(text, pretty);
}

#[test]
fn new_op() {
    let text = "new(Outer) ModifierClass";
    let pretty = "(new (Outer) ModifierClass)";
    assert_parse_pretty(text, pretty);
}

fn assert_parse_pretty(text: &str, pretty: &str) {
    let mut sources = Sources::new();
    let id = sources
        .add_file(Identifier::from_str("TestFile").unwrap(), text.as_bytes())
        .unwrap();
    let lex = Lexer::new(&sources, id);
    let mut p = Parser::new(lex);
    let expr = p.parse_base_expression().unwrap();
    let mut out = vec![];

    pretty::format_base_expr(&expr, &mut out).unwrap();
    let parse_pretty = std::str::from_utf8(&out).unwrap();
    assert_eq!(parse_pretty, pretty)
}
