#![feature(iter_intersperse)]

use std::collections::HashMap;

pub use uc_def;
use uc_def::{ArgFlags, ClassFlags, Flags, FuncFlags, Op, StructFlags, VarFlags};
use uc_files::Span;
use uc_name::Identifier;

pub mod pretty;
pub mod visit;

#[derive(Debug)]
pub enum Values {
    Absent,
    Nums(Box<[i32]>),
    Idents(Box<[Identifier]>),
}

#[derive(Debug)]
pub struct Modifiers<F: Flags> {
    pub flags: F,
    pub followups: HashMap<F, Option<Values>>,
}

#[derive(Debug)]
pub struct Hir {
    pub header: ClassDef,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
    pub consts: Vec<ConstDef>,
    pub vars: Vec<VarDef>,
    pub states: Vec<StateDef>,
    pub funcs: Vec<FuncDef>,
}

#[derive(Debug)]
pub enum ClassHeader {
    Class { extends: Option<Identifier>, implements: Vec<Identifier>, within: Option<Identifier> },
    Interface { extends: Option<Identifier> },
}

#[derive(Debug)]
pub struct ClassDef {
    pub name: Identifier,
    pub kind: ClassHeader,
    pub dependson: Box<[Identifier]>,
    pub mods: Modifiers<ClassFlags>,
    pub span: Span,
}

#[derive(Debug)]
pub enum DimCount {
    None,
    Number(u32),
    Complex(Vec<Identifier>),
}

#[derive(Debug)]
pub enum Ty {
    Simple(Identifier),
    Qualified(Vec<Identifier>),
    Array(Box<Ty>),
    Class(Identifier),
    Delegate(Vec<Identifier>),
}

#[derive(Debug)]
pub struct ConstDef {
    pub name: Identifier,
    pub val: ConstVal,
    pub span: Span,
}

#[derive(Debug)]
pub enum ConstVal {
    Literal(Literal),
    ValueReference(Identifier),
}

#[derive(Debug)]
pub struct VarInstance {
    pub name: Identifier,
    pub count: DimCount,
    pub span: Span,
}

#[derive(Debug)]
pub struct VarDef {
    pub ty: Ty,
    pub names: Vec<VarInstance>,
    pub mods: Modifiers<VarFlags>,
    pub span: Span,
}

#[derive(Debug)]
pub struct EnumDef {
    pub name: Identifier,
    pub variants: Vec<(Span, Identifier)>,
    pub span: Span,
}

#[derive(Debug)]
pub struct StructDef {
    pub name: Identifier,
    pub extends: Option<Vec<Identifier>>,
    pub fields: Vec<VarDef>,
    pub mods: Modifiers<StructFlags>,
    pub span: Span,
}

#[derive(Debug)]
pub enum FuncName {
    Oper(Op),
    Iden(Identifier),
}

#[derive(Debug)]
pub struct StateDef {
    pub name: Identifier,
    pub extends: Option<Identifier>,
    pub funcs: Vec<FuncDef>,
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FuncDef {
    pub name: FuncName,
    pub overrides: Option<Identifier>,
    pub mods: Modifiers<FuncFlags>,
    pub sig: FuncSig,
    pub body: Option<FuncBody>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FuncSig {
    pub ret_ty: Option<Ty>,
    pub args: Vec<FuncArg>,
}

#[derive(Debug)]
pub struct FuncArg {
    pub ty: Ty,
    pub name: Identifier,
    pub count: DimCount,
    pub def: Option<Expr>,
    pub mods: Modifiers<ArgFlags>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FuncBody {
    pub locals: Vec<LocalDef>,
    pub consts: Vec<ConstDef>,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    IfStatement { cond: Expr, then: Block, or_else: Option<Block> },
    ForStatement { init: Box<Statement>, cond: Expr, retry: Box<Statement>, run: Block },
    ForeachStatement { ctx: Box<Context>, name: Identifier, args: Vec<Option<Expr>>, run: Block },
    WhileStatement { cond: Expr, run: Block },
    DoStatement { cond: Expr, run: Block },
    SwitchStatement { scrutinee: Expr, cases: Vec<CaseClause> },
    BreakStatement,
    ContinueStatement,
    ReturnStatement { expr: Option<Expr> },
    Label { name: Identifier },
    Assignment { lhs: Expr, rhs: Expr },
    Expr { expr: Expr },
}

#[derive(Debug)]
pub struct CaseClause {
    pub case: Case,
    pub case_span: Span,
    pub stmts: Vec<Statement>,
}

#[derive(Debug)]
pub enum Case {
    Case(Expr),
    Default,
}

#[derive(Debug)]
pub struct Block {
    pub from_single_stmt: bool,
    pub stmts: Vec<Statement>,
}

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub paren: bool,
    pub kind: ExprKind,
}

/// It'd be nice to syntactically split this into field and function
/// contexts, but syntactically,
#[derive(Debug)]
pub enum Context {
    /// Thing
    Bare,
    /// global.Function(...)
    Global,
    /// Super(...).Function(...)
    Super(Option<Identifier>),
    /// static.Function.Thing, Something.static.Thing
    Static(Option<Expr>),
    /// default.Thing, Something.default.Thing
    Default(Option<Expr>),
    /// default.Thing, Something.default.Thing
    Const(Option<Expr>),
    /// xyz.Thing
    Expr(Expr),
}

#[derive(Debug)]
pub enum ExprKind {
    IndexExpr { base: Box<Expr>, idx: Box<Expr> },
    FieldExpr { lhs: Box<Context>, rhs: Identifier },
    FuncCallExpr { lhs: Box<Context>, name: Identifier, args: Vec<Option<Expr>> },
    ClassMetaCastExpr { ty: Ty, expr: Box<Expr> },
    NewExpr { args: Vec<Option<Expr>>, cls: Box<Expr>, arch: Option<Box<Expr>> },
    PreOpExpr { op: Op, rhs: Box<Expr> },
    PostOpExpr { lhs: Box<Expr>, op: Op },
    BinOpExpr { lhs: Box<Expr>, op: Op, rhs: Box<Expr> },
    TernExpr { cond: Box<Expr>, then: Box<Expr>, alt: Box<Expr> },
    LiteralExpr { lit: Literal },
}

#[derive(Debug)]
pub enum Literal {
    None,
    ObjReference(Identifier, Identifier),
    Float(f32),
    Int(i32),
    Bool(bool),
    Name(Identifier),
    String(Box<str>),
}

#[derive(Debug)]
pub struct LocalDef {
    pub ty: Ty,
    pub names: Vec<VarInstance>,
}
