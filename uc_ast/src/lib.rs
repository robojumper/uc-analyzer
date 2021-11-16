use std::collections::HashMap;

pub use uc_def;
use uc_def::{ArgFlags, ClassFlags, Flags, FuncFlags, StructFlags, VarFlags};
use uc_files::Span;
use uc_name::Identifier;

pub mod pretty;

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
    Class {
        extends: Option<Identifier>,
        implements: Vec<Identifier>,
        within: Option<Identifier>,
    },
    Interface {
        extends: Option<Identifier>,
    },
}

#[derive(Debug)]
pub struct ClassDef {
    pub name: Identifier,
    pub kind: ClassHeader,
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
    Class(Option<Identifier>),
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
    Bool,
    Int,
    String,
    Name,
    Float,
    /// Enum value? Other const?
    ValueReference,
}

#[derive(Debug)]
pub struct VarInstance {
    pub name: Identifier,
    pub count: DimCount,
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
    pub variants: Vec<Identifier>,
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
}

#[derive(Debug)]
pub struct FuncBody {
    pub locals: Vec<Local>,
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
    IfStatement {
        cond: Expr,
        then: Block,
        or_else: Option<Block>,
    },
    ForStatement {
        init: Box<Statement>,
        cond: Expr,
        retry: Box<Statement>,
        run: Block,
    },
    ForeachStatement {
        source: Expr,
        run: Block,
    },
    WhileStatement {
        cond: Expr,
        run: Block,
    },
    DoStatement {
        cond: Expr,
        run: Vec<Statement>,
    },
    SwitchStatement {
        scrutinee: Expr,
        cases: Vec<CaseClause>,
    },
    BreakStatement,
    ContinueStatement,
    ReturnStatement {
        expr: Option<Expr>,
    },
    Label(Identifier),
    Assignment {
        lhs: Expr,
        rhs: Expr,
    },
    Expr {
        expr: Expr,
    },
}

#[derive(Debug)]
pub struct CaseClause {
    pub case: Case,
    pub case_span: Span,
    pub statements: Vec<Statement>,
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

#[derive(Copy, Clone, Debug)]
pub enum Op {
    Add,
    AddAdd,
    AddAssign,
    And,
    AndAnd,
    At,
    AtAssign,
    Bang,
    BangEq,
    Div,
    DivAssign,
    Dollar,
    DollarAssign,
    EqEq,
    Gt,
    GtEq,
    GtGt,
    GtGtGt,
    Lt,
    LtEq,
    LtLt,
    Mod,
    Mul,
    MulMul,
    MulAssign,
    Or,
    OrOr,
    Pow,
    PowPow,
    Sub,
    SubAssign,
    SubSub,
    Tilde,
    TildeEq,

    VecCross,
    VecDot,
}

#[derive(Debug)]
pub enum Expr {
    IndexExpr {
        base: Box<Expr>,
        idx: Box<Expr>,
    },
    FieldExpr {
        lhs: Box<Expr>,
        rhs: Identifier,
    },
    CallExpr {
        lhs: Box<Expr>,
        args: Vec<Option<Expr>>,
    },
    ClassMetaCastExpr {
        ty: Ty,
        expr: Box<Expr>,
    },
    NewExpr {
        args: Vec<Expr>,
        cls: Box<Expr>,
        arch: Option<Box<Expr>>,
    },
    PreOpExpr {
        op: Op,
        rhs: Box<Expr>,
    },
    PostOpExpr {
        lhs: Box<Expr>,
        op: Op,
    },
    BinOpExpr {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
    TernExpr {
        cond: Box<Expr>,
        then: Box<Expr>,
        alt: Box<Expr>,
    },
    SymExpr {
        sym: Identifier,
    },
    LiteralExpr {
        lit: Literal,
    },
}

#[derive(Debug)]
pub enum Literal {
    None,
    ObjReference,
    Number,
    Bool,
    Name,
    String(Box<str>),
}

#[derive(Debug)]
pub struct Local {
    pub ty: Ty,
    pub names: Vec<VarInstance>,
}
