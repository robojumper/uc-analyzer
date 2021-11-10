use std::{collections::HashMap, hash::Hash};

use bitflags::bitflags;
use uc_files::Span;
use uc_name::Identifier;

pub mod pretty;

bitflags! {
    pub struct ClassFlags: u32 {
        const INTERFACE = 1 << 0;
        const NATIVE = 1 << 1;
        const CONFIG = 1 << 2;
        const PEROBJECTCONFIG = 1 << 3;
        const IMPLEMENTS = 1 << 4;
        const ABSTRACT = 1 << 5;
    }

    pub struct InterfaceFlags: u32 {
        const NATIVE = 1 << 0;
    }

    pub struct FuncFlags: u32 {
        const EVENT = 1 << 0;
        const SIMULATED = 1 << 1;
        const NATIVE = 1 << 2;
        const OPERATOR = 1 << 3;
        const PREOPERATOR = 1 << 4;
        const POSTOPERATOR = 1 << 5;
        const STATIC = 1 << 6;
        const FINAL = 1 << 7;
        const EXEC = 1 << 8;
        const LATENT = 1 << 9;
        const PUBLIC = 1 << 10;
        const PRIVATE = 1 << 11;
        const PROTECTED = 1 << 12;
        const COERCE = 1 << 13;
        const ITERATOR = 1 << 14;
        const DELEGATE = 1 << 15;
    }

    pub struct VarFlags: u32 {
        const NATIVE = 1 << 0;
        const CONFIG = 1 << 1;
        const GLOBALCONFIG = 1 << 2;
        const LOCALIZED = 1 << 3;
        const CONST = 1 << 4;
        const PUBLIC = 1 << 5;
        const PROTECTED = 1 << 6;
        const PROTECTEDWRITE = 1 << 7;
        const PRIVATE = 1 << 8;
        const PRIVATEWRITE = 1 << 9;
    }

    pub struct ArgFlags: u32 {
        const COERCE = 1 << 0;
        const CONST = 1 << 1;
        const OPTIONAL = 1 << 2;
        const SKIP = 1 << 3;
        const OUT = 1 << 4;
        const REF = 1 << 5;
    }

    pub struct StructFlags: u32 {

    }
}

pub trait Flags: Copy + Eq + Hash {
    fn into_raw(self) -> u32;
    fn from_raw(bits: u32) -> Self;
}

macro_rules! impl_flags_for_bitflags {
    ($($t:ty),+ $(,)?) => {
        $(
            impl Flags for $t {
                fn into_raw(self) -> u32 {
                    self.bits()
                }

                fn from_raw(bits: u32) -> Self {
                    let ret = <$t>::from_bits_truncate(bits);
                    assert_eq!(bits, ret.bits());
                    ret
                }
            }
        )+
    }
}

impl_flags_for_bitflags! {
    ClassFlags, FuncFlags, VarFlags, ArgFlags, StructFlags,
}

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
        flags: ClassFlags,
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
