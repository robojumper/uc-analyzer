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
pub enum Values<T> {
    Absent,
    Nums(Box<[i32]>),
    Idents(Box<[T]>),
}

#[derive(Debug)]
pub struct Modifiers<F: Flags, T> {
    pub flags: F,
    pub followups: HashMap<F, Option<Values<T>>>,
}

#[derive(Debug)]
pub struct Hir<T> {
    pub header: ClassDef<T>,
    pub structs: Vec<StructDef<T>>,
    pub enums: Vec<EnumDef>,
    pub consts: Vec<ConstDef>,
    pub vars: Vec<VarDef<T>>,
    pub states: Vec<StateDef<T>>,
    pub funcs: Vec<FuncDef<T>>,
}

#[derive(Debug)]
pub enum ClassHeader<T> {
    Class {
        extends: Option<T>,
        implements: Vec<T>,
        within: Option<T>,
        flags: ClassFlags,
    },
    Interface {
        extends: Option<T>,
    },
}

#[derive(Debug)]
pub struct ClassDef<T> {
    pub name: Identifier,
    pub kind: ClassHeader<T>,
    pub mods: Modifiers<ClassFlags, T>,
    pub span: Span,
}

#[derive(Debug)]
pub enum DimCount<T> {
    None,
    Number(u32),
    Complex(Vec<T>),
}

#[derive(Debug)]
pub enum Ty<T> {
    Simple(T),
    Qualified(Vec<T>),
    Array(Box<Ty<T>>),
    Class(Option<T>),
    Delegate(Vec<T>),
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
pub struct VarInstance<T> {
    pub name: Identifier,
    pub count: DimCount<T>,
}

#[derive(Debug)]
pub struct VarDef<T> {
    pub ty: Ty<T>,
    pub names: Vec<VarInstance<T>>,
    pub mods: Modifiers<VarFlags, T>,
    pub span: Span,
}

#[derive(Debug)]
pub struct EnumDef {
    pub name: Identifier,
    pub variants: Vec<Identifier>,
    pub span: Span,
}

#[derive(Debug)]
pub struct StructDef<T> {
    pub name: Identifier,
    pub extends: Option<Vec<T>>,
    pub fields: Vec<VarDef<T>>,
    pub mods: Modifiers<StructFlags, T>,
    pub span: Span,
}

#[derive(Debug)]
pub enum FuncName {
    Oper(Op),
    Iden(Identifier),
}

#[derive(Debug)]
pub struct StateDef<T> {
    pub name: T,
    pub extends: Option<T>,
    pub funcs: Vec<FuncDef<T>>,
    pub statements: Vec<Statement<T>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FuncDef<T> {
    pub name: FuncName,
    pub overrides: Option<T>,
    pub mods: Modifiers<FuncFlags, T>,
    pub sig: FuncSig<T>,
    pub body: Option<FuncBody<T>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FuncSig<T> {
    pub ret_ty: Option<Ty<T>>,
    pub args: Vec<FuncArg<T>>,
}

#[derive(Debug)]
pub struct FuncArg<T> {
    pub ty: Ty<T>,
    pub name: Identifier,
    pub count: DimCount<T>,
    pub def: Option<Expr<T>>,
    pub mods: Modifiers<ArgFlags, T>,
}

#[derive(Debug)]
pub struct FuncBody<T> {
    pub locals: Vec<Local<T>>,
    pub consts: Vec<ConstDef>,
    pub statements: Vec<Statement<T>>,
}

#[derive(Debug)]
pub struct Statement<T> {
    pub span: Span,
    pub kind: StatementKind<T>,
}

#[derive(Debug)]
pub enum StatementKind<T> {
    IfStatement {
        cond: Expr<T>,
        then: Block<T>,
        or_else: Option<Block<T>>,
    },
    ForStatement {
        init: Box<Statement<T>>,
        cond: Expr<T>,
        retry: Box<Statement<T>>,
        run: Block<T>,
    },
    ForeachStatement {
        source: Expr<T>,
        run: Block<T>,
    },
    WhileStatement {
        cond: Expr<T>,
        run: Block<T>,
    },
    DoStatement {
        cond: Expr<T>,
        run: Vec<Statement<T>>,
    },
    SwitchStatement {
        scrutinee: Expr<T>,
        cases: Vec<CaseClause<T>>,
    },
    BreakStatement,
    ContinueStatement,
    ReturnStatement {
        expr: Option<Expr<T>>,
    },
    Label(T),
    Assignment {
        lhs: Expr<T>,
        rhs: Expr<T>,
    },
    Expr {
        expr: Expr<T>,
    },
}

#[derive(Debug)]
pub struct CaseClause<T> {
    pub case: Case<T>,
    pub case_span: Span,
    pub statements: Vec<Statement<T>>,
}

#[derive(Debug)]
pub enum Case<T> {
    Case(Expr<T>),
    Default,
}

#[derive(Debug)]
pub struct Block<T> {
    pub stmts: Vec<Statement<T>>,
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
pub enum Expr<T> {
    IndexExpr {
        base: Box<Expr<T>>,
        idx: Box<Expr<T>>,
    },
    FieldExpr {
        lhs: Box<Expr<T>>,
        rhs: T,
    },
    CallExpr {
        lhs: Box<Expr<T>>,
        args: Vec<Option<Expr<T>>>,
    },
    ClassMetaCastExpr {
        ty: Ty<T>,
        expr: Box<Expr<T>>,
    },
    NewExpr {
        args: Vec<Expr<T>>,
        cls: Box<Expr<T>>,
        arch: Option<Box<Expr<T>>>,
    },
    PreOpExpr {
        op: Op,
        rhs: Box<Expr<T>>,
    },
    PostOpExpr {
        lhs: Box<Expr<T>>,
        op: Op,
    },
    BinOpExpr {
        lhs: Box<Expr<T>>,
        op: Op,
        rhs: Box<Expr<T>>,
    },
    TernExpr {
        cond: Box<Expr<T>>,
        then: Box<Expr<T>>,
        alt: Box<Expr<T>>,
    },
    SymExpr {
        sym: T,
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
pub struct Local<T> {
    pub ty: Ty<T>,
    pub names: Vec<VarInstance<T>>,
}
