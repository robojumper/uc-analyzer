use std::{fmt::Display, str::FromStr};

use bitflags::bitflags;

pub mod pretty;

bitflags! {
    pub struct ClassFlags: u32 {
        const NATIVE = 1 << 0;
        const CONFIG = 1 << 1;
        const PEROBJECTCONFIG = 1 << 2;
        const IMPLEMENTS = 1 << 3;
        const ABSTRACT = 1 << 4;
    }

    pub struct InterfaceFlags: u32 {
        const NATIVE = 1 << 0;
    }

    pub struct PropFlags: u32 {
        const LOCALIZED = 1 << 0;
        const CONFIG = 1 << 1;
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(unicase::Ascii<String>);

impl FromStr for Identifier {
    type Err = <unicase::Ascii<String> as FromStr>::Err;
    fn from_str(t: &str) -> Result<Self, Self::Err> {
        Ok(Self(unicase::Ascii::from_str(t)?))
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.0.as_str(), f)
    }
}

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.0.as_str(), f)
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

#[derive(Debug)]
pub struct Hir<T> {
    pub header: ClassDef<T>,
    pub structs: Vec<StructDef<T>>,
    pub enums: Vec<EnumDef>,
    pub consts: Vec<ConstDef>,
    pub vars: Vec<VarDef<T>>,
    pub dels: Vec<DelegateDef<T>>,
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
    Array(T),
    Class(Option<T>),
    Delegate(Vec<T>),
}

#[derive(Debug)]
pub struct ConstDef {
    pub name: Identifier,
    pub val: ConstVal,
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
    pub flags: PropFlags,
}

#[derive(Debug)]
pub struct EnumDef {
    pub name: Identifier,
    pub variants: Vec<Identifier>,
}

#[derive(Debug)]
pub struct StructDef<T> {
    pub name: Identifier,
    pub extends: Option<T>,
    pub fields: Vec<VarDef<T>>,
}

#[derive(Debug)]
pub struct DelegateDef<T> {
    pub name: Identifier,
    pub sig: FuncSig<T>,
}

#[derive(Debug)]
pub struct FuncDef<T> {
    pub name: Identifier,
    pub overrides: Option<T>,
    pub flags: FuncFlags,
    pub sig: FuncSig<T>,
    pub body: Option<FuncBody<T>>,
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
    pub val: Option<ConstVal>,
}

#[derive(Debug)]
pub struct FuncBody<T> {
    pub locals: Vec<Local<T>>,
    pub statements: Vec<Statement<T>>,
}

#[derive(Debug)]
pub enum Statement<T> {
    IfStatement {
        cond: Expr<T>,
        then: BlockOrStatement<T>,
        or_else: Option<BlockOrStatement<T>>,
    },
    ForStatement {
        init: Box<Statement<T>>,
        cond: Expr<T>,
        every: Box<Statement<T>>,
        run: BlockOrStatement<T>,
    },
    ForeachStatement {
        source: Expr<T>,
        dest: T,
        run: BlockOrStatement<T>,
    },
    WhileStatement {
        cond: Expr<T>,
        run: BlockOrStatement<T>,
    },
    DoStatement {
        cond: Expr<T>,
        run: BlockOrStatement<T>,
    },
    SwitchStatement {
        scrutinee: Expr<T>,
        cases: Vec<CaseClause<T>>,
    },
    BreakStatement,
    ContinueStatement,
    GotoStatement,
    ReturnStatement,
    Label,
    Expression(Expr<T>),
}

#[derive(Debug)]
pub struct CaseClause<T> {
    pub case: Case<T>,
    pub statements: Vec<Statement<T>>,
}

#[derive(Debug)]
pub enum Case<T> {
    Case(Expr<T>),
    Default,
}

#[derive(Debug)]
pub enum BlockOrStatement<T> {
    Block(Vec<Statement<T>>),
    Statement(Box<Statement<T>>),
}

#[derive(Debug)]
pub enum Expr<T> {
    AssignmentExpr {
        lhs: BaseExpr<T>,
        rhs: BaseExpr<T>,
    },
    AssignmentOpExpr {
        lhs: BaseExpr<T>,
        op: T,
        rhs: BaseExpr<T>,
    },
}

#[derive(Debug)]
pub enum BaseExpr<T> {
    IndexExpr {
        base: Box<BaseExpr<T>>,
        idx: Box<BaseExpr<T>>,
    },
    ClassPropExpr {
        lhs: Box<BaseExpr<T>>,
        kind: ClassAccess,
        field: T,
    },
    FieldExpr {
        lhs: Box<BaseExpr<T>>,
        field: T,
    },
    CallExpr {
        func: Box<BaseExpr<T>>,
        args: Vec<BaseExpr<T>>,
    },
    NewExpr {
        args: Vec<BaseExpr<T>>,
        cls: Box<BaseExpr<T>>,
    },
    PreOpExpr {
        op: T,
        rhs: Box<BaseExpr<T>>,
    },
    PostOpExpr {
        lhs: Box<BaseExpr<T>>,
        op: T,
    },
    BinOpExpr {
        lhs: Box<BaseExpr<T>>,
        op: T,
        rhs: Box<BaseExpr<T>>,
    },
    TernExpr {
        cond: Box<BaseExpr<T>>,
        then: Box<BaseExpr<T>>,
        alt: Box<BaseExpr<T>>,
    },
    ExplContextExpr {
        access: ExplContextAccess,
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
    String,
}

#[derive(Debug)]
pub enum ClassAccess {
    Default,
    Static,
    Const,
}

#[derive(Debug)]
pub enum ExplContextAccess {
    Zelf,
    Default,
    Static,
    Const,
}

#[derive(Debug)]
pub struct Local<T> {
    pub ty: Ty<T>,
    pub names: Vec<VarInstance<T>>,
}
