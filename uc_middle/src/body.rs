use uc_files::Span;

use crate::{DefId, ty::Ty};

#[derive(Debug)]
pub struct Body {
    pub blocks: Vec<Block>,
    pub statements: Vec<Statement>,
    pub exprs: Vec<Expr>,
}


#[derive(Debug)]
pub struct BlockId(u32);

#[derive(Debug)]
pub struct ExprId(u32);

#[derive(Debug)]
pub struct StmtId(u32);

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<StmtId>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span
}

#[derive(Debug)]
pub enum StatementKind {
    /// A simple expression statement
    Expr(ExprId),
    /// If-else?
    If(ExprId, BlockId, Option<BlockId>),
    /// An infinite loop. The loop-specific statements and expressions
    /// that cause the loop to end are within the block and can be
    /// identified by looking at the LoopDesugaring
    Loop(BlockId, LoopDesugaring),
    /// Switch, with the scrutinee expression, the case clauses,
    /// and the number of statements in that block to skip
    Switch(ExprId, Vec<(ExprId, u32)>, BlockId),
    /// Return
    Return(Option<ExprId>),
    Break,
    Continue,
    Assign(ExprId, ExprId),
}

#[derive(Debug)]
pub enum LoopDesugaring {
    For { init: StmtId, cond: ExprId, retry: StmtId },
    While { cond: ExprId },
    Do { cond: ExprId },
    Foreach { init: StmtId, cond: ExprId, next: StmtId },
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span
}

#[derive(Debug)]
pub enum ExprKind {
    Value(ValueExprKind),
    Place(PlaceExprKind),
}

#[derive(Debug)]
pub enum ValueExprKind {
    /// A literal of any kind
    Lit(Literal),
    /// Access to a class' constant
    Const(DefId),
    /// Access to a function of a particular object,
    /// for delegate assignment or call
    FuncAccess(ExprId, DefId),
    /// Function call, with function, receiver, and args
    FuncCall(DefId, ExprId, Box<[ExprId]>),
    /// Delegate call, with delegate expr and args
    DelegateCall(ExprId, Box<[ExprId]>),
    /// Operators are conventionally static, so there's no receiver
    OpCall(DefId, ExprId, Option<ExprId>),
    /// x ? y : z
    TernaryOp(ExprId, ExprId, ExprId),
    /// `new (a, b) c (d)`
    NewExpr(Option<ExprId>, Option<ExprId>, ExprId, Option<ExprId>),
    /// string(myname), class<Actor>(obj)
    CastExpr(Ty, ExprId),
}

#[derive(Debug)]
pub enum PlaceExprKind {
    Local(DefId),
    Index(ExprId, ExprId),
    Field(ExprId, DefId),
}

#[derive(Debug)]
pub enum Literal {
    Bool,
    Int,
    Name,
    String,
    Object(DefId),
    Class(DefId),
}