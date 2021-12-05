use std::num::NonZeroU32;

use uc_files::Span;

use crate::{ty::Ty, DefId};

#[derive(Debug, Default)]
pub struct Body {
    entry_block: Option<BlockId>,
    blocks: Vec<Block>,
    statements: Vec<Statement>,
    exprs: Vec<Expr>,
}

impl Body {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_block(&mut self, block: Block) -> BlockId {
        let id = (self.blocks.len() + 1).try_into().unwrap();
        self.blocks.push(block);
        BlockId(NonZeroU32::new(id).unwrap())
    }

    pub fn add_stmt(&mut self, stmt: Statement) -> StmtId {
        let id = (self.statements.len() + 1).try_into().unwrap();
        self.statements.push(stmt);
        StmtId(NonZeroU32::new(id).unwrap())
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprId {
        let id = (self.exprs.len() + 1).try_into().unwrap();
        self.exprs.push(expr);
        ExprId(NonZeroU32::new(id).unwrap())
    }

    pub fn get_expr(&self, expr: ExprId) -> &Expr {
        self.exprs.get(expr.0.get() as usize - 1).unwrap()
    }

    pub fn get_expr_ty(&self, expr: ExprId) -> ExprTy {
        self.get_expr(expr).ty
    }

    pub fn set_entry(&mut self, entry: BlockId) {
        self.entry_block = Some(entry);
    }
}

#[derive(Copy, Clone, Debug)]
pub struct BlockId(NonZeroU32);

#[derive(Copy, Clone, Debug)]
pub struct ExprId(NonZeroU32);

#[derive(Copy, Clone, Debug)]
pub struct StmtId(NonZeroU32);

#[derive(Debug)]
pub struct Block {
    pub stmts: Box<[StmtId]>,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum StatementKind {
    /// A simple expression statement
    Expr(ExprId),
    /// If-else?
    If(ExprId, BlockId, Option<BlockId>),
    /// An infinite loop. The loop-specific break condition is within
    /// the block and can be identified by looking at the LoopDesugaring.
    Loop(
        /*init*/ Option<StmtId>,
        /*retry*/ Option<StmtId>,
        BlockId,
        LoopDesugaring,
    ),
    /// Switch, with the scrutinee expression, the case clauses,
    /// and the number of statements in that block to skip for every
    /// expression, or the default case
    Switch(ExprId, Box<[(ExprId, u32)]>, Option<u32>, BlockId),
    /// Return
    Return(Option<ExprId>),
    Break,
    Continue,
    Assign(ExprId, ExprId),
}

#[derive(Debug)]
pub enum LoopDesugaring {
    For {
        init: StmtId,
        cond: ExprId,
        retry: StmtId,
    },
    While {
        cond: ExprId,
    },
    Do {
        cond: ExprId,
    },
    Foreach {
        init: StmtId,
        cond: ExprId,
        next: StmtId,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum ExprTy {
    /// The return type of functions without a return type
    Void,
    Ty(Ty),
}

impl ExprTy {
    pub fn expect_ty(&self, msg: &str) -> Ty {
        match self {
            ExprTy::Void => panic!("expected type in {}", msg),
            ExprTy::Ty(ty) => *ty,
        }
    }

    pub fn ty_or<T>(&self, t: T) -> Result<Ty, T> {
        match self {
            ExprTy::Void => Err(t),
            ExprTy::Ty(ty) => Ok(*ty),
        }
    }
}

#[derive(Debug)]
pub struct Expr {
    pub ty: ExprTy,
    pub kind: ExprKind,
    pub span: Option<Span>,
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
    DelegateCreation(FuncReceiver, DefId),
    /// Function call, with function, receiver, and args
    FuncCall(DefId, FuncReceiver, Box<[Option<ExprId>]>),
    /// Delegate call, with delegate expr and args
    DelegateCall(ExprId, Box<[Option<ExprId>]>),
    /// See [`DynArrayOpKind`] for details
    DynArrayIntrinsic(ExprId, DynArrayOpKind),
    /// Operators are conventionally static, so there's no receiver
    OpCall(DefId, ExprId, Option<ExprId>),
    /// x ? y : z
    TernaryOp(ExprId, ExprId, ExprId),
    /// If false checks Ne, if true checks Eq
    StructComparison(ExprId, ExprId, bool),
    /// If false checks Ne, if true checks Eq
    DelegateComparison(ExprId, ExprId, bool),
    /// `new (a, b) c (d)`
    /// TODO: This could be a `native coerce object function`?
    NewExpr(Option<ExprId>, Option<ExprId>, ExprId, Option<ExprId>),
    /// string(myname), class<Actor>(obj), true if explicit
    CastExpr(Ty, ExprId, bool),
}

#[derive(Debug)]
pub enum DynArrayOpKind {
    FindElem(ExprId),
    FindField(DefId, ExprId),
    Add(ExprId),
    AddItem(ExprId),
    Insert(ExprId, ExprId),
    InsertItem(ExprId, ExprId),
    Remove(ExprId, ExprId),
    RemoveItem(ExprId),
    Sort(ExprId),
    RandomizeOrder,
}

#[derive(Debug)]
pub enum PlaceExprKind {
    SelfAccess,
    Local(DefId),
    Arg(DefId),
    Index(ExprId, ExprId),
    Field(ExprId, DefId),
    DynArrayLen(ExprId),
}

#[derive(Debug)]
pub enum Literal {
    None,
    Bool(bool),
    Int(i32),
    Float(f32),
    Name,
    String,
    Byte(u8),
    Object(DefId),
    Class(DefId),
}

#[derive(Debug)]
pub enum FuncReceiver {
    Cdo(DefId),
    Expr(ExprId),
}
