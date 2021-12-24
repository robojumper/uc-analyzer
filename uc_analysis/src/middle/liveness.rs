//! An AST liveness pass. UC is not nearly complicated enough to warrant
//! any complicated CFG liveness pass. Main UC-specific behaviors here:
//!
//! 1. We recursively flatten local/arg structs so that we can track
//!    individual struct fields, e.g. `MyStruct.Var1 = 1; MyStruct.Var1 = 2;`
//!    should cause a liveness lint.
//! 2. non-const `out` arguments are live upon entry to the exit node, since the caller
//!    will presumably read them.
//! 3. Array operations are a read immediately followed by a write.

use std::{collections::HashMap, num::NonZeroU32};

use bit_vec::{self, BitVec};
use uc_middle::body::{ExprId, StmtId};

/// A node in our ad-hoc expression CFG.
enum NodeKind {
    /// The exit node, with the non-const out locals it gens.
    Exit(BitVec),
    /// An expression that gens some locals.
    Expr(ExprId, BitVec),
    /// The locals that an assignment statement kills in the writeback, all at once.
    AssignEffect(StmtId, BitVec),
    /// The locals that a function or operator call kills, all at once.
    OutArgEffect(ExprId, BitVec),
    /// A node used to tie back-edges together or split if/elses
    Dummy,
}

#[derive(Clone, Copy)]
struct NodeIdx(NonZeroU32);

struct Node {
    self_idx: NodeIdx,
    kind: NodeKind,
    succ: Vec<NodeIdx>,
}

struct AdHocCfg {
    locals: Vec<Local>,
    nodes: Vec<Node>,
}

#[derive(Clone, Copy)]
struct LocalIdx(NonZeroU32);

struct Local;

struct LocalMapping {}
