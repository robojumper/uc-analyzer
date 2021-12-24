//! Various helpers for constant interpretation.

use uc_def::Op;

use crate::{
    body::{ExprKind, Literal, ValueExprKind},
    Defs,
};

use super::{Body, ExprId};

pub fn try_interp_integer_expr(defs: &Defs, body: &Body, expr: ExprId) -> Option<i32> {
    let expr = body.get_expr(expr);
    let ty = expr.ty.expect_ty("interpret integer expr");
    assert!(ty.is_byte() || ty.is_int());
    match &expr.kind {
        ExprKind::Value(ValueExprKind::Lit(l)) => match l {
            Literal::Int(i) => Some(*i),
            Literal::Byte(b) => Some(*b as i32),
            _ => panic!("how is this of type int or byte?"),
        },
        &ExprKind::Value(ValueExprKind::OpCall(op, e, rhs)) => {
            let op = defs.get_op(op);
            // Check for a unary negation operator
            if rhs.is_none() && op.op == Op::Sub {
                try_interp_integer_expr(defs, body, e).map(|x| x.wrapping_neg())
            } else {
                None
            }
        }
        _ => None,
    }
}
