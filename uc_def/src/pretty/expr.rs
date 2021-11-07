use std::io;

use crate::{BaseExpr, Literal, Op};

use super::{PPrinter, RefLookup};

impl<W: io::Write, R: RefLookup> PPrinter<W, R> {
    pub fn format_expr(&mut self, expr: &BaseExpr<R::From>) -> io::Result<()> {
        match expr {
            BaseExpr::IndexExpr { base, idx } => {
                self.w.write_all(b"(")?;
                self.format_expr(base)?;
                self.w.write_all(b"[")?;
                self.format_expr(idx)?;
                self.w.write_all(b"]")?;
                self.w.write_all(b")")?;
            }
            BaseExpr::FieldExpr { lhs, rhs } => {
                self.format_expr(lhs)?;
                self.w.write_all(b".")?;
                self.format_expr(rhs)?;
            }
            BaseExpr::CallExpr { lhs, args } => {
                self.format_expr(lhs)?;
                self.w.write_all(b"(")?;
                for (idx, arg) in args.iter().enumerate() {
                    self.format_expr(arg)?;
                    if idx != args.len() - 1 {
                        self.w.write_all(b", ")?;
                    }
                }
                self.w.write_all(b")")?;
            }
            BaseExpr::NewExpr { args, cls, arch } => {
                self.w.write_all(b"(")?;
                self.w.write_all(b"new ")?;
                if !args.is_empty() {
                    self.w.write_all(b"(")?;
                    for (idx, arg) in args.iter().enumerate() {
                        self.format_expr(arg)?;
                        if idx != args.len() - 1 {
                            self.w.write_all(b", ")?;
                        }
                    }
                    self.w.write_all(b")")?;
                }
                self.format_expr(cls)?;

                if let Some(arch) = arch {
                    self.w.write_all(b"(")?;
                    self.format_expr(arch)?;
                    self.w.write_all(b")")?;
                }
                self.w.write_all(b")")?;
            }
            BaseExpr::PreOpExpr { op, rhs } => {
                self.w.write_all(b"(")?;
                self.format_op(op)?;
                self.w.write_all(b" ")?;
                self.format_expr(rhs)?;
                self.w.write_all(b")")?;
            }
            BaseExpr::PostOpExpr { lhs, op } => {
                self.w.write_all(b"(")?;
                self.format_expr(lhs)?;
                self.w.write_all(b" ")?;
                self.format_op(op)?;
                self.w.write_all(b")")?;
            }
            BaseExpr::BinOpExpr { lhs, op, rhs } => {
                self.w.write_all(b"(")?;
                self.format_expr(lhs)?;
                self.w.write_all(b" ")?;
                self.format_op(op)?;
                self.w.write_all(b" ")?;
                self.format_expr(rhs)?;
                self.w.write_all(b")")?;
            }
            BaseExpr::TernExpr { cond, then, alt } => {
                self.w.write_all(b"(")?;
                self.format_expr(cond)?;
                self.w.write_all(b" ? ")?;
                self.format_expr(then)?;
                self.w.write_all(b" : ")?;
                self.format_expr(alt)?;
                self.w.write_all(b")")?;
            }
            BaseExpr::SymExpr { sym } => {
                self.format_i(sym)?;
            }
            BaseExpr::LiteralExpr { lit } => {
                self.format_lit(lit)?;
            }
        }
        Ok(())
    }

    fn format_lit(&mut self, l: &Literal) -> io::Result<()> {
        match l {
            Literal::None => self.w.write_all(b"None")?,
            Literal::ObjReference => self.w.write_all(b"<ObjectReference>")?,
            Literal::Number => self.w.write_all(b"<Number>")?,
            Literal::Bool => self.w.write_all(b"<Bool>")?,
            Literal::Name => self.w.write_all(b"<Name>")?,
            Literal::String => self.w.write_all(b"<String>")?,
        }
        Ok(())
    }

    pub fn format_op(&mut self, op: &Op) -> io::Result<()> {
        let sl = match op {
            Op::Add => "+",
            Op::AddAdd => "++",
            Op::AddAssign => "+=",
            Op::And => "&",
            Op::AndAnd => "&&",
            Op::At => "@",
            Op::AtAssign => "@=",
            Op::Bang => "!",
            Op::BangEq => "!=",
            Op::Div => "/",
            Op::DivAssign => "/=",
            Op::Dollar => "$",
            Op::DollarAssign => "$=",
            Op::EqEq => "==",
            Op::Gt => ">",
            Op::GtEq => ">=",
            Op::GtGt => ">>",
            Op::GtGtGt => ">>>",
            Op::Lt => "<",
            Op::LtEq => "<=",
            Op::LtLt => "<<",
            Op::Mod => "%",
            Op::Mul => "*",
            Op::MulMul => "**",
            Op::MulAssign => "*=",
            Op::Or => "|",
            Op::OrOr => "||",
            Op::Pow => "^",
            Op::PowPow => "^^",
            Op::Sub => "-",
            Op::SubAssign => "-=",
            Op::SubSub => "--",
            Op::Tilde => "~",
            Op::TildeEq => "~=",
            Op::VecCross => "cross",
            Op::VecDot => "dot",
        };
        self.w.write_all(sl.as_bytes())?;
        Ok(())
    }
}
