use std::io;

use crate::{Block, Expr, Literal, Op, Statement};

use super::{PPrinter, RefLookup};

impl<W: io::Write, R: RefLookup> PPrinter<W, R> {
    pub fn format_statement(&mut self, stmt: &Statement<R::From>) -> io::Result<()> {
        match stmt {
            Statement::IfStatement {
                cond,
                then,
                or_else,
            } => {
                self.w.write_all(b"if (")?;
                self.format_expr(cond)?;
                self.w.write_all(b")")?;
                self.format_block(then)?;
                if let Some(or_else) = or_else {
                    self.w.write_all(b" else ")?;
                    self.format_block(or_else)?;
                } else {
                    self.w.write_all(b"\n")?;
                }
            }
            Statement::ForStatement {
                init,
                cond,
                retry,
                run,
            } => {
                self.w.write_all(b"for (")?;
                self.format_statement(init)?;
                self.w.write_all(b" ")?;
                self.format_expr(cond)?;
                self.w.write_all(b"; ")?;
                self.format_statement(retry)?;
                self.w.write_all(b")")?;
                self.format_block(run)?;
            }
            Statement::ForeachStatement { source, run } => todo!(),
            Statement::WhileStatement { cond, run } => {
                self.w.write_all(b"if (")?;
                self.format_expr(cond)?;
                self.w.write_all(b")")?;
                self.format_block(run)?;
                self.w.write_all(b"\n")?;
            }
            Statement::DoStatement { cond, run } => todo!(),
            Statement::SwitchStatement { scrutinee, cases } => todo!(),
            Statement::BreakStatement => todo!(),
            Statement::ContinueStatement => todo!(),
            //Statement::GotoStatement => todo!(),
            Statement::ReturnStatement { expr } => {
                self.w.write_all(b"return")?;
                if let Some(expr) = expr {
                    self.w.write_all(b" ")?;
                    self.format_expr(expr)?;
                }
                self.w.write_all(b";")?;
            }
            Statement::Label(_) => todo!(),
            Statement::Assignment { lhs, rhs } => {
                self.format_expr(lhs)?;
                self.w.write_all(b" = ")?;
                self.format_expr(rhs)?;
                self.w.write_all(b";")?;
            }
            Statement::Expr { expr } => {
                self.format_expr(expr)?;
                self.w.write_all(b";")?;
            }
        }

        Ok(())
    }

    pub fn format_block(&mut self, b: &Block<R::From>) -> io::Result<()> {
        self.w.write_all(b" {\n")?;
        self.indent_incr();
        for stmt in &b.stmts {
            self.indent()?;
            self.format_statement(stmt)?;
            self.w.write_all(b"\n")?;
        }
        self.indent_decr();
        self.indent()?;
        self.w.write_all(b"}")?;
        Ok(())
    }

    pub fn format_expr(&mut self, expr: &Expr<R::From>) -> io::Result<()> {
        match expr {
            Expr::IndexExpr { base, idx } => {
                self.w.write_all(b"(")?;
                self.format_expr(base)?;
                self.w.write_all(b"[")?;
                self.format_expr(idx)?;
                self.w.write_all(b"]")?;
                self.w.write_all(b")")?;
            }
            Expr::FieldExpr { lhs, rhs } => {
                self.format_expr(lhs)?;
                self.w.write_all(b".")?;
                self.format_expr(rhs)?;
            }
            Expr::CallExpr { lhs, args } => {
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
            Expr::NewExpr { args, cls, arch } => {
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
            Expr::PreOpExpr { op, rhs } => {
                self.w.write_all(b"(")?;
                self.format_op(op)?;
                self.w.write_all(b" ")?;
                self.format_expr(rhs)?;
                self.w.write_all(b")")?;
            }
            Expr::PostOpExpr { lhs, op } => {
                self.w.write_all(b"(")?;
                self.format_expr(lhs)?;
                self.w.write_all(b" ")?;
                self.format_op(op)?;
                self.w.write_all(b")")?;
            }
            Expr::BinOpExpr { lhs, op, rhs } => {
                self.w.write_all(b"(")?;
                self.format_expr(lhs)?;
                self.w.write_all(b" ")?;
                self.format_op(op)?;
                self.w.write_all(b" ")?;
                self.format_expr(rhs)?;
                self.w.write_all(b")")?;
            }
            Expr::TernExpr { cond, then, alt } => {
                self.w.write_all(b"(")?;
                self.format_expr(cond)?;
                self.w.write_all(b" ? ")?;
                self.format_expr(then)?;
                self.w.write_all(b" : ")?;
                self.format_expr(alt)?;
                self.w.write_all(b")")?;
            }
            Expr::SymExpr { sym } => {
                self.format_i(sym)?;
            }
            Expr::LiteralExpr { lit } => {
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
