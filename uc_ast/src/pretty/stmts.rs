use std::io;

use crate::{Block, Case, Expr, ExprKind, Literal, Op, Statement, StatementKind};

use super::PPrinter;

impl<W: io::Write> PPrinter<W> {
    pub fn format_statement(&mut self, stmt: &Statement) -> io::Result<()> {
        match &stmt.kind {
            StatementKind::IfStatement {
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
            StatementKind::ForStatement {
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
            StatementKind::ForeachStatement { source, run } => {
                self.w.write_all(b"foreach ")?;
                self.format_expr(source)?;
                self.format_block(run)?;
            }
            StatementKind::WhileStatement { cond, run } => {
                self.w.write_all(b"if (")?;
                self.format_expr(cond)?;
                self.w.write_all(b")")?;
                self.format_block(run)?;
                self.w.write_all(b"\n")?;
            }
            StatementKind::DoStatement { cond, run } => {
                self.w.write_all(b"do")?;
                self.indent_incr();
                self.format_block(run)?;
                self.w.write_all(b" until(")?;
                self.format_expr(cond)?;
                self.w.write_all(b");\n")?;
            }
            StatementKind::SwitchStatement { scrutinee, cases } => {
                self.w.write_all(b"switch (")?;
                self.format_expr(scrutinee)?;
                self.w.write_all(b") {\n")?;
                for case in cases {
                    self.indent()?;
                    match &case.case {
                        Case::Case(c) => {
                            self.w.write_all(b"case ")?;
                            self.format_expr(c)?;
                            self.w.write_all(b":\n")?
                        }
                        Case::Default => self.w.write_all(b"default:\n")?,
                    }
                    self.indent_incr();
                    for stmt in &case.stmts {
                        self.indent()?;
                        self.format_statement(stmt)?;
                        self.w.write_all(b"\n")?
                    }
                    self.indent_decr();
                }
                self.indent()?;
                self.w.write_all(b"}\n")?
            }
            StatementKind::BreakStatement => {
                self.w.write_all(b"break;")?;
            }
            StatementKind::ContinueStatement => {
                self.w.write_all(b"continue;")?;
            }
            StatementKind::ReturnStatement { expr } => {
                self.w.write_all(b"return")?;
                if let Some(expr) = expr {
                    self.w.write_all(b" ")?;
                    self.format_expr(expr)?;
                }
                self.w.write_all(b";")?;
            }
            StatementKind::Label { name } => {
                self.format_i(name)?;
                self.w.write_all(b":")?;
            }
            StatementKind::Assignment { lhs, rhs } => {
                self.format_expr(lhs)?;
                self.w.write_all(b" = ")?;
                self.format_expr(rhs)?;
                self.w.write_all(b";")?;
            }
            StatementKind::Expr { expr } => {
                self.format_expr(expr)?;
                self.w.write_all(b";")?;
            }
        }

        Ok(())
    }

    pub fn format_block(&mut self, b: &Block) -> io::Result<()> {
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

    pub fn format_expr(&mut self, expr: &Expr) -> io::Result<()> {
        match &expr.kind {
            ExprKind::IndexExpr { base, idx } => {
                self.w.write_all(b"(")?;
                self.format_expr(base)?;
                self.w.write_all(b"[")?;
                self.format_expr(idx)?;
                self.w.write_all(b"]")?;
                self.w.write_all(b")")?;
            }
            ExprKind::FieldExpr { lhs, rhs } => {
                self.format_expr(lhs)?;
                self.w.write_all(b".")?;
                self.format_i(rhs)?;
            }
            ExprKind::FuncCallExpr { lhs, name, args } => {
                if let Some(lhs) = lhs {
                    self.format_expr(lhs)?;
                    self.w.write_all(b".")?;
                }
                self.format_i(name)?;
                self.w.write_all(b"(")?;
                for (idx, arg) in args.iter().enumerate() {
                    if let Some(arg) = arg {
                        self.format_expr(arg)?;
                    }
                    if idx != args.len() - 1 {
                        self.w.write_all(b", ")?;
                    }
                }
                self.w.write_all(b")")?;
            }
            ExprKind::DelegateCallExpr { lhs, args } => {
                self.w.write_all(b"(")?;
                self.format_expr(lhs)?;
                self.w.write_all(b")")?;
                self.w.write_all(b"(")?;
                for (idx, arg) in args.iter().enumerate() {
                    if let Some(arg) = arg {
                        self.format_expr(arg)?;
                    }
                    if idx != args.len() - 1 {
                        self.w.write_all(b", ")?;
                    }
                }
                self.w.write_all(b")")?;
            }
            ExprKind::ClassMetaCastExpr { ty, expr } => {
                self.format_ty(ty)?;
                self.w.write_all(b"(")?;
                self.format_expr(expr)?;
                self.w.write_all(b")")?;
            }
            ExprKind::NewExpr { args, cls, arch } => {
                self.w.write_all(b"(")?;
                self.w.write_all(b"new ")?;
                if !args.is_empty() {
                    self.w.write_all(b"(")?;
                    for (idx, arg) in args.iter().enumerate() {
                        if let Some(arg) = arg {
                            self.format_expr(arg)?;
                        } else {
                            self.w.write_all(b",")?;
                        }
                        if idx != args.len() - 1 {
                            self.w.write_all(b", ")?;
                        }
                    }
                    self.w.write_all(b") ")?;
                }
                self.format_expr(cls)?;

                if let Some(arch) = arch {
                    self.w.write_all(b"(")?;
                    self.format_expr(arch)?;
                    self.w.write_all(b")")?;
                }
                self.w.write_all(b")")?;
            }
            ExprKind::PreOpExpr { op, rhs } => {
                self.w.write_all(b"(")?;
                self.format_op(op)?;
                self.format_expr(rhs)?;
                self.w.write_all(b")")?;
            }
            ExprKind::PostOpExpr { lhs, op } => {
                self.w.write_all(b"(")?;
                self.format_expr(lhs)?;
                self.w.write_all(b" ")?;
                self.format_op(op)?;
                self.w.write_all(b")")?;
            }
            ExprKind::BinOpExpr { lhs, op, rhs } => {
                self.w.write_all(b"(")?;
                self.format_expr(lhs)?;
                self.w.write_all(b" ")?;
                self.format_op(op)?;
                self.w.write_all(b" ")?;
                self.format_expr(rhs)?;
                self.w.write_all(b")")?;
            }
            ExprKind::TernExpr { cond, then, alt } => {
                self.w.write_all(b"(")?;
                self.format_expr(cond)?;
                self.w.write_all(b" ? ")?;
                self.format_expr(then)?;
                self.w.write_all(b" : ")?;
                self.format_expr(alt)?;
                self.w.write_all(b")")?;
            }
            ExprKind::SymExpr { sym } => {
                self.format_i(sym)?;
            }
            ExprKind::LiteralExpr { lit } => {
                self.format_lit(lit)?;
            }
        }
        Ok(())
    }

    fn format_lit(&mut self, l: &Literal) -> io::Result<()> {
        match l {
            Literal::None => self.w.write_all(b"None")?,
            Literal::ObjReference(a, b) => {
                self.w.write_all(a.as_ref().as_bytes())?;
                self.w.write_all(b"'")?;
                self.w.write_all(b.as_ref().as_bytes())?;
                self.w.write_all(b"'")?;
            }
            Literal::Float(f) => self.w.write_all(format!("{}", f).as_bytes())?,
            Literal::Int(i) => self.w.write_all(format!("{}", i).as_bytes())?,
            Literal::Bool(b) => self.w.write_all(if *b { b"true" } else { b"false" })?,
            Literal::Name(n) => {
                self.w.write_all(b"'")?;
                self.w.write_all(n.as_ref().as_bytes())?;
                self.w.write_all(b"'")?;
            }
            Literal::String(s) => self.w.write_all(s.as_bytes())?,
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
            Op::YawClockwiseFrom => "clockwisefrom",
        };
        self.w.write_all(sl.as_bytes())?;
        Ok(())
    }
}
