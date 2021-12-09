//! UnrealScript statement and expression parser based on Pratt parsing.
//! See <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
//! for an introduction to Pratt parsing.

use std::str::FromStr;

use uc_ast::{Context, Expr, ExprKind, Ty};
use uc_def::Op;
use uc_name::Identifier;

use crate::{
    kw,
    lexer::{NumberSyntax, Sigil, Token, TokenKind as Tk},
    sig,
};

mod test;

use super::{ParseError, Parser};

/// Slightly annoying: Operators on the parser level don't neatly correspond
/// to sigils but also not to operators on the HIR. We need to give the dot and
/// the ternary op a binding power, but also have the vector operators
/// cross and dot. This might not be the best way to handle this.
#[derive(Copy, Clone, Debug)]
enum OpLike {
    Op(Op),
    Tern,
    Dot,
    LBrack,
}

impl OpLike {
    fn from_token(token: &Token) -> Option<Self> {
        match token.kind {
            sig!(Tern) => Some(OpLike::Tern),
            sig!(Dot) => Some(OpLike::Dot),
            sig!(LBrack) => Some(OpLike::LBrack),
            kw!(ClockwiseFrom) => Some(OpLike::Op(Op::YawClockwiseFrom)),
            kw!(Cross) => Some(OpLike::Op(Op::VecCross)),
            kw!(Dot) => Some(OpLike::Op(Op::VecDot)),
            Tk::Sig(s) => s.to_op().map(OpLike::Op),
            _ => None,
        }
    }

    fn unwrap_op(&self) -> Op {
        match self {
            OpLike::Op(o) => *o,
            _ => panic!("not an operator: {:?}", self),
        }
    }
}

impl Parser<'_> {
    pub fn parse_base_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_base_expression_bp(0)
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Option<Expr>>, ParseError> {
        let mut args = vec![];
        loop {
            while self.eat(Tk::Comma) {
                args.push(None);
            }
            if self.eat(sig!(RParen)) {
                break;
            }
            let expr = self.parse_base_expression_bp(0)?;
            args.push(Some(expr));
            let delim = self.next_any()?;
            match delim.kind {
                Tk::Comma => continue,
                sig!(RParen) => break,
                _ => return Err(self.fmt_err("Expected comma or )", Some(delim))),
            }
        }
        Ok(args)
    }

    fn continue_context(
        &mut self,
        i: Identifier,
        lhs: Option<Expr>,
    ) -> Result<(Context, Identifier), ParseError> {
        Ok(if &i == "static" {
            self.expect(sig!(Dot))?;
            let nm = self.expect_ident()?;
            (Context::Static(lhs), nm)
        } else if &i == "const" {
            self.expect(sig!(Dot))?;
            let nm = self.expect_ident()?;
            (Context::Const(lhs), nm)
        } else if &i == "default" {
            self.expect(sig!(Dot))?;
            let nm = self.expect_ident()?;
            (Context::Default(lhs), nm)
        } else if &i == "global" {
            if lhs.is_some() {
                todo!("error")
            }
            self.expect(sig!(Dot))?;
            let nm = self.expect_ident()?;
            (Context::Global, nm)
        } else if &i == "super" {
            if lhs.is_some() {
                todo!("error")
            }
            let s_class = if self.eat(sig!(LParen)) {
                let s_class = self.expect_ident()?;
                self.expect(sig!(RParen))?;
                Some(s_class)
            } else {
                None
            };
            self.expect(sig!(Dot))?;
            let nm = self.expect_ident()?;
            (Context::Super(s_class), nm)
        } else {
            match lhs {
                Some(e) => (Context::Expr(e), i),
                None => (Context::Bare, i),
            }
        })
    }

    fn parse_base_expression_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let lhs_marker = self.marker();
        let mut lhs = {
            let tok = self.next_any()?;
            match tok.kind {
                sig!(LParen) => {
                    let mut lhs = self.parse_base_expression_bp(0)?;
                    lhs.paren = true;
                    self.expect(sig!(RParen))?;
                    lhs
                }
                Tk::Sig(sig) => {
                    match OpLike::from_token(&tok).and_then(prefix_binding_power) {
                        Some(((), r_bp)) => {
                            let rhs = self.parse_base_expression_bp(r_bp)?;
                            Expr {
                                span: lhs_marker.complete(self),
                                paren: false,
                                kind: ExprKind::PreOpExpr {
                                    op: sig.to_op().unwrap(),
                                    rhs: Box::new(rhs),
                                },
                            }
                        }
                        None => {
                            // Hack: Simply eat a preoperator + if followed by a number directly. UCC just considers it
                            // part of the number by feeding info back from the parser to the lexer.
                            if sig == Sigil::Add
                                && matches!(self.peek(), Some(Token { kind: Tk::Number(_), .. }))
                            {
                                let tok = self.next().unwrap();
                                let syn = match tok.kind {
                                    Tk::Number(f) => f,
                                    _ => unreachable!(),
                                };
                                Expr {
                                    span: lhs_marker.complete(self),
                                    paren: false,
                                    kind: ExprKind::LiteralExpr {
                                        lit: match syn {
                                            NumberSyntax::Int(i) | NumberSyntax::Hex(i) => {
                                                uc_ast::Literal::Int(i)
                                            }
                                            NumberSyntax::Float(f) => uc_ast::Literal::Float(f),
                                        },
                                    },
                                }
                            } else {
                                return Err(self.fmt_err("Not a preoperator", Some(tok)));
                            }
                        }
                    }
                }
                kw!(New) => {
                    let args = if self.eat(sig!(LParen)) { self.parse_arg_list()? } else { vec![] };

                    let ((), r_bp) = NEW_PREFIX_POWER;

                    let cls = self.parse_base_expression_bp(r_bp)?;
                    let arch = if self.eat(sig!(LParen)) {
                        let arch = self.parse_base_expression_bp(0)?;
                        self.expect(sig!(RParen))?;
                        Some(arch)
                    } else {
                        None
                    };
                    Expr {
                        span: lhs_marker.complete(self),
                        paren: false,
                        kind: ExprKind::NewExpr {
                            args,
                            cls: Box::new(cls),
                            arch: arch.map(Box::new),
                        },
                    }
                }
                kw!(Class)
                    if matches!(self.peek(), Some(Token { kind: sig!(Lt) | sig!(LParen), .. })) =>
                {
                    let name = if self.eat(sig!(Lt)) {
                        let ident = self.expect_ident()?;
                        self.expect(sig!(Gt))?;
                        ident
                    } else {
                        Identifier::from_str("Object").unwrap()
                    };

                    self.expect(sig!(LParen))?;
                    let expr = self.parse_base_expression_bp(0)?;
                    self.expect(sig!(RParen))?;
                    Expr {
                        span: lhs_marker.complete(self),
                        paren: false,
                        kind: ExprKind::ClassMetaCastExpr {
                            ty: Ty::Class(name),
                            expr: Box::new(expr),
                        },
                    }
                }
                Tk::Sym(_)
                    if matches!(self.peek(), Some(Token { kind: Tk::Name | Tk::DotName, .. })) =>
                {
                    let nm_tok = self.next().unwrap();
                    let a = self.sym_to_ident(&tok);
                    let b = self.lex.extract_name(&nm_tok);
                    Expr {
                        span: lhs_marker.complete(self),
                        paren: false,
                        kind: ExprKind::LiteralExpr { lit: uc_ast::Literal::ObjReference(a, b) },
                    }
                }
                kw!(None) => Expr {
                    span: lhs_marker.complete(self),
                    paren: false,
                    kind: ExprKind::LiteralExpr { lit: uc_ast::Literal::None },
                },
                Tk::Sym(_) => {
                    let ident = self.sym_to_ident(&tok);
                    let (ctx, name) = self.continue_context(ident, None)?;

                    if self.eat(sig!(LParen)) {
                        let args = self.parse_arg_list()?;
                        Expr {
                            span: lhs_marker.complete(self),
                            paren: false,
                            kind: ExprKind::FuncCallExpr { lhs: Box::new(ctx), name, args },
                        }
                    } else {
                        let span = lhs_marker.complete(self);
                        Expr {
                            span,
                            paren: false,
                            kind: ExprKind::FieldExpr { lhs: Box::new(ctx), rhs: name },
                        }
                    }
                }
                Tk::Number(syn) => Expr {
                    span: lhs_marker.complete(self),
                    paren: false,
                    kind: ExprKind::LiteralExpr {
                        lit: match syn {
                            NumberSyntax::Int(i) | NumberSyntax::Hex(i) => uc_ast::Literal::Int(i),
                            NumberSyntax::Float(f) => uc_ast::Literal::Float(f),
                        },
                    },
                },
                Tk::String => Expr {
                    span: lhs_marker.complete(self),
                    paren: false,
                    kind: ExprKind::LiteralExpr {
                        lit: uc_ast::Literal::String(self.lex.extract_string(&tok)),
                    },
                },
                Tk::Name => Expr {
                    span: lhs_marker.complete(self),
                    paren: false,
                    kind: ExprKind::LiteralExpr {
                        lit: uc_ast::Literal::Name(self.lex.extract_name(&tok)),
                    },
                },
                Tk::Bool(b) => Expr {
                    span: lhs_marker.complete(self),
                    paren: false,
                    kind: ExprKind::LiteralExpr { lit: uc_ast::Literal::Bool(b) },
                },
                _ => return Err(self.fmt_err("Unknown start of expression", Some(tok))),
            }
        };

        #[allow(clippy::while_let_loop)]
        loop {
            let op = if let Some(op) = self.peek().and_then(|t| OpLike::from_token(&t)) {
                op
            } else {
                // break conditions: EOF, non-op token, or token that's not prefix or postfix op (break below)
                break;
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.next();

                lhs = if let OpLike::LBrack = op {
                    let rhs = self.parse_base_expression_bp(0)?;
                    self.expect(sig!(RBrack))?;
                    Expr {
                        span: lhs_marker.complete(self),
                        paren: false,
                        kind: ExprKind::IndexExpr { base: Box::new(lhs), idx: Box::new(rhs) },
                    }
                } else {
                    Expr {
                        span: lhs_marker.complete(self),
                        paren: false,
                        kind: ExprKind::PostOpExpr { lhs: Box::new(lhs), op: op.unwrap_op() },
                    }
                };
            } else if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.next();

                lhs = if let OpLike::Tern = op {
                    let mhs = self.parse_base_expression_bp(0)?;
                    self.expect(sig!(Colon))?;
                    let rhs = self.parse_base_expression_bp(r_bp)?;
                    Expr {
                        span: lhs_marker.complete(self),
                        paren: false,
                        kind: ExprKind::TernExpr {
                            cond: Box::new(lhs),
                            then: Box::new(mhs),
                            alt: Box::new(rhs),
                        },
                    }
                } else if let OpLike::Dot = op {
                    let rhs_tok = self.next_any()?;
                    let bridge = if let Tk::Sym(_) = rhs_tok.kind {
                        self.sym_to_ident(&rhs_tok)
                    } else {
                        return Err(
                            self.fmt_err("Unexpected token, expected identifier", Some(rhs_tok))
                        );
                    };

                    let (ctx, name) = self.continue_context(bridge, Some(lhs))?;
                    if self.eat(sig!(LParen)) {
                        let args = self.parse_arg_list()?;
                        Expr {
                            span: lhs_marker.complete(self),
                            paren: false,
                            kind: ExprKind::FuncCallExpr { lhs: Box::new(ctx), name, args },
                        }
                    } else {
                        Expr {
                            span: lhs_marker.complete(self),
                            paren: false,
                            kind: ExprKind::FieldExpr { lhs: Box::new(ctx), rhs: name },
                        }
                    }
                } else {
                    let rhs = self.parse_base_expression_bp(r_bp)?;
                    Expr {
                        span: lhs_marker.complete(self),
                        paren: false,
                        kind: ExprKind::BinOpExpr {
                            lhs: Box::new(lhs),
                            op: op.unwrap_op(),
                            rhs: Box::new(rhs),
                        },
                    }
                };
            } else {
                break;
            }
        }

        Ok(lhs)
    }
}

// Todo: is this correct?
fn prefix_binding_power(op: OpLike) -> Option<((), u8)> {
    let op = match op {
        OpLike::Op(op) => op,
        _ => return None,
    };
    let res = match op {
        Op::Bang => ((), 39),
        Op::AddAdd => ((), 39),
        Op::SubSub => ((), 39),
        Op::Tilde => ((), 39),
        Op::Sub => ((), 39),
        _ => return None,
    };
    Some(res)
}

// Todo: is this correct?
fn postfix_binding_power(op: OpLike) -> Option<(u8, ())> {
    let op = match op {
        OpLike::LBrack => return Some((40, ())),
        OpLike::Op(op) => op,
        _ => return None,
    };
    let res = match op {
        Op::AddAdd => (38, ()),
        Op::SubSub => (38, ()),
        _ => return None,
    };
    Some(res)
}

/// `new` needs to have a lower binding power than `.` and `(`
/// This is huge sadness because it means that `new class'SomeClass' (Archetype);`
/// resolves as a function call, but it has to because
/// `new self.SomeFunction(WithAnArg)` is in fact a function call and
/// `new self.SomeFunction(WithAnArg) (Archetype)` is also valid.
/// The AST consumer will have to resolve `self.SomeFunction` and see
/// if it's a valid function call and pull things apart if not.
const NEW_PREFIX_POWER: ((), u8) = ((), 38);

// The UC number in the () is the opposite of the binding power, i.e. lower number
// binds stronger. The lowest specified binding power is 12, the highest is 34.
// The numbers for UC operators were obtained by calculating (34+12)-n
fn infix_binding_power(op: OpLike) -> Option<(u8, u8)> {
    let op = match op {
        OpLike::Dot => return Some((42, 43)),
        OpLike::Tern => return Some((13, 12)),
        OpLike::Op(op) => op,
        _ => return None,
    };
    let res = match op {
        Op::MulMul => (34, 35),

        Op::Mul => (30, 31),
        Op::Div => (30, 31),

        Op::Mod => (28, 29),

        Op::Add => (26, 27),
        Op::Sub => (26, 27),

        Op::LtLt => (24, 25),
        Op::GtGt => (24, 25),
        Op::GtGtGt => (24, 25),

        Op::Lt => (22, 23),
        Op::Gt => (22, 23),
        Op::LtEq => (22, 23),
        Op::GtEq => (22, 23),
        Op::EqEq => (22, 23),
        Op::TildeEq => (22, 23),

        Op::YawClockwiseFrom => (22, 23),

        Op::BangEq => (20, 21),

        Op::And => (18, 19),
        Op::Pow => (18, 19),
        Op::Or => (18, 19),

        Op::AndAnd => (16, 17),
        Op::PowPow => (16, 17),

        Op::VecCross => (16, 17),
        Op::VecDot => (16, 17),

        Op::OrOr => (14, 15),

        // Pushed these down from 12, 13 to accomodate ternary
        Op::MulAssign => (10, 11),
        Op::DivAssign => (10, 11),
        Op::AddAssign => (10, 11),
        Op::SubAssign => (10, 11),

        Op::Dollar => (8, 9),
        Op::At => (8, 9),

        Op::DollarAssign => (6, 7),
        Op::AtAssign => (6, 7),
        // Technically there's a `-=` for strings with an even lower binding power
        // here. We just pretend it doesn't exist. Fix your own code.
        _ => return None,
    };
    Some(res)
}
