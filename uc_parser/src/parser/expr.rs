//! UnrealScript statement and expression parser based on Pratt parsing.
//! See https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
//! for an introduction to Pratt parsing.

use uc_def::{BaseExpr, Identifier, Op};

use crate::{
    kw,
    lexer::{Keyword, Sigil, Symbol, Token, TokenKind as Tk},
    sig,
};

mod test;

use super::Parser;

/// Slightly annoying: Operators on the parser level don't neatly correspond
/// to sigils but also not to operators on the HIR. We need to give parens and
/// the ternary op a binding power, but also have the vector operators
/// cross and dot. This might not be the best way to handle this.
/// TODO: Investigate eagerly converting Sig to Op and create a FakeOp variant?
#[derive(Copy, Clone, Debug)]
enum SigilOrVecOp {
    Sig(Sigil),
    VecOp(Keyword),
}

impl SigilOrVecOp {
    fn to_op(self) -> Op {
        match self {
            SigilOrVecOp::Sig(s) => s.to_op(),
            SigilOrVecOp::VecOp(Keyword::Cross) => Op::VecCross,
            SigilOrVecOp::VecOp(Keyword::Dot) => Op::VecDot,
            _ => unreachable!("only constructed with cross or dot"),
        }
    }
}

impl Parser<'_> {
    pub fn parse_base_expression(&mut self) -> Result<BaseExpr<Identifier>, String> {
        self.parse_base_expression_bp(0)
    }

    fn parse_base_expression_bp(&mut self, min_bp: u8) -> Result<BaseExpr<Identifier>, String> {
        let mut lhs = {
            let tok = self.next_any()?;
            match tok.kind {
                sig!(LParen) => {
                    let lhs = self.parse_base_expression_bp(0)?;
                    self.expect(sig!(RParen))?;
                    lhs
                }
                Tk::Sig(sig) => match prefix_binding_power(SigilOrVecOp::Sig(sig)) {
                    Some(((), r_bp)) => {
                        let rhs = self.parse_base_expression_bp(r_bp)?;
                        BaseExpr::PreOpExpr {
                            op: sig.to_op(),
                            rhs: Box::new(rhs),
                        }
                    }
                    None => return Err(format!("Not a preoperator: {:?}", tok)),
                },
                kw!(New) => {
                    let mut args = vec![];
                    if self.eat(sig!(LParen)) {
                        loop {
                            let expr = self.parse_base_expression_bp(0)?;
                            args.push(expr);
                            let delim = self.next_any()?;
                            match delim.kind {
                                Tk::Comma => continue,
                                sig!(RParen) => break,
                                _ => return Err(format!("Expected comma or }}, got {:?}", delim)),
                            }
                        }
                    }

                    let ((), r_bp) = NEW_PREFIX_POWER;

                    let cls = self.parse_base_expression_bp(r_bp)?;
                    let arch = if self.eat(sig!(LParen)) {
                        let arch = self.parse_base_expression_bp(0)?;
                        self.expect(sig!(RParen))?;
                        Some(arch)
                    } else {
                        None
                    };

                    BaseExpr::NewExpr {
                        args,
                        cls: Box::new(cls),
                        arch: arch.map(Box::new),
                    }
                }
                Tk::Sym(_) if matches!(self.peek(), Some(Token { kind: Tk::Name, .. })) => {
                    self.next();
                    BaseExpr::LiteralExpr {
                        lit: uc_def::Literal::ObjReference,
                    }
                }
                kw!(None) => BaseExpr::LiteralExpr {
                    lit: uc_def::Literal::None,
                },
                Tk::Sym(_) => BaseExpr::SymExpr {
                    sym: self.sym_to_ident(&tok),
                },
                Tk::Number(_) => BaseExpr::LiteralExpr {
                    lit: uc_def::Literal::Number,
                },
                Tk::String => BaseExpr::LiteralExpr {
                    lit: uc_def::Literal::String,
                },
                Tk::Name => BaseExpr::LiteralExpr {
                    lit: uc_def::Literal::Name,
                },
                Tk::Bool(_) => BaseExpr::LiteralExpr {
                    lit: uc_def::Literal::Bool,
                },
                _ => return Err(format!("Unknown start of expression: {:?}", tok)),
            }
        };

        loop {
            let op = match self.peek() {
                Some(Token {
                    kind: Tk::Sig(s), ..
                }) if is_infix_or_postfix_op(s) => SigilOrVecOp::Sig(s),
                Some(Token {
                    kind: Tk::Sym(Symbol::Kw(k @ (Keyword::Cross | Keyword::Dot))),
                    ..
                }) => SigilOrVecOp::VecOp(k),
                // EOF is fine, anything else is fine too. At least for now
                // TODO: List tokens that could appear after exprs explicitly?
                // `;` statement delimiter
                // `,` arg delimiter (in case of optional arg expr)
                // EOF file end
                _ => break,
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.next();

                lhs = if let SigilOrVecOp::Sig(Sigil::LBrack) = op {
                    let rhs = self.parse_base_expression_bp(0)?;
                    self.expect(sig!(RBrack))?;
                    BaseExpr::IndexExpr {
                        base: Box::new(lhs),
                        idx: Box::new(rhs),
                    }
                } else if let SigilOrVecOp::Sig(Sigil::LParen) = op {
                    let mut args = vec![];
                    loop {
                        if self.eat(sig!(RParen)) {
                            break;
                        }
                        let expr = self.parse_base_expression_bp(0)?;
                        args.push(expr);
                        let delim = self.next_any()?;
                        match delim.kind {
                            Tk::Comma => continue,
                            sig!(RParen) => break,
                            _ => return Err(format!("Expected comma or }}, got {:?}", delim)),
                        }
                    }
                    BaseExpr::CallExpr {
                        lhs: Box::new(lhs),
                        args,
                    }
                } else {
                    BaseExpr::PostOpExpr {
                        lhs: Box::new(lhs),
                        op: op.to_op(),
                    }
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.next();

                lhs = if let SigilOrVecOp::Sig(Sigil::Tern) = op {
                    let mhs = self.parse_base_expression_bp(0)?;
                    self.expect(sig!(Colon))?;
                    let rhs = self.parse_base_expression_bp(r_bp)?;
                    BaseExpr::TernExpr {
                        cond: Box::new(lhs),
                        then: Box::new(mhs),
                        alt: Box::new(rhs),
                    }
                } else {
                    let rhs = self.parse_base_expression_bp(r_bp)?;
                    if let SigilOrVecOp::Sig(Sigil::Dot) = op {
                        BaseExpr::FieldExpr {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }
                    } else {
                        BaseExpr::BinOpExpr {
                            lhs: Box::new(lhs),
                            op: op.to_op(),
                            rhs: Box::new(rhs),
                        }
                    }
                };
                continue;
            }
        }

        Ok(lhs)
    }
}

fn is_infix_or_postfix_op(op: Sigil) -> bool {
    postfix_binding_power(SigilOrVecOp::Sig(op)).is_some()
        || infix_binding_power(SigilOrVecOp::Sig(op)).is_some()
}

// Todo: is this correct?
fn prefix_binding_power(op: SigilOrVecOp) -> Option<((), u8)> {
    let op = match op {
        SigilOrVecOp::Sig(s) => s,
        SigilOrVecOp::VecOp(_) => return None,
    };
    let res = match op {
        Sigil::Bang => ((), 11),
        Sigil::AddAdd => ((), 11),
        Sigil::SubSub => ((), 11),
        Sigil::Tilde => ((), 11),
        Sigil::Sub => ((), 11),
        _ => return None,
    };
    Some(res)
}

// Todo: is this correct?
fn postfix_binding_power(op: SigilOrVecOp) -> Option<(u8, ())> {
    let op = match op {
        SigilOrVecOp::Sig(s) => s,
        SigilOrVecOp::VecOp(_) => return None,
    };
    let res = match op {
        Sigil::AddAdd => (10, ()),
        Sigil::SubSub => (10, ()),
        Sigil::LBrack => (40, ()),
        Sigil::LParen => (40, ()),
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
fn infix_binding_power(op: SigilOrVecOp) -> Option<(u8, u8)> {
    let op = match op {
        SigilOrVecOp::Sig(s) => s,
        SigilOrVecOp::VecOp(_) => return Some((16, 17)),
    };
    let res = match op {
        Sigil::Dot => (42, 43),

        Sigil::MulMul => (34, 35),

        Sigil::Mul => (30, 31),
        Sigil::Div => (30, 31),

        Sigil::Mod => (28, 29),

        Sigil::Add => (26, 27),
        Sigil::Sub => (26, 27),

        Sigil::LtLt => (24, 25),
        Sigil::GtGt => (24, 25),
        Sigil::GtGtGt => (24, 25),

        Sigil::Lt => (22, 23),
        Sigil::Gt => (22, 23),
        Sigil::LtEq => (22, 23),
        Sigil::GtEq => (22, 23),
        Sigil::EqEq => (22, 23),

        Sigil::BangEq => (20, 21),

        Sigil::And => (18, 19),
        Sigil::Pow => (18, 19),
        Sigil::Or => (18, 19),

        Sigil::AndAnd => (16, 17),
        Sigil::PowPow => (16, 17),

        Sigil::OrOr => (14, 15),

        // Inserted here...
        Sigil::Tern => (13, 12),

        // And pushed these down from 12, 13 to accomodate ternary
        Sigil::MulAssign => (10, 11),
        Sigil::DivAssign => (10, 11),
        Sigil::AddAssign => (10, 11),
        Sigil::SubAssign => (10, 11),

        Sigil::Dollar => (8, 9),
        Sigil::At => (8, 9),

        Sigil::DollarAssign => (6, 7),
        Sigil::AtAssign => (6, 7),
        // Technically there's a `-=` for strings with an even lower binding power
        // here. We just pretend it doesn't exist. Fix your own code.
        _ => return None,
    };
    Some(res)
}
