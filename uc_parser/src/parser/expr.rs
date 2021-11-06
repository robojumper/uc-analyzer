//! UnrealScript statement and expression parser based on Pratt parsing.
//! See https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
//! for an introduction to Pratt parsing.

use std::str::FromStr;

use uc_def::{BaseExpr, Identifier};

use crate::{
    kw,
    lexer::{Sigil, Token, TokenKind as Tk},
};

use super::Parser;

impl Parser<'_> {
    pub fn parse_base_expression(&mut self) -> Result<BaseExpr<Identifier>, String> {
        self.parse_base_expression_bp(0)
    }

    fn parse_base_expression_bp(&mut self, min_bp: u8) -> Result<BaseExpr<Identifier>, String> {
        let mut lhs = {
            let tok = self.next_any()?;
            match tok.kind {
                Tk::Sig(Sigil::LParen) => {
                    let lhs = self.parse_base_expression_bp(0)?;
                    self.expect(Tk::Sig(Sigil::RParen))?;
                    lhs
                }
                Tk::Sig(sig) => match prefix_binding_power(sig) {
                    Some(((), r_bp)) => {
                        let rhs = self.parse_base_expression_bp(r_bp)?;
                        BaseExpr::PreOpExpr {
                            op: Identifier::from_str(&format!("__op_{}", sig.as_ref())).unwrap(),
                            rhs: Box::new(rhs),
                        }
                    }
                    None => return Err(format!("Not a preoperator: {:?}", tok)),
                },
                kw!(New) => {
                    self.next();
                    let mut args = vec![];
                    if self.eat(Tk::Sig(Sigil::LParen)) {
                        loop {
                            let expr = self.parse_base_expression_bp(0)?;
                            args.push(expr);
                            let delim = self.next_any()?;
                            match delim.kind {
                                Tk::Comma => continue,
                                Tk::Sig(Sigil::RParen) => break,
                                _ => return Err(format!("Expected comma or }}, got {:?}", delim)),
                            }
                        }
                    }

                    let ((), r_bp) = NEW_PREFIX_POWER;

                    let cls = self.parse_base_expression_bp(r_bp)?;

                    BaseExpr::NewExpr {
                        args,
                        cls: Box::new(cls),
                    }
                }
                Tk::Sym(_) if matches!(self.peek(), Some(Token { kind: Tk::Name, .. })) => {
                    self.next();
                    BaseExpr::LiteralExpr {
                        lit: uc_def::Literal::ObjReference,
                    }
                }
                Tk::Sym(_)
                    if matches!(
                        self.peek(),
                        Some(Token {
                            kind: Tk::Sig(Sigil::LParen),
                            ..
                        })
                    ) =>
                {
                    let func = self.sym_to_ident(&tok);
                    self.next();
                    let mut args = vec![];
                    loop {
                        let expr = self.parse_base_expression_bp(0)?;
                        args.push(expr);
                        let delim = self.next_any()?;
                        match delim.kind {
                            Tk::Comma => continue,
                            Tk::Sig(Sigil::RParen) => break,
                            _ => return Err(format!("Expected comma or }}, got {:?}", delim)),
                        }
                    }
                    BaseExpr::CallExpr { func, args }
                }
                kw!(None) => BaseExpr::LiteralExpr {
                    lit: uc_def::Literal::None,
                },
                Tk::Sym(_) => {
                    BaseExpr::SymExpr {
                        sym: self.sym_to_ident(&tok),
                    }
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
                }) if is_infix_or_suffix_op(s) => s,
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

                lhs = if op == Sigil::LBrack {
                    let rhs = self.parse_base_expression_bp(0)?;
                    self.expect(Tk::Sig(Sigil::RBrack))?;
                    BaseExpr::IndexExpr {
                        base: Box::new(lhs),
                        idx: Box::new(rhs),
                    }
                } else {
                    BaseExpr::PostOpExpr {
                        lhs: Box::new(lhs),
                        op: Identifier::from_str(&format!("__op_{}", op.as_ref())).unwrap(),
                    }
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.next();

                lhs = if op == Sigil::Tern {
                    let mhs = self.parse_base_expression_bp(0)?;
                    self.expect(Tk::Sig(Sigil::Colon))?;
                    let rhs = self.parse_base_expression_bp(r_bp)?;
                    BaseExpr::TernExpr {
                        cond: Box::new(lhs),
                        then: Box::new(mhs),
                        alt: Box::new(rhs),
                    }
                } else {
                    let rhs = self.parse_base_expression_bp(r_bp)?;
                    if op == Sigil::Dot {
                        BaseExpr::FieldExpr {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }
                    } else {
                        BaseExpr::BinOpExpr {
                            lhs: Box::new(lhs),
                            op: Identifier::from_str(&format!("__op_{}", op.as_ref())).unwrap(),
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

fn is_infix_or_suffix_op(op: Sigil) -> bool {
    postfix_binding_power(op).is_some() || infix_binding_power(op).is_some()
}

// Todo: is this correct?
fn prefix_binding_power(op: Sigil) -> Option<((), u8)> {
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
fn postfix_binding_power(op: Sigil) -> Option<(u8, ())> {
    let res = match op {
        Sigil::AddAdd => (10, ()),
        Sigil::SubSub => (10, ()),
        Sigil::LBrack => (38, ()),
        _ => return None,
    };
    Some(res)
}

// TODO - no idea what constructions are valid after new class''. Just
// assign something super high so it breaks when anything but a ; or closing parameter appears.
const NEW_PREFIX_POWER: ((), u8) = ((), 100);

fn infix_binding_power(op: Sigil) -> Option<(u8, u8)> {
    let res = match op {
        Sigil::Tern => (9, 8),

        Sigil::MulMul => (12, 13),

        Sigil::Mul => (16, 17),
        Sigil::Div => (16, 17),

        Sigil::Mod => (18, 19),

        Sigil::Add => (20, 21),
        Sigil::Sub => (20, 21),

        Sigil::LtLt => (22, 23),
        Sigil::GtGt => (22, 23),
        Sigil::GtGtGt => (22, 23),

        Sigil::Lt => (24, 25),
        Sigil::Gt => (24, 25),
        Sigil::LtEq => (24, 25),
        Sigil::GtEq => (24, 25),
        Sigil::EqEq => (24, 25),

        Sigil::BangEq => (26, 27),

        Sigil::And => (28, 29),
        Sigil::Pow => (28, 29),
        Sigil::Or => (28, 29),

        Sigil::AndAnd => (30, 31),
        Sigil::PowPow => (30, 31),

        Sigil::OrOr => (32, 33),

        Sigil::MulAssign => (34, 35),
        Sigil::DivAssign => (34, 35),
        Sigil::AddAssign => (34, 35),
        Sigil::SubAssign => (34, 35),

        Sigil::Dot => (40, 41),
        _ => return None,
    };
    Some(res)
}
