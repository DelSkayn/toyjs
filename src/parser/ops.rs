use crate::{
    interner::StringId,
    parser::prime::PrimeExpr,
    parser::*,
    ssa::{BinOp, InstrVar, Instruction, SsaVar, UnaryOp},
    token::TokenKind,
};

#[derive(Clone, Copy)]
pub enum Expr {
    Index {
        object: SsaVar,
        index: SsaVar,
    },
    Ident {
        object: Option<SsaVar>,
        ident: StringId,
    },
    Expr(SsaVar),
}

impl Expr {
    pub fn from_value(v: SsaVar) -> Expr {
        Expr::Expr(v)
    }

    pub fn assignable(&self) -> bool {
        match *self {
            Expr::Ident {
                object: _,
                ident: _,
            } => true,
            Expr::Index {
                object: _,
                index: _,
            } => true,
            Expr::Expr(_) => false,
        }
    }

    pub fn from_prime(prime: PrimeExpr) -> Expr {
        match prime {
            PrimeExpr::Ident(x) => Expr::Ident {
                object: None,
                ident: x,
            },
            PrimeExpr::Variable(x) => Expr::Expr(x),
        }
    }

    pub fn from_prime_index(prime: PrimeExpr, object: SsaVar) -> Expr {
        match prime {
            PrimeExpr::Ident(x) => Expr::Ident {
                object: Some(object),
                ident: x,
            },
            PrimeExpr::Variable(x) => Expr::Index { object, index: x },
        }
    }

    pub fn to_value(self, parser: &mut Parser) -> SsaVar {
        match self {
            Expr::Index { object, index } => {
                parser.builder.push_instruction(Instruction::ObjectGet {
                    object: object.into(),
                    key: index.into(),
                })
            }
            Expr::Ident { object, ident } => {
                let object = if let Some(object) = object {
                    object
                } else {
                    parser.builder.push_instruction(Instruction::LoadGlobal)
                };
                let key = parser.builder.load_constant(ident);
                parser.builder.push_instruction(Instruction::ObjectGet {
                    object: object.into(),
                    key: key.into(),
                })
            }
            Expr::Expr(x) => x,
        }
    }

    pub fn assign(self, parser: &mut Parser, value: SsaVar) -> PResult<()> {
        match self {
            Expr::Index { object, index } => {
                parser.builder.push_instruction(Instruction::ObjectSet {
                    object: object.into(),
                    key: index.into(),
                    value: value.into(),
                });
                Ok(())
            }
            Expr::Ident { object, ident } => {
                let object = if let Some(object) = object {
                    object
                } else {
                    parser.builder.push_instruction(Instruction::LoadGlobal)
                };
                let key = parser.builder.load_constant(ident);
                parser.builder.push_instruction(Instruction::ObjectSet {
                    object: object.into(),
                    key: key.into(),
                    value: value.into(),
                });
                Ok(())
            }
            Expr::Expr(_) => unexpected!(parser => "left hand side is not assignable"),
        }
    }
}

fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8)> {
    match kind {
        t!("=")
        | t!("*=")
        | t!("/=")
        | t!("%=")
        | t!("+=")
        | t!("-=")
        | t!("<<=")
        | t!(">>=")
        | t!(">>>=")
        | t!("&=")
        | t!("^=")
        | t!("|=")
        | t!("**=") => Some((1, 2)),
        t!("?") => Some((4, 3)),
        t!("??") => Some((6, 5)),
        t!("||") => Some((7, 8)),
        t!("&&") => Some((9, 10)),
        t!("|") => Some((11, 12)),
        t!("^") => Some((13, 14)),
        t!("&") => Some((15, 16)),
        t!("==") | t!("!=") | t!("===") | t!("!==") => Some((17, 18)),
        t!("<") | t!(">") | t!("<=") | t!(">=") | t!("instanceof") | t!("in") => Some((19, 20)),
        t!("<<") | t!(">>") | t!(">>>") => Some((21, 22)),
        t!("+") | t!("-") => Some((23, 24)),
        t!("*") | t!("%") | t!("/") => Some((25, 26)),
        t!("**") => Some((27, 28)),
        t!("?.") => Some((36, 35)),
        t!(".") => Some((38, 37)),
        _ => None,
    }
}

fn postfix_binding_power(kind: TokenKind) -> Option<(u8, ())> {
    match kind {
        t!("++") | t!("--") => Some((31, ())),
        t!("[") | t!("(") => Some((38, ())),
        _ => None,
    }
}

fn prefix_binding_power(kind: TokenKind) -> Option<((), u8)> {
    match kind {
        t!("delete") | t!("void") | t!("typeof") | t!("+") | t!("-") | t!("~") | t!("!") => {
            Some(((), 29))
        }
        t!("++") | t!("--") => Some(((), 31)),
        t!("new") => Some(((), 33)),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    pub fn parse_ops(&mut self) -> PResult<SsaVar> {
        Ok(self.parse_ops_rec(0)?.to_value(self))
    }

    fn parse_prefix_op(&mut self, r_bp: u8) -> PResult<Expr> {
        let kind = match self.next()?.unwrap().kind {
            t!("delete") => UnaryOp::Delete,
            t!("void") => UnaryOp::Void,
            t!("typeof") => UnaryOp::Typeof,
            t!("new") => {
                to_do!(self);
            }
            t!("+") => UnaryOp::Postive,
            t!("-") => UnaryOp::Negative,
            t!("!") => UnaryOp::Not,
            t!("~") => UnaryOp::BinaryNot,
            x @ t!("++") | x @ t!("--") => {
                let right = self.parse_ops_rec(r_bp)?;
                let kind = if let t!("++") = x {
                    BinOp::Add
                } else {
                    BinOp::Subtract
                };
                let one = self.builder.load_constant(1);
                let left = right.to_value(self).into();
                let val = self.builder.push_instruction(Instruction::Binary {
                    kind,
                    left,
                    right: one.into(),
                });
                right.assign(self, val)?;
                return Ok(Expr::from_value(val));
            }
            _ => panic!("not a unary operator"),
        };
        let operand = self.parse_ops_rec(r_bp)?.to_value(self).into();
        let res = self
            .builder
            .push_instruction(Instruction::Unary { kind, operand });
        Ok(Expr::from_value(res))
    }

    fn parse_postfix_op(&mut self, _l_bp: u8, lhs: Expr) -> PResult<Expr> {
        match self.next()?.unwrap().kind {
            x @ t!("++") | x @ t!("--") => {
                let value = lhs.to_value(self);
                let kind = if let t!("++") = x {
                    BinOp::Add
                } else {
                    BinOp::Subtract
                };
                let one = self.builder.load_constant(1);
                let post_value = self.builder.push_instruction(Instruction::Binary {
                    kind,
                    left: value.into(),
                    right: one.into(),
                });
                lhs.assign(self, post_value)?;
                return Ok(Expr::from_value(value));
            }
            _ => to_do!(self),
        }
    }

    fn parse_infix_op(&mut self, r_bp: u8, lhs: Expr) -> PResult<Expr> {
        let kind = match self.next()?.unwrap().kind {
            t!("?") => {
                let condition = lhs.to_value(self).into();
                let cond_jump_instr = self.builder.push_instruction(Instruction::CondJump {
                    negative: true,
                    condition,
                    target: InstrVar::null(),
                });
                let next_id = self.builder.next_id();
                let mut mhs = self.parse_ops_rec(0)?.to_value(self);
                if next_id == self.builder.next_id() {
                    mhs = self.builder.push_instruction(Instruction::Move {
                        operand: mhs.into(),
                    });
                }
                let jump_instr = self.builder.push_instruction(Instruction::Jump {
                    target: InstrVar::null(),
                });
                expect!(self, ":");
                self.builder.patch_jump_target_next(cond_jump_instr);
                let next_id = self.builder.next_id();
                let mut rhs = self.parse_ops_rec(r_bp)?.to_value(self);
                if next_id == self.builder.next_id() {
                    rhs = self.builder.push_instruction(Instruction::Move {
                        operand: rhs.into(),
                    });
                }
                self.builder.patch_jump_target_next(jump_instr);
                let res = self.builder.push_instruction(Instruction::Alias {
                    left: mhs.into(),
                    right: rhs.into(),
                });
                return Ok(Expr::from_value(res));
            }
            x @ t!("||") | x @ t!("&&") => {
                let condition = lhs.to_value(self).into();
                let cond_jump = self.builder.push_instruction(Instruction::CondJump {
                    negative: x == t!("&&"),
                    condition,
                    target: InstrVar::null(),
                });
                let next_id = self.builder.next_id();
                let mut rhs = self.parse_ops_rec(r_bp)?.to_value(self);
                if next_id == self.builder.next_id() {
                    rhs = self.builder.push_instruction(Instruction::Move {
                        operand: rhs.into(),
                    });
                }
                let left = lhs.to_value(self).into();
                let target = self.builder.push_instruction(Instruction::Alias {
                    left,
                    right: rhs.into(),
                });
                self.builder.patch_jump_target(cond_jump, target.into());
                return Ok(Expr::from_value(target));
            }
            t!("??") => {
                let operand = lhs.to_value(self).into();
                let cond = self.builder.push_instruction(Instruction::Unary {
                    kind: UnaryOp::IsNullish,
                    operand,
                });
                let cond_jump = self.builder.push_instruction(Instruction::CondJump {
                    negative: true,
                    condition: cond.into(),
                    target: InstrVar::null(),
                });
                let next_id = self.builder.next_id();
                let mut rhs = self.parse_ops_rec(r_bp)?.to_value(self);
                if next_id == self.builder.next_id() {
                    rhs = self.builder.push_instruction(Instruction::Move {
                        operand: rhs.into(),
                    });
                }
                let left = lhs.to_value(self).into();
                let target = self.builder.push_instruction(Instruction::Alias {
                    left,
                    right: rhs.into(),
                });
                self.builder.patch_jump_target(cond_jump, target.into());
                return Ok(Expr::from_value(target));
            }
            t!("|") => BinOp::BitwiseOr,
            t!("&") => BinOp::BitwiseAnd,
            t!("^") => BinOp::BitwiseXor,
            t!("+") => BinOp::Add,
            t!("-") => BinOp::Subtract,
            t!("*") => BinOp::Multiply,
            t!("/") => BinOp::Divide,
            t!("%") => BinOp::Modulo,
            t!("**") => BinOp::Power,
            t!("<") => BinOp::Less,
            t!("<=") => BinOp::LessEqual,
            t!(">") => BinOp::Greater,
            t!(">=") => BinOp::GreaterEqual,
            t!("<<") => BinOp::ShiftLeft,
            t!(">>") => BinOp::ShiftRight,
            t!(">>>") => BinOp::ShiftRightUnsigned,
            t!("instanceof") => BinOp::InstanceOf,
            t!("in") => BinOp::In,
            t!(".") => BinOp::Dot,
            t!("==") => BinOp::Equal,
            t!("===") => BinOp::StrictEqual,
            t!("!=") => BinOp::NotEqual,
            t!("!==") => BinOp::StrictNotEqual,
            // TODO create a way to check for assignability
            t!("=") => {
                if !lhs.assignable() {
                    unexpected!(self => "left hand side is not assigable");
                }
                let rhs = self.parse_ops_rec(r_bp)?.to_value(self);
                lhs.assign(self, rhs)?;
                return Ok(Expr::from_value(rhs));
            }
            x @ t!("*=")
            | x @ t!("/=")
            | x @ t!("%=")
            | x @ t!("+=")
            | x @ t!("-=")
            | x @ t!("<<=")
            | x @ t!(">>=")
            | x @ t!(">>>=")
            | x @ t!("&=")
            | x @ t!("^=")
            | x @ t!("|=")
            | x @ t!("**=") => {
                let bin_op = match x {
                    t!("*=") => BinOp::Multiply,
                    t!("/=") => BinOp::Divide,
                    t!("%=") => BinOp::Modulo,
                    t!("+=") => BinOp::Add,
                    t!("-=") => BinOp::Subtract,
                    t!("<<=") => BinOp::ShiftLeft,
                    t!(">>=") => BinOp::ShiftRight,
                    t!(">>>=") => BinOp::ShiftRightUnsigned,
                    t!("&=") => BinOp::BitwiseAnd,
                    t!("^=") => BinOp::BitwiseXor,
                    t!("|=") => BinOp::BitwiseOr,
                    t!("**=") => BinOp::Power,
                    _ => unreachable!(),
                };

                if !lhs.assignable() {
                    unexpected!(self => "left hand side is not assigable");
                }
                let rhs = self.parse_ops_rec(r_bp)?.to_value(self);
                let left = lhs.to_value(self).into();
                let value = self.builder.push_instruction(Instruction::Binary {
                    left,
                    right: rhs.into(),
                    kind: bin_op,
                });
                lhs.assign(self, value)?;
                return Ok(Expr::from_value(rhs));
            }
            _ => unreachable!(),
        };
        let rhs = self.parse_ops_rec(r_bp)?;
        let left = lhs.to_value(self).into();
        let right = rhs.to_value(self).into();
        let value = self
            .builder
            .push_instruction(Instruction::Binary { kind, left, right });
        Ok(Expr::from_value(value))
    }

    fn parse_ops_rec(&mut self, min_bp: u8) -> PResult<Expr> {
        trace_log!("assign expr");
        let mut lhs =
            if let Some(((), r_bp)) = self.peek_kind()?.and_then(|x| prefix_binding_power(x)) {
                self.parse_prefix_op(r_bp)?
            } else {
                Expr::from_prime(self.parse_prime_expr()?)
            };

        loop {
            let op = match self.peek_kind()? {
                Some(x) => x,
                None => break,
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lhs = self.parse_postfix_op(l_bp, lhs)?;
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lhs = self.parse_infix_op(r_bp, lhs)?;
                continue;
            }
            break;
        }
        Ok(lhs)
    }
}
