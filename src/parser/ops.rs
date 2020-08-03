use crate::{
    parser::*,
    ssa::{BinOp, InstrVar, Instruction, SsaVar, UnaryOp},
    token::TokenKind,
};

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
        self.parse_ops_rec(0)
    }

    fn parse_prefix_op(&mut self, r_bp: u8) -> PResult<SsaVar> {
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
            t!("++") | t!("--") => {
                to_do!(self)
                /*
                let right = self.parse_ops_rec(r_bp)?;
                let kind = if let t!("++") = x{
                    BinOp::Add
                }else{
                    BinOp::Sub
                };
                let one = self.builder.load_constant(1);
                return Ok(self.builder.push_instruction(Instruction::Binary{
                    kind,
                    left: one,
                    right,
                }))
                */
            }
            _ => panic!("not a unary operator"),
        };
        let operand = self.parse_ops_rec(r_bp)?.into();
        Ok(self
            .builder
            .push_instruction(Instruction::Unary { kind, operand }))
    }

    fn parse_postfix_op(&mut self, _l_bp: u8, _lhs: SsaVar) -> PResult<SsaVar> {
        to_do!(self);
        /*
        match self.next().unwrap().kind {
            t!("[") => to_do!(self),
            t!("(") => to_do!(self),
            _x @ t!("++") | t!("--") => to_do!(self),
        }
        if eat!(self, "[") {
            let expr = self.parse_expr()?;
            expect!(self, "]");
            return Op::Postfix {
                expr: Box::new(lhs),
                op: PostfixOp::Index(expr),
            };
        }
        if is!(self, "(") {
            let args = self.parse_arguments()?;
            return Op::Postfix {
                expr: Box::new(lhs),
                op: PostfixOp::Call(args),
            };
        }
        Op::Postfix {
            expr: Box::new(lhs),
            op: if eat!(self, "++") {
                PostfixOp::Increment
            } else if eat!(self, "--") {
                PostfixOp::Decrement
            } else {
                panic!("encountered postfix operator with binding power but no production")
            },
        }
        */
    }

    fn parse_infix_op(&mut self, r_bp: u8, lhs: SsaVar) -> PResult<'a, SsaVar> {
        let kind = match self.next()?.unwrap().kind {
            t!("?") => {
                let cond_jump_instr = self.builder.push_instruction(Instruction::CondJump {
                    negative: true,
                    condition: lhs.into(),
                    target: InstrVar::null(),
                });
                let next_id = self.builder.next_id();
                let mut mhs = self.parse_ops_rec(0)?;
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
                let mut rhs = self.parse_ops_rec(r_bp)?;
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
                return Ok(res);
            }
            x @ t!("||") | x @ t!("&&") => {
                let cond_jump = self.builder.push_instruction(Instruction::CondJump {
                    negative: x == t!("&&"),
                    condition: lhs.into(),
                    target: InstrVar::null(),
                });
                let next_id = self.builder.next_id();
                let mut rhs = self.parse_ops_rec(r_bp)?;
                if next_id == self.builder.next_id() {
                    rhs = self.builder.push_instruction(Instruction::Move {
                        operand: rhs.into(),
                    });
                }
                let target = self.builder.push_instruction(Instruction::Alias {
                    left: lhs.into(),
                    right: rhs.into(),
                });
                self.builder.patch_jump_target(cond_jump, target.into());
                return Ok(target);
            }
            t!("??") => {
                let cond = self.builder.push_instruction(Instruction::Unary {
                    kind: UnaryOp::IsNullish,
                    operand: lhs.into(),
                });
                let cond_jump = self.builder.push_instruction(Instruction::CondJump {
                    negative: true,
                    condition: cond.into(),
                    target: InstrVar::null(),
                });
                let next_id = self.builder.next_id();
                let mut rhs = self.parse_ops_rec(r_bp)?;
                if next_id == self.builder.next_id() {
                    rhs = self.builder.push_instruction(Instruction::Move {
                        operand: rhs.into(),
                    });
                }
                let target = self.builder.push_instruction(Instruction::Alias {
                    left: lhs.into(),
                    right: rhs.into(),
                });
                self.builder.patch_jump_target(cond_jump, target.into());
                return Ok(target);
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
            t!("=") => to_do!(self),
            t!("*=") => to_do!(self),
            t!("/=") => to_do!(self),
            t!("%=") => to_do!(self),
            t!("+=") => to_do!(self),
            t!("-=") => to_do!(self),
            t!("<<=") => to_do!(self),
            t!(">>=") => to_do!(self),
            t!(">>>=") => to_do!(self),
            t!("&=") => to_do!(self),
            t!("^=") => to_do!(self),
            t!("|=") => to_do!(self),
            t!("**=") => to_do!(self),
            _ => unreachable!(),
        };
        let rhs = self.parse_ops_rec(r_bp)?;
        Ok(self.builder.push_instruction(Instruction::Binary {
            kind,
            left: lhs.into(),
            right: rhs.into(),
        }))
    }

    fn parse_ops_rec(&mut self, min_bp: u8) -> PResult<'a, SsaVar> {
        trace_log!("assign expr");
        let mut lhs =
            if let Some(((), r_bp)) = self.peek_kind()?.and_then(|x| prefix_binding_power(x)) {
                self.parse_prefix_op(r_bp)?
            } else {
                self.parse_prime_expr()?
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
