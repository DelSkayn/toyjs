use crate::{
    interner::StringId,
    parser::*,
    ssa::{BinOp, Expr, Instruction, SsaBuilder, SsaId, UnaryOp, VariableId},
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
    pub fn parse_ops(&mut self, builder: &mut SsaBuilder) -> PResult<SsaId> {
        let res = self.parse_ops_rec(0, builder)?;
        Ok(builder.evaluate(res))
    }

    fn parse_prefix_op(&mut self, r_bp: u8, builder: &mut SsaBuilder) -> PResult<Expr> {
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
                let right = self.parse_ops_rec(r_bp, builder)?;
                if !right.is_assignable() {
                    unexpected!(self => "right hand side is not assigable");
                }
                let kind = if let t!("++") = x {
                    UnaryOp::AddOne
                } else {
                    UnaryOp::SubtractOne
                };
                let val = builder.unary_op(kind, right);
                builder.assign(right, val);
                return Ok(val);
            }
            _ => panic!("not a unary operator"),
        };
        let operand = self.parse_ops_rec(r_bp, builder)?;
        let res = builder.unary_op(kind, operand);
        Ok(res)
    }

    fn parse_postfix_op(
        &mut self,
        _l_bp: u8,
        lhs: Expr,
        builder: &mut SsaBuilder,
    ) -> PResult<Expr> {
        match self.next()?.unwrap().kind {
            x @ t!("++") | x @ t!("--") => {
                if !lhs.is_assignable() {
                    unexpected!(self => "left hand side is not assigable");
                }
                let value = builder.evaluate(lhs);
                let kind = if let t!("++") = x {
                    UnaryOp::AddOne
                } else {
                    UnaryOp::SubtractOne
                };
                let post_value = builder.unary_op(kind, lhs);
                builder.assign(lhs, post_value);
                Ok(value.into())
            }
            _ => to_do!(self),
        }
    }

    fn parse_infix_op(&mut self, r_bp: u8, lhs: Expr, builder: &mut SsaBuilder) -> PResult<Expr> {
        let kind = match self.next()?.unwrap().kind {
            t!("?") => {
                let jump_targets = builder.take_expr_context();
                let cond_jump_instr = builder.jump_cond(OptionSsaId::none(), lhs, false);
                let mhs = self.parse_ops_rec(0, builder)?;
                let mhs = builder.evaluate(mhs);
                let jump_instr = builder.jump(OptionSsaId::none());

                expect!(self, ":");

                builder.patch_jump(cond_jump_instr, builder.next());
                builder.patch_expr_jump(&jump_targets, builder.next(), true);

                let rhs = self.parse_ops_rec(r_bp, builder)?;
                let rhs = builder.evaluate(rhs);
                builder.patch_jump(jump_instr, builder.next());
                return Ok(builder.alias(mhs.into(), rhs.into()));
            }
            x @ t!("||") | x @ t!("&&") => {
                let lhs = builder.evaluate(lhs);
                let cond_jump =
                    builder.jump_cond_context(OptionSsaId::none(), lhs.into(), x == t!("||"), true);

                let rhs = self.parse_ops_rec(r_bp, builder)?;
                let rhs = builder.evaluate(rhs);

                let target = builder.next();
                let res = builder.alias(lhs.into(), rhs.into());
                builder.patch_jump(cond_jump, target);
                return Ok(res);
            }
            t!("??") => {
                let lhs = builder.evaluate(lhs);
                builder.take_expr_context();
                let cond = builder.unary_op(UnaryOp::IsNullish, lhs.into());
                let cond_jump = builder.jump_cond(OptionSsaId::none(), cond, false);
                let rhs = self.parse_ops_rec(r_bp, builder)?;
                let rhs = builder.evaluate(rhs);
                let target = builder.next();
                let res = builder.alias(lhs.into(), rhs.into());
                builder.patch_jump(cond_jump, target);
                return Ok(res);
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
                if !lhs.is_assignable() {
                    unexpected!(self => "left hand side is not assigable");
                }
                let rhs = self.parse_ops_rec(r_bp, builder)?;
                let rhs = builder.evaluate(rhs);
                builder.assign(lhs, rhs.into());
                return Ok(rhs.into());
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

                if !lhs.is_assignable() {
                    unexpected!(self => "left hand side is not assigable");
                }
                let rhs = self.parse_ops_rec(r_bp, builder)?;
                let left = builder.evaluate(lhs);
                let value = builder.bin_op(bin_op, left.into(), rhs);
                builder.assign(lhs, value);
                return Ok(value);
            }
            _ => unreachable!(),
        };
        let left = builder.evaluate(lhs);
        let rhs = self.parse_ops_rec(r_bp, builder)?;
        let right = builder.evaluate(rhs);
        let value = builder.bin_op(kind, left.into(), right.into());
        Ok(value)
    }

    fn parse_ops_rec(&mut self, min_bp: u8, builder: &mut SsaBuilder) -> PResult<Expr> {
        trace_log!("assign expr");
        let mut lhs = if let Some(((), r_bp)) = self.peek_kind()?.and_then(prefix_binding_power) {
            self.parse_prefix_op(r_bp, builder)?
        } else {
            self.parse_prime_expr(builder)?
        };

        while let Some(op) = self.peek_kind()? {
            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lhs = self.parse_postfix_op(l_bp, lhs, builder)?;
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lhs = self.parse_infix_op(r_bp, lhs, builder)?;
                continue;
            }
            break;
        }
        Ok(lhs)
    }
}
