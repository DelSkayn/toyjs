use super::*;

use ast::{AssignOperator, BinaryOperator, Expr, PostfixOperator, PrefixOperator};
use token::{t, TokenKind};

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
        | t!("**=") => Some((2, 1)),
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
        //t!(".") => Some((38, 37)),
        _ => None,
    }
}

fn postfix_binding_power(kind: TokenKind) -> Option<(u8, ())> {
    match kind {
        t!("++") | t!("--") => Some((31, ())),
        t!("[") | t!(".") | t!("(") => Some((38, ())),
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

impl<'a, A: Allocator + Clone> Parser<'a, A> {
    pub(crate) fn parse_expr(&mut self) -> Result<Vec<Expr<A>, A>> {
        let mut res = Vec::new_in(self.alloc.clone());
        loop {
            res.push(self.parse_ops_rec(0)?);
            if !self.eat(t!(","))? {
                break;
            }
        }
        Ok(res)
    }

    pub(crate) fn parse_single_expr(&mut self) -> Result<Expr<A>> {
        self.parse_ops_rec(0)
    }

    fn parse_prefix_op(&mut self, r_bp: u8) -> Result<Expr<A>> {
        Ok(Expr::UnaryPrefix(
            match self.next()?.unwrap().kind {
                t!("delete") => PrefixOperator::Delete,
                t!("void") => PrefixOperator::Void,
                t!("typeof") => PrefixOperator::TypeOf,
                t!("new") => {
                    to_do!(self);
                }
                t!("+") => PrefixOperator::Positive,
                t!("-") => PrefixOperator::Negative,
                t!("!") => PrefixOperator::Not,
                t!("~") => PrefixOperator::BinaryNot,
                t!("++") => PrefixOperator::AddOne,
                t!("--") => PrefixOperator::SubtractOne,
                _ => panic!("not a unary operator"),
            },
            Box::new_in(self.parse_ops_rec(r_bp)?, self.alloc.clone()),
        ))
    }

    fn parse_postfix_op(&mut self, _l_bp: u8, lhs: Expr<A>) -> Result<ast::Expr<A>> {
        let kind = match self.next()?.unwrap().kind {
            t!("++") => PostfixOperator::AddOne,
            t!("--") => PostfixOperator::SubtractOne,
            t!(".") => {
                expect_bind!(self,let index = "ident");
                PostfixOperator::Dot(index)
            }
            t!("(") => {
                let stmts = if let Some(t!(")")) = self.peek_kind()? {
                    Vec::new_in(self.alloc.clone())
                } else {
                    self.parse_expr()?
                };
                expect!(self, ")");
                PostfixOperator::Call(stmts)
            }
            t!("[") => {
                let expr = self.parse_single_expr()?;
                expect!(self, "]");
                PostfixOperator::Index(Box::new_in(expr, self.alloc.clone()))
            }
            _ => to_do!(self),
        };
        Ok(Expr::UnaryPostfix(
            Box::new_in(lhs, self.alloc.clone()),
            kind,
        ))
    }

    fn parse_infix_op(&mut self, r_bp: u8, lhs: Expr<A>) -> Result<ast::Expr<A>> {
        let kind = match self.next()?.unwrap().kind {
            t!("?") => {
                let expr = self.parse_ops_rec(r_bp)?;
                expect!(self, ":");
                let expr = Box::new_in(expr, self.alloc.clone());
                BinaryOperator::Ternary(expr)
            }
            t!("??") => BinaryOperator::NullCoalessing,
            t!("?.") => BinaryOperator::TenaryNull,
            t!("||") => BinaryOperator::Or,
            t!("&&") => BinaryOperator::And,
            t!("|") => BinaryOperator::BitwiseOr,
            t!("&") => BinaryOperator::BitwiseAnd,
            t!("^") => BinaryOperator::BitwiseXor,
            t!("+") => BinaryOperator::Add,
            t!("-") => BinaryOperator::Subtract,
            t!("*") => BinaryOperator::Multiply,
            t!("/") => BinaryOperator::Divide,
            t!("%") => BinaryOperator::Modulo,
            t!("**") => BinaryOperator::Exponentiate,
            t!("<") => BinaryOperator::Less,
            t!("<=") => BinaryOperator::LessEqual,
            t!(">") => BinaryOperator::Greater,
            t!(">=") => BinaryOperator::GreaterEqual,
            t!("<<") => BinaryOperator::ShiftLeft,
            t!(">>") => BinaryOperator::ShiftRight,
            t!(">>>") => BinaryOperator::ShiftRightUnsigned,
            t!("instanceof") => BinaryOperator::InstanceOf,
            t!("in") => BinaryOperator::In,
            t!(".") => BinaryOperator::Index,
            t!("==") => BinaryOperator::Equal,
            t!("===") => BinaryOperator::StrictEqual,
            t!("!=") => BinaryOperator::NotEqual,
            t!("!==") => BinaryOperator::StrictNotEqual,
            x @ t!("=")
            | x @ t!("*=")
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
                let op = match x {
                    t!("=") => AssignOperator::Assign,
                    t!("*=") => AssignOperator::Multiply,
                    t!("/=") => AssignOperator::Divide,
                    t!("%=") => AssignOperator::Modulo,
                    t!("+=") => AssignOperator::Add,
                    t!("-=") => AssignOperator::Subtract,
                    t!("<<=") => AssignOperator::ShiftLeft,
                    t!(">>=") => AssignOperator::ShiftRight,
                    t!(">>>=") => AssignOperator::ShiftRightUnsigned,
                    t!("&=") => AssignOperator::BitwiseAnd,
                    t!("^=") => AssignOperator::BitwiseXor,
                    t!("|=") => AssignOperator::BitwiseOr,
                    t!("**=") => AssignOperator::Exponentiate,
                    _ => unreachable!(),
                };
                if !lhs.is_assignable() {
                    unexpected!(self => "left hand side is not assignable");
                }

                let rhs = self.parse_ops_rec(r_bp)?;
                return Ok(Expr::Assign(
                    Box::new_in(lhs, self.alloc.clone()),
                    op,
                    Box::new_in(rhs, self.alloc.clone()),
                ));
            }
            _ => unreachable!(),
        };
        Ok(Expr::Binary(
            Box::new_in(lhs, self.alloc.clone()),
            kind,
            Box::new_in(self.parse_ops_rec(r_bp)?, self.alloc.clone()),
        ))
    }

    fn parse_ops_rec(&mut self, min_bp: u8) -> Result<Expr<A>> {
        let mut lhs = if let Some(((), r_bp)) = self.peek_kind()?.and_then(prefix_binding_power) {
            self.parse_prefix_op(r_bp)?
        } else {
            Expr::Prime(self.parse_prime_expr()?)
        };

        while let Some(op) = self.peek_kind()? {
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
