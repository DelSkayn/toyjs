use super::{Allocator, Parser, Result};

use ast::{
    symbol_table::ScopeKind, ArrowBody, AssignOperator, BinaryOperator, Expr, PostfixOperator,
    PrefixOperator, PrimeExpr,
};
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
        | t!("**=")
        | t!("=>") => Some((2, 1)),
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

impl<A: Allocator + Clone> Parser<'_, '_, A> {
    pub(crate) fn parse_expr(&mut self) -> Result<Vec<Expr<A>, A>> {
        let mut res = Vec::new_in(self.alloc.clone());
        loop {
            res.push(self.parse_single_expr()?);
            if !self.eat(t!(","))? {
                break;
            }
        }
        Ok(res)
    }

    pub(crate) fn parse_single_expr(&mut self) -> Result<Expr<A>> {
        let res = self.parse_ops_rec(0)?;
        if self.peek_lt()?.map(|x| x.kind) == Some(t!("=>")) {
            return self.reparse_arrow_function(res);
        }
        Ok(res)
    }

    fn parse_prefix_op(&mut self, r_bp: u8) -> Result<Expr<A>> {
        Ok(Expr::UnaryPrefix(
            match self.next()?.unwrap().kind {
                t!("delete") => PrefixOperator::Delete,
                t!("void") => PrefixOperator::Void,
                t!("typeof") => PrefixOperator::TypeOf,
                t!("new") => {
                    if self.eat(t!("."))? {
                        let peek = self.peek_kind()?;
                        let tgt = self.lexer.interner.intern("target");
                        if peek == Some(TokenKind::Ident(tgt)) {
                            if !self.state.r#return {
                                unexpected!(self => "new.target expression is not allowed here");
                            }
                            self.next()?;
                            return Ok(Expr::Prime(ast::PrimeExpr::NewTarget));
                        } else {
                            unexpected!(self);
                        }
                    }
                    PrefixOperator::New
                }
                t!("+") => PrefixOperator::Positive,
                t!("-") => PrefixOperator::Negative,
                t!("!") => PrefixOperator::Not,
                t!("~") => PrefixOperator::BitwiseNot,
                t!("++") => {
                    let expr = self.parse_ops_rec(r_bp)?;
                    if !expr.is_assignable() {
                        unexpected!(self => "right hand side expression is not assignable")
                    }
                    return Ok(Expr::UnaryPrefix(
                        PrefixOperator::AddOne,
                        Box::new_in(expr, self.alloc.clone()),
                    ));
                }
                t!("--") => {
                    let expr = self.parse_ops_rec(r_bp)?;
                    if !expr.is_assignable() {
                        unexpected!(self => "right hand side expression is not assignable")
                    }
                    return Ok(Expr::UnaryPrefix(
                        PrefixOperator::SubtractOne,
                        Box::new_in(expr, self.alloc.clone()),
                    ));
                }
                _ => panic!("not a unary operator"),
            },
            Box::new_in(self.parse_ops_rec(r_bp)?, self.alloc.clone()),
        ))
    }

    fn parse_postfix_op(&mut self, _l_bp: u8, lhs: Expr<A>) -> Result<ast::Expr<A>> {
        let kind = match self.next()?.unwrap().kind {
            t!("++") => {
                if !lhs.is_assignable() {
                    unexpected!(self => "left hand side expression is not assignable")
                }
                PostfixOperator::AddOne
            }
            t!("--") => {
                if !lhs.is_assignable() {
                    unexpected!(self => "left hand side expression is not assignable")
                }
                PostfixOperator::SubtractOne
            }
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
            t!("=>") => return self.reparse_arrow_function(lhs),
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

    fn reparse_arrow_function(&mut self, expr: Expr<A>) -> Result<Expr<A>> {
        let scope = self.symbol_table.push_scope(ScopeKind::Function);
        let params = self.reparse_as_param(expr)?;
        let body = if self.peek_kind()? == Some(t!("{")) {
            let block = self.alter_state(
                |s| {
                    s.r#return = true;
                    s.r#continue = false;
                    s.r#break = false;
                },
                |this| {
                    expect!(this, "{");
                    let mut stmts = Vec::new_in(this.alloc.clone());
                    while !this.eat(t!("}"))? {
                        stmts.push(this.parse_stmt()?)
                    }
                    Ok(stmts)
                },
            )?;
            ArrowBody::Block(block)
        } else {
            let expr = self.parse_single_expr()?;
            ArrowBody::Expr(Box::new_in(expr, self.alloc.clone()))
        };

        debug_assert_eq!(scope, self.symbol_table.current_scope());
        self.symbol_table.pop_scope();

        Ok(Expr::Prime(PrimeExpr::ArrowFunction(scope, params, body)))
    }

    fn reparse_as_param(&mut self, expr: Expr<A>) -> Result<ast::Params<A>> {
        match expr {
            Expr::Prime(ast::PrimeExpr::Variable(symbol)) => {
                let symbol = self.symbol_table.reparse_function_param(symbol);
                let mut res = Vec::new_in(self.alloc.clone());
                res.push(symbol);
                Ok(ast::Params(res, None))
            }
            Expr::Prime(ast::PrimeExpr::Covered(exprs)) => {
                let mut res = Vec::new_in(self.alloc.clone());
                for e in exprs {
                    if let Expr::Prime(ast::PrimeExpr::Variable(s)) = e {
                        res.push(self.symbol_table.reparse_function_param(s));
                    } else {
                        unexpected!(self => "could not parse left hand side as arrow function parameters");
                    }
                }
                Ok(ast::Params(res, None))
            }
            Expr::Prime(ast::PrimeExpr::ArrowArgs(x)) => Ok(x),
            _ => {
                unexpected!(self => "could not parse left hand side as arrow function parameters");
            }
        }
    }
}
