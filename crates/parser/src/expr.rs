use ast::{
    AssignOp, BaseOp, BinaryOp, Expr, ListHead, ListId, NodeId, PostfixOp, PrefixOp, PrimeExpr,
};
use token::{t, TokenKind};

use crate::{expect, next_expect, peek_expect, unexpected, Parser, Result};

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

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Result<ListId<Expr>> {
        let item = self.parse_assignment_expr()?;

        let res = self.ast.append_list(item, None);
        let mut expr = res;
        while self.eat(t!(",")) {
            let item = self.parse_assignment_expr()?;
            expr = self.ast.append_list(item, Some(expr));
        }
        Ok(res)
    }

    pub(crate) fn parse_assignment_expr(&mut self) -> Result<NodeId<Expr>> {
        let res = self.pratt_parse_expr(0)?;

        if self.eat(t!("=>")) {
            todo!("arrow function")
        }
        Ok(res)
    }

    fn parse_prefix_op(&mut self, r_bp: u8) -> Result<NodeId<Expr>> {
        let token = self
            .next()
            .expect("`parse_prefix_op` should only be called if next token is present");

        let operator = match token.kind() {
            t!("delete") => PrefixOp::Delete,
            t!("void") => PrefixOp::Void,
            t!("typeof") => PrefixOp::TypeOf,
            t!("new") => {
                if self.eat(t!(".")) {
                    let token = next_expect!(self, "target");
                    if token.kind() != t!("target") {
                        unexpected!(self, token.kind(), "target");
                    }
                    let expr = self.ast.push_node(PrimeExpr::NewTarget);
                    let expr = self.ast.push_node(Expr::Prime { expr });
                    return Ok(expr);
                }
                PrefixOp::New
            }
            t!("+") => PrefixOp::Plus,
            t!("-") => PrefixOp::Minus,
            t!("!") => PrefixOp::Not,
            t!("~") => PrefixOp::BitwiseNot,
            t!("++") => PrefixOp::AddOne,
            t!("--") => PrefixOp::SubOne,
            _ => {
                panic!("`parse_prefix_op` should only be called when the next token is a operator")
            }
        };

        let expr = self.pratt_parse_expr(r_bp)?;
        Ok(self.ast.push_node(Expr::Prefix { op: operator, expr }))
    }

    fn parse_postfix_op(&mut self, _l_bp: u8, lhs: NodeId<Expr>) -> Result<NodeId<Expr>> {
        let token = self
            .next()
            .expect("`parse_postfix_op` should only be called if next token is present");

        let op = match token.kind() {
            t!("++") => PostfixOp::AddOne,
            t!("--") => PostfixOp::SubOne,
            t!("[") => {
                let index = self.parse_assignment_expr()?;
                expect!(self, "]");
                return Ok(self.ast.push_node(Expr::Index { index, expr: lhs }));
            }
            t!(".") => {
                let ident = self.parse_ident_name()?;
                return Ok(self.ast.push_node(Expr::Dot { ident, expr: lhs }));
            }
            t!("(") => {
                let params = if let t!(")") = peek_expect!(self, ")").kind() {
                    ListHead::Empty
                } else {
                    ListHead::Present(self.parse_expr()?)
                };
                expect!(self, ")");
                return Ok(self.ast.push_node(Expr::Call { params, expr: lhs }));
            }
            x => panic!("`parse_postfix_op` called with not a token {:?}", x),
        };

        Ok(self.ast.push_node(Expr::Postfix { op, expr: lhs }))
    }

    fn parse_infix_op(&mut self, r_bp: u8, lhs: NodeId<Expr>) -> Result<NodeId<Expr>> {
        let token = self
            .next()
            .expect("`parse_postfix_op` should only be called if next token is present");

        let op = match token.kind() {
            t!("=>") => todo!("arrow function"),
            t!("??") => BinaryOp::Base(BaseOp::NullCoalessing),
            t!("?.") => BinaryOp::Base(BaseOp::TenaryNull),
            t!("||") => BinaryOp::Base(BaseOp::Or),
            t!("&&") => BinaryOp::Base(BaseOp::And),
            t!("&") => BinaryOp::Base(BaseOp::BitwiseAnd),
            t!("|") => BinaryOp::Base(BaseOp::BitwiseOr),
            t!("^") => BinaryOp::Base(BaseOp::BitwiseXor),
            t!("+") => BinaryOp::Base(BaseOp::Add),
            t!("-") => BinaryOp::Base(BaseOp::Sub),
            t!("*") => BinaryOp::Base(BaseOp::Mul),
            t!("/") => BinaryOp::Base(BaseOp::Div),
            t!("%") => BinaryOp::Base(BaseOp::Mod),
            t!("**") => BinaryOp::Base(BaseOp::Exp),
            t!("<") => BinaryOp::Base(BaseOp::Less),
            t!("<=") => BinaryOp::Base(BaseOp::LessEqual),
            t!(">") => BinaryOp::Base(BaseOp::Greater),
            t!(">=") => BinaryOp::Base(BaseOp::GreaterEqual),
            t!("<<") => BinaryOp::Base(BaseOp::ShiftLeft),
            t!(">>") => BinaryOp::Base(BaseOp::ShiftRight),
            t!(">>>") => BinaryOp::Base(BaseOp::ShiftRightUnsigned),
            t!("instanceof") => BinaryOp::Base(BaseOp::InstanceOf),
            t!("in") => BinaryOp::Base(BaseOp::In),
            t!("==") => BinaryOp::Base(BaseOp::Equal),
            t!("===") => BinaryOp::Base(BaseOp::StrictEqual),
            t!("!=") => BinaryOp::Base(BaseOp::NotEqual),
            t!("!==") => BinaryOp::Base(BaseOp::StrictNotEqual),
            t!("=") => BinaryOp::Assign(AssignOp::Assign),
            t!("+=") => BinaryOp::Assign(AssignOp::Add),
            t!("-=") => BinaryOp::Assign(AssignOp::Sub),
            t!("*=") => BinaryOp::Assign(AssignOp::Mul),
            t!("/=") => BinaryOp::Assign(AssignOp::Div),
            t!("%=") => BinaryOp::Assign(AssignOp::Mod),
            t!("**=") => BinaryOp::Assign(AssignOp::Exp),
            t!("<<=") => BinaryOp::Assign(AssignOp::ShiftLeft),
            t!(">>=") => BinaryOp::Assign(AssignOp::ShiftRight),
            t!(">>>=") => BinaryOp::Assign(AssignOp::ShiftRightUnsigned),
            t!("&=") => BinaryOp::Assign(AssignOp::BitwiseAnd),
            t!("|=") => BinaryOp::Assign(AssignOp::BitwiseOr),
            t!("^=") => BinaryOp::Assign(AssignOp::BitwiseXor),
            t!("?") => {
                let then = self.parse_assignment_expr()?;
                expect!(self, ":");
                let r#else = self.parse_assignment_expr()?;
                let tenary = self.ast.push_node(ast::Tenary {
                    cond: lhs,
                    then,
                    r#else,
                });
                return Ok(self.ast.push_node(Expr::Tenary(tenary)));
            }
            _ => {
                panic!("`parse_infix_op` called without a infix operator")
            }
        };

        let right = self.pratt_parse_expr(r_bp)?;
        Ok(self.ast.push_node(Expr::Binary {
            op,
            left: lhs,
            right,
        }))
    }

    fn pratt_parse_expr(&mut self, min_bp: u8) -> Result<NodeId<Expr>> {
        let mut lhs = if let Some(((), r_bp)) = self.peek_kind().and_then(prefix_binding_power) {
            self.parse_prefix_op(r_bp)?
        } else {
            let expr = self.parse_prime()?;
            self.ast.push_node(Expr::Prime { expr })
        };

        while let Some(op) = self.peek_kind() {
            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                lhs = self.parse_postfix_op(l_bp, lhs)?;
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if !self.state.r#in && t!("in") == op {
                    break;
                }
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

#[cfg(test)]
mod test {
    use ast::{BaseOp, BinaryOp, Expr, PrimeExpr};

    use crate::create_test_parser;

    #[test]
    fn basic() {
        create_test_parser!("1 + 2", parser);
        let Ok(x) = parser.parse_assignment_expr() else {
            panic!()
        };
        let Expr::Binary { op, left, right } = parser.ast[x] else {
            panic!()
        };
        let BinaryOp::Base(BaseOp::Add) = op else{
            panic!()
        };
        let Expr::Prime{ expr: left } = parser.ast[left] else {
            panic!()
        };
        let Expr::Prime{ expr: right } = parser.ast[right] else {
            panic!()
        };
        let PrimeExpr::Number(left) = parser.ast[left] else {
            panic!()
        };
        let PrimeExpr::Number(right) = parser.ast[right] else {
            panic!()
        };
        let left = parser.lexer.data.numbers[left];
        assert_eq!(left.0, 1.0);
        let right = parser.lexer.data.numbers[right];
        assert_eq!(right.0, 2.0);
    }
}
