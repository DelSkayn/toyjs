use super::{PResult, Parser};
use crate::{
    ast::*,
    lexer::Lexer,
    token::{DelimToken, Kw, Span, Token, TokenKind, UnaryOpToken},
};

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> PResult<'a, Expr<'a>> {
        to_do!(self);
    }

    pub fn parse_assigment_expr(&mut self) -> PResult<'a, Expr<'a>> {
        self.parse_lhs_expr()
    }

    pub fn parse_member_expr(&mut self) -> PResult<'a, MemberExpr<'a>> {
        let peek = self.peek();
        if peek.is_none() {
            unexpected_end!(self);
        }
        let peek = self.peek().unwrap();

        let mut root = match peek.kind {
            tok!("new") => {
                self.next();
                if is!(self, ".") {
                    expect!(self, "target");
                    MemberExpr::NewTarget
                } else {
                    return self.parse_member_new_expr();
                }
            }
            tok!("import") => {
                expect!(self, ".");
                expect!(self, "meta");
                MemberExpr::ImportMeta
            }
            tok!("super") => {
                if eat!(self, "[") {
                    let expr = self.parse_expr()?;
                    MemberExpr::SuperIndex { expr }
                } else if eat!(self, ".") {
                    MemberExpr::SuperDot
                } else {
                    unexpected!(self, "[", ".");
                }
            }
            _ => to_do!(self),
        };
        let mut rest = self.parse_member_path_expr()?;
        for x in rest.drain(..) {
            root = MemberExpr::In {
                left: Box::new(root),
                right: Box::new(x),
            };
        }
        Ok(root)
    }

    pub fn parse_member_new_expr(&mut self) -> PResult<'a, MemberExpr<'a>> {
        to_do!(self)
    }

    pub fn parse_member_path_expr(&mut self) -> PResult<'a, Vec<MemberExpr<'a>>> {
        let mut exprs = Vec::new();
        while let Ok(x) = self.parse_primary_expr() {
            exprs.push(MemberExpr::Prime { kind: x });
            let brack_span = self.cur_span();
            if eat!(self, "[") {
                let expr = self.parse_expr()?;
                exprs.push(MemberExpr::Index { expr: expr });
                close_delim!(self, brack_span, "]")
            }
            if eat!(self, ".") {
                exprs.push(MemberExpr::Dot)
            }
        }
        Ok(exprs)
    }

    pub fn parse_primary_expr(&mut self) -> PResult<'a, PrimeExpr<'a>> {
        if self.is_lit() {
            return Ok(PrimeExpr::Literal {
                token: self.next().unwrap(),
            });
        }
        if is_any!(self, "true", "false") {
            return Ok(PrimeExpr::Literal {
                token: self.next().unwrap(),
            });
        }
        if is!(self, "[") {
            return self.parse_array_lit();
        }
        if is!(self, "{") {
            return self.parse_object_lit();
        }
        if is!(self, "function") {
            return self.parse_function_expr();
        }
        if is!(self, "class") {
            return self.parse_class_expr();
        }
        if self.is_ident() {
            return Ok(PrimeExpr::Ident {
                token: self.next().unwrap(),
            });
        }
        unexpected!(
            self,
            "identifier",
            "literal",
            "true",
            "false",
            "[",
            "{",
            "function",
            "class"
        )
    }

    pub fn parse_function_expr(&mut self) -> PResult<'a, PrimeExpr<'a>> {
        to_do!(self);
    }

    pub fn parse_class_expr(&mut self) -> PResult<'a, PrimeExpr<'a>> {
        to_do!(self);
    }

    pub fn parse_cond_expr(&mut self) -> PResult<'a, Expr<'a>> {
        to_do!(self);
    }

    pub fn parse_bin_expr(&mut self) -> PResult<'a, Expr<'a>> {
        to_do!(self);
    }

    pub fn parse_unary_expr(&mut self) -> PResult<'a, Expr<'a>> {
        let peek = self.peek();
        if peek.is_none() {
            unexpected_end!(self)
        }
        let peek = peek.unwrap();
        let op = match_tok!((peek) {
            "++" => Some(UnaryOpToken::AddOne),
            "--" => Some(UnaryOpToken::AddOne),
            _ => None,
        });
        if let Some(op) = op {
            self.next();

            let arg = self.parse_unary_expr()?;
            return Ok(Expr::Unary {
                kind: op,
                prefix: true,
                arg: Box::new(arg),
            });
        }

        let op = match peek.kind {
            tok!("delete") => Some(UnaryOpToken::Delete),
            tok!("void") => Some(UnaryOpToken::Void),
            tok!("typeof") => Some(UnaryOpToken::Typeof),
            tok!("+") => Some(UnaryOpToken::Positive),
            tok!("-") => Some(UnaryOpToken::Negative),
            tok!("~") => Some(UnaryOpToken::BitwiseNot),
            tok!("!") => Some(UnaryOpToken::Not),
            _ => None,
        };
        if let Some(op) = op {
            self.next();
            let arg = self.parse_unary_expr()?;
            return Ok(Expr::Unary {
                prefix: true,
                kind: op,
                arg: Box::new(arg),
            });
        }

        let arg = self.parse_lhs_expr()?;

        let op = match self.peek_with_lt().map(|e| e.kind) {
            Some(tok!("++")) => Some(UnaryOpToken::AddOne),
            Some(tok!("--")) => Some(UnaryOpToken::SubractOne),
            _ => None,
        };

        if let Some(op) = op {
            return Ok(Expr::Unary {
                prefix: false,
                kind: op,
                arg: Box::new(arg),
            });
        }

        return Ok(arg);
    }

    pub fn parse_lhs_expr(&mut self) -> PResult<'a, Expr<'a>> {
        Ok(Expr::Lhs {
            expr: Box::new(self.parse_member_expr()?),
        })
    }
}
