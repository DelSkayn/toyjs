use super::{Allocator, Error, ErrorKind, Parser, Result, TokenKind};
use ast::{
    symbol_table::{DeclType, ScopeKind},
    Expr, Literal, PrimeExpr,
};
use common::atom::Atom;
use token::t;

impl<'a, 'b, A: Allocator + Clone> Parser<'a, 'b, A> {
    pub(crate) fn parse_prime_expr(&mut self) -> Result<PrimeExpr<A>> {
        let peek = match self.peek_kind()? {
            Some(x) => x,
            None => unexpected!(self => "expected expression"),
        };
        match peek {
            t!("(") => {
                self.next()?;
                // Covered expression, or arrow function
                if self.eat(t!(")"))? {
                    if self.peek_kind()? != Some(t!("=>")) {
                        unexpected!(self,"=>" => "expected arrow function declaration");
                    }
                    return Ok(PrimeExpr::ArrowArgs(ast::Params(
                        Vec::new_in(self.alloc.clone()),
                        None,
                    )));
                }
                let expr = match self.parse_expr() {
                    Ok(x) => x,
                    Err(e) => {
                        if let Ok(x) = self.parse_params(true) {
                            return Ok(PrimeExpr::ArrowArgs(x));
                        }
                        if self.peek_kind()? != Some(t!("=>")) {
                            unexpected!(self,"=>" => "expected arrow function declaration");
                        }
                        return Err(e);
                    }
                };
                expect!(self, ")");
                Ok(PrimeExpr::Covered(expr))
            }
            t!("true") => {
                self.next()?;
                Ok(PrimeExpr::Literal(Literal::Boolean(true)))
            }
            t!("false") => {
                self.next()?;
                Ok(PrimeExpr::Literal(Literal::Boolean(false)))
            }
            t!("null") => {
                self.next()?;
                Ok(PrimeExpr::Literal(Literal::Null))
            }
            t!("this") => {
                self.next()?;
                Ok(PrimeExpr::This)
            }
            TokenKind::Ident(x) => {
                self.next()?;

                if x == common::atom::constant::eval {
                    if let Some(x) = self.try_parse_eval()? {
                        return Ok(x);
                    }
                }

                let var = self.symbol_table.use_symbol(x);
                Ok(PrimeExpr::Variable(var))
            }
            t!("{") => self.parse_object(),
            t!("[") => self.parse_array(),
            t!("`..`", x) => {
                self.next()?;

                Ok(PrimeExpr::Template(ast::Template {
                    head: x,
                    subtitutions: Vec::new_in(self.alloc.clone()),
                }))
            }
            t!("`..${", x) => {
                self.next()?;
                self.parse_template(x)
            }
            TokenKind::Literal(x) => {
                self.next()?;
                Ok(match x {
                    token::Literal::String(x) => PrimeExpr::Literal(Literal::String(x)),
                    token::Literal::Number(token::Number::Float(x)) => {
                        PrimeExpr::Literal(Literal::Float(x))
                    }
                    token::Literal::Number(token::Number::Integer(x)) => {
                        PrimeExpr::Literal(Literal::Integer(x))
                    }
                    _ => to_do!(self),
                })
            }
            t!("function") => self.parse_function_expression(),
            x => to_do!(self, x),
        }
    }

    fn try_parse_eval(&mut self) -> Result<Option<PrimeExpr<A>>> {
        if self.peek_kind()? != Some(t!("(")) {
            return Ok(None);
        }
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        Ok(Some(PrimeExpr::Eval(expr)))
    }

    fn parse_template(&mut self, head: Atom) -> Result<PrimeExpr<A>> {
        let mut subtitutions = Vec::new_in(self.alloc.clone());
        let empty = self.lexer.interner.intern("");
        loop {
            let mut exprs = Vec::new_in(self.alloc.clone());
            let token = loop {
                let expr = self.parse_single_expr()?;
                exprs.push(expr);
                let next = self.next()?;
                let next = if let Some(next) = next {
                    next
                } else {
                    unexpected!(self,"}" => "unfinished template")
                };
                if next.kind != t!(",") {
                    break next;
                }
            };
            if token.kind != t!("}") {
                unexpected!(self, "}" => "expected template subtitution to end here");
            }
            let token = self.lexer.relex_template_subsituation(token)?;
            match token.kind {
                t!("}..${", x) => {
                    if x == empty {
                        subtitutions.push((exprs, None))
                    } else {
                        subtitutions.push((exprs, Some(x)))
                    }
                }
                t!("}..`", x) => {
                    if x == empty {
                        subtitutions.push((exprs, None))
                    } else {
                        subtitutions.push((exprs, Some(x)))
                    }
                    break;
                }
                _ => unexpected!(self, "}" => "expected template subtitution to end here"),
            }
        }
        Ok(PrimeExpr::Template(ast::Template { head, subtitutions }))
    }

    pub(crate) fn parse_object(&mut self) -> Result<PrimeExpr<A>> {
        expect!(self, "{");
        let mut exprs = Vec::new_in(self.alloc.clone());
        while self.peek_kind()? != Some(t!("}")) {
            expect_bind!(self, let bind = "ident");
            if self.peek_kind()? == Some(t!("(")) {
                let scope = self.symbol_table.push_scope(ScopeKind::Function);
                let params = self.parse_params(false)?;
                expect!(self, "{");
                let stmts = self.alter_state::<_, _, Result<_>>(
                    |s| {
                        s.r#return = true;
                        s.r#break = false;
                        s.r#continue = false;
                    },
                    |this| {
                        let mut stmts = Vec::new_in(this.alloc.clone());
                        while !this.eat(t!("}"))? {
                            if this.peek()?.is_none() {
                                unexpected!(this, "expected statement or function end");
                            }
                            stmts.push(this.parse_stmt()?);
                        }
                        Ok(stmts)
                    },
                )?;
                self.symbol_table.pop_scope();
                let expr = Expr::Prime(PrimeExpr::Function(scope, None, params, stmts));
                exprs.push((bind, expr));
                continue;
            }
            expect!(self, ":");
            let expr = self.parse_single_expr()?;
            exprs.push((bind, expr));
            if !self.eat(t!(","))? {
                break;
            }
        }
        expect!(self,"}" => "expected object to end here, missing a comma?");
        Ok(PrimeExpr::Object(exprs))
    }

    pub(crate) fn parse_array(&mut self) -> Result<PrimeExpr<A>> {
        expect!(self, "[");
        let mut exprs = Vec::new_in(self.alloc.clone());
        while self.peek_kind()? != Some(t!("]")) {
            let expr = self.parse_single_expr()?;
            exprs.push(expr);
            if !self.eat(t!(","))? {
                break;
            }
        }
        expect!(self,"]" => "expected array to end here, missing a comma?");
        Ok(PrimeExpr::Array(exprs))
    }

    fn parse_function_expression(&mut self) -> Result<PrimeExpr<A>> {
        expect!(self, "function");
        let symbol = self
            .peek()?
            .and_then(|x| {
                if let TokenKind::Ident(x) = x.kind {
                    self.next().unwrap();
                    Some(x)
                } else {
                    None
                }
            })
            .map(|x| {
                self.symbol_table.define(x, DeclType::Var).ok_or(Error {
                    kind: ErrorKind::RedeclaredVariable,
                    origin: self.last_span,
                })
            })
            .transpose()?;
        let scope = self.symbol_table.push_scope(ScopeKind::Function);
        let params = self.parse_params(false)?;
        expect!(self, "{");
        let stmts = self.alter_state::<_, _, Result<_>>(
            |s| {
                s.r#return = true;
                s.r#break = false;
                s.r#continue = false;
            },
            |this| {
                let mut stmts = Vec::new_in(this.alloc.clone());
                while !this.eat(t!("}"))? {
                    if this.peek()?.is_none() {
                        unexpected!(this, "expected statement or function end");
                    }
                    stmts.push(this.parse_stmt()?);
                }
                Ok(stmts)
            },
        )?;

        debug_assert_eq!(scope, self.symbol_table.current_scope());
        self.symbol_table.pop_scope();
        Ok(ast::PrimeExpr::Function(scope, symbol, params, stmts))
    }
}
