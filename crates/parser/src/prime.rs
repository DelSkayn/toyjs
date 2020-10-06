use super::*;
use ast::{Literal, PrimeExpr};
use token::t;

impl<'a, 'b> Parser<'a, 'b> {
    pub(crate) fn parse_prime_expr(&mut self) -> Result<PrimeExpr<'b>> {
        let peek = match self.peek_kind()? {
            Some(x) => x,
            None => unexpected!(self => "expected expression"),
        };
        match peek {
            t!("(") => {
                self.next()?;
                // Covered expression, or arrow function
                let expr = self.parse_expr()?;
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
            TokenKind::Ident(x) => {
                self.next()?;
                let var = self.variables.use_variable(x);
                Ok(PrimeExpr::Variable(var))
            }
            t!("{") => self.parse_object(),
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
            x => to_do!(self, x),
        }
    }

    pub(crate) fn parse_object(&mut self) -> Result<PrimeExpr<'b>> {
        expect!(self, "{");
        let mut exprs = Vec::new_in(self.bump);
        if self.peek_kind()? != Some(t!("}")) {
            loop {
                expect_bind!(self, let bind = "ident");
                expect!(self, ":");
                let expr = self.parse_single_expr()?;
                exprs.push((bind, expr));
                if !self.eat(t!(","))? {
                    break;
                }
            }
        }
        expect!(self,"}" => "expected object to end here, missing comma?");
        Ok(PrimeExpr::Object(exprs))
    }
}
