use std::mem;

use ast::{Function, ListHead, NodeId, Stmt};
use token::{t, TokenKind};

use crate::{expect, peek_expect, unexpected, Parser, Result};

pub struct FunctionBody {
    pub body: ListHead<Stmt>,
    pub is_strict: bool,
}

pub enum FunctionKind {
    Stmt,
    Expression,
    Method,
}

impl<'a> Parser<'a> {
    pub fn parse_function(&mut self, kind: FunctionKind) -> Result<NodeId<Function>> {
        let name = match kind {
            FunctionKind::Stmt => Some(self.parse_ident()?),
            FunctionKind::Expression => {
                let token = peek_expect!(self, "ident");
                match token.kind() {
                    TokenKind::UnreservedKeyword(_) | TokenKind::Ident => Some(self.parse_ident()?),
                    t!("(") => None,
                    _ => {
                        unexpected!(self, token.kind(), "ident");
                    }
                }
            }
            FunctionKind::Method => None,
        };

        expect!(self, "(");
        let mut param_head = ListHead::Empty;
        let mut param_prev = None;
        let mut rest_param = None;
        loop {
            let next = peek_expect!(self, ")");
            match next.kind() {
                t!(")") => {
                    break;
                }
                t!("...") => {
                    self.next();
                    rest_param = Some(self.parse_ident_or_pattern()?);
                    break;
                }
                _ => {
                    let elem = self.parse_binding_element()?;
                    let elem = self.ast.push_node(elem);
                    param_prev = Some(self.ast.append_list(elem, param_prev));
                    param_head = param_head.or(param_prev.into());
                    if !self.eat(t!(",")) {
                        break;
                    }
                }
            }
        }
        expect!(self, ")");

        let body = self.parse_function_body()?;

        let function = self.ast.push_node(Function::Base {
            name,
            params: param_head,
            rest_param,
            body: body.body,
            is_strict: body.is_strict,
        });
        Ok(function)
    }

    pub fn parse_function_body(&mut self) -> Result<FunctionBody> {
        expect!(self, "{");
        let mut strict = self.state.strict;

        let mut head = ListHead::Empty;
        let mut prev = None;

        loop {
            if let t!("}") = peek_expect!(self, "}").kind() {
                self.next();
                break;
            }

            let stmt = self.parse_stmt()?;
            if head.is_empty() && !self.state.strict {
                self.state.strict = self.is_strict_directive(stmt);
            }
            prev = Some(self.ast.append_list(stmt, prev));
            head = head.or(prev.into());
        }
        mem::swap(&mut strict, &mut self.state.strict);
        Ok(FunctionBody {
            body: head,
            is_strict: strict,
        })
    }
}
