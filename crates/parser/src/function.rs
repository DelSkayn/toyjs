use std::mem;

use ast::{Function, FunctionKind, ListHead, NodeId, Stmt};
use token::{t, TokenKind};

use crate::{expect, peek_expect, unexpected, Parser, Result};

pub struct FunctionBody {
    pub body: ListHead<Stmt>,
    pub is_strict: bool,
}

pub enum FunctionCtx {
    Stmt,
    Expression,
    Method,
}

impl<'a> Parser<'a> {
    pub fn parse_function(
        &mut self,
        ctx: FunctionCtx,
        kind: FunctionKind,
    ) -> Result<NodeId<Function>> {
        let name = match ctx {
            FunctionCtx::Stmt => Some(self.parse_ident()?),
            FunctionCtx::Expression => {
                let token = peek_expect!(self, "ident");
                match token.kind() {
                    TokenKind::UnreservedKeyword(_) | TokenKind::Ident => Some(self.parse_ident()?),
                    t!("(") => None,
                    _ => {
                        unexpected!(self, token.kind(), "ident");
                    }
                }
            }
            FunctionCtx::Method => None,
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

        let await_ident = self.state.await_ident;
        self.state.await_ident = matches!(kind, FunctionKind::Simple | FunctionKind::Generator);
        let body = self.parse_function_body()?;
        self.state.await_ident = await_ident;

        let function = self.ast.push_node(Function::Base {
            name,
            kind,
            params: param_head,
            rest_param,
            body: body.body,
            is_strict: body.is_strict,
        });
        Ok(function)
    }

    pub fn parse_getter(&mut self) -> Result<NodeId<Function>> {
        expect!(self,"(" => "expected start of getter parameters");
        expect!(self,")" => "getter can't have any parameters");
        let body = self.parse_function_body()?;
        Ok(self.ast.push_node(Function::Base {
            is_strict: body.is_strict,
            kind: FunctionKind::Simple,
            name: None,
            params: ListHead::Empty,
            rest_param: None,
            body: body.body,
        }))
    }

    pub fn parse_setter(&mut self) -> Result<NodeId<Function>> {
        expect!(self,"(" => "expected start of setter parameters");
        let param = self.parse_binding_element()?;
        let param = self.ast.push_node(param);
        let params = self.ast.append_list(param, None);
        expect!(self,")" => "setter must have a single parameter");

        let body = self.parse_function_body()?;
        Ok(self.ast.push_node(Function::Base {
            is_strict: body.is_strict,
            kind: FunctionKind::Simple,
            name: None,
            params: ListHead::Present(params),
            rest_param: None,
            body: body.body,
        }))
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
