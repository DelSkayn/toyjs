use std::mem;

use ast::{Function, ListHead, NodeId};
use token::t;

use crate::{expect, peek_expect, unexpected, Parser, Result};

pub struct Parameter {}

impl<'a> Parser<'a> {
    pub fn parse_parameters(&mut self) -> Result<ListHead<Parameter>> {
        loop {
            let next = peek_expect!(self, ")");
            match next.kind() {
                t!("...") => {
                    // Rest binding
                    expect!(self,")" => "rest parameter must be last");
                }
                t!("ident") | t!("{") | t!("[") => {
                    // Binding
                }
                x => unexpected!(self, x, "ident", "{", "[", "..."),
            }
            if !self.eat(t!(",")) {
                expect!(self,")" => "expected parameter to end");
            }
        }
    }

    pub fn parse_function(&mut self, allow_nameless: bool) -> Result<NodeId<Function>> {
        let token = peek_expect!(self, "ident", "(");
        let name = if let t!("(") = token.kind() {
            if !allow_nameless {
                unexpected!(self,token.kind(),"ident" => "function statement must have a name")
            }
            None
        } else {
            Some(self.parse_ident()?)
        };

        expect!(self, "(");
        let mut param_head = ListHead::Empty;
        let mut param_prev = None;
        let mut rest_param = None;
        loop {
            let next = peek_expect!(self, ")");
            match next.kind() {
                t!(")") => {
                    self.next();
                    break;
                }
                t!("...") => {
                    self.next();
                    rest_param = Some(self.parse_ident_or_pattern()?);
                    expect!(self, ")");
                    break;
                }
                _ => {
                    let elem = self.parse_binding_element()?;
                    let elem = self.ast.push_node(elem);
                    param_prev = Some(self.ast.append_list(elem, param_prev));
                    param_head = param_head.or(param_prev.into());
                    if !self.eat(t!(",")) {
                        expect!(self, ")");
                        break;
                    }
                }
            }
        }

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

        let function = self.ast.push_node(Function::Base {
            name,
            params: param_head,
            rest_param,
            body: head,
            strict,
        });
        Ok(function)
    }
}
