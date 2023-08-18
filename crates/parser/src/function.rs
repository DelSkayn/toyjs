use std::mem;

use ast::{Function, FunctionKind, ListHead, NodeId, Stmt};
use token::{t, TokenKind};

use crate::{alter_state, expect, peek_expect, unexpected, Parser, ParserState, Result};

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
    /// Parses the parameters and body of a function:
    /// ```javascript
    /// // function declaration.
    /// function foo /* starts here */(a,b){
    ///     return a + b
    /// }
    /// ({
    ///     // member
    ///     foo /* starts here */(a,b){
    ///         return a + b
    ///     }
    /// })
    /// ```
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

        alter_state!(self => {
            self.state.set(ParserState::AwaitIdent,
                           matches!(kind,FunctionKind::Simple | FunctionKind::Generator));
            self.state.set(ParserState::YieldIdent,
                           matches!(kind,FunctionKind::Simple | FunctionKind::Async));
            let body = self.parse_function_body()?;
        });

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

    /// Parses the parameters and body of a getter method:
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

    /// Parses the parameters and body of a setter method:
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

    /// Parses the body of a function:
    /// function foo() /* start here */ {
    ///     // some body
    /// }
    pub fn parse_function_body(&mut self) -> Result<FunctionBody> {
        expect!(self, "{");
        let state = self.state;

        let mut head = ListHead::Empty;
        let mut prev = None;

        loop {
            if let t!("}") = peek_expect!(self, "}").kind() {
                self.next();
                break;
            }

            let stmt = self.parse_stmt()?;
            if head.is_empty()
                && !self.state.contains(ParserState::Strict)
                && self.is_strict_directive(stmt)
            {
                self.state.insert(ParserState::Strict);
            }
            prev = Some(self.ast.append_list(stmt, prev));
            head = head.or(prev.into());
        }
        let is_strict = self.state.contains(ParserState::Strict);
        self.state = state;
        Ok(FunctionBody {
            body: head,
            is_strict,
        })
    }
}
