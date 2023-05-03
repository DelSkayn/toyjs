use ast::{NodeId, Stmt, VariableKind};
use token::t;

use crate::{expect, next_expect, unexpected, Parser, Result};

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Result<NodeId<Stmt>> {
        let Some(token) = self.peek() else {
            return Ok(self.ast.push_node(Stmt::Empty));
        };
        let expr = match token.kind() {
            t!("{") => self.parse_block_stmt()?,
            t!("var") => self.parse_variable_decl(VariableKind::Var)?,
            t!("let") => self.parse_variable_decl(VariableKind::Let)?,
            t!("const") => self.parse_variable_decl(VariableKind::Const)?,
            t!("if") => self.parse_if_stmt()?,
            t!("while") => self.parse_while_stmt()?,
            t!("do") => self.parse_do_while_stmt()?,
            t!("for") => self.parse_for_stmt()?,
            t!("switch") => self.parse_for_stmt()?,
            t!("break") => self.parse_cntrl_flow_stmt(true)?,
            t!("continue") => self.parse_cntrl_flow_stmt(false)?,
            t!("with") => {
                if self.state.strict {
                    todo!()
                }
                self.parse_with_stmt()?
            }
            t!(";") => {
                self.next();
                self.ast.push_node(Stmt::Empty)
            }
            _ => todo!(),
        };

        Ok(expr)
    }

    pub fn parse_block_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "{");
        todo!()
    }

    pub fn parse_if_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "if");
        todo!()
    }

    pub fn parse_while_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "while");
        todo!()
    }

    pub fn parse_do_while_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "do");
        todo!()
    }

    pub fn parse_for_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "for");
        todo!()
    }

    pub fn parse_switch_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "switch");
        todo!()
    }

    pub fn parse_cntrl_flow_stmt(&mut self, is_break: bool) -> Result<NodeId<Stmt>> {
        if is_break {
            expect!(self, "break");
        } else {
            expect!(self, "continue");
        }
        todo!()
    }

    pub fn parse_with_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "with");
        todo!()
    }

    pub fn parse_variable_decl(&mut self, kind: VariableKind) -> Result<NodeId<Stmt>> {
        expect!(self, "{");
        todo!()
    }
}
