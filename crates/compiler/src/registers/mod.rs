use std::num::NonZeroU32;

use crate::Result;
use ast::NodeId;
use bc::{limits::MAX_REGISTERS, FarReg, Reg};
use common::string::StringId;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RegisterState {
    Free,
    Tmp { valid: NodeId<ast::Expr> },
}

pub struct Registers {
    state: [RegisterState; MAX_REGISTERS],
    max_used: u8,
}

/// An enum which encodes where an expression should be placed.
pub enum Placement {
    // The expression is a intermediate result and muste be valid until atleast the given id.
    Temporary(NodeId<ast::Expr>),
    // The expression is a new value for a variable, it must be stored in the variable.
    Variable(()),
}

pub enum Read {
    // The value is available in register.
    Available(Reg),
    // The value must be popped into the given register by emitting times pop instructions into the
    // to register.
    Pop { to: Reg, times: NonZeroU32 },
    // The value is spilled and not at the top of the stack,
    Far { from: FarReg, to: Reg },
}

pub enum Write {
    // A register is readily available
    Available(Reg),
    // A register must be spilled to make place.
    Spil(Reg),
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            state: [RegisterState::Free; MAX_REGISTERS],
            max_used: 0,
        }
    }

    pub fn read(&mut self, ident: StringId, expr: NodeId<ast::Expr>) -> Result<Read> {
        to_do!()
    }

    pub fn store(&mut self, ident: String, value: NodeId<ast::Expr>) -> Result<Write> {
        to_do!()
    }

    pub fn dst(&mut self, expr: NodeId<ast::Expr>) -> Result<Reg> {
        to_do!()
    }

    pub fn alloc_tmp(
        &mut self,
        current: NodeId<ast::Expr>,
        valid: NodeId<ast::Expr>,
    ) -> Result<Reg> {
        let free = self.state.iter().position(|x| match x {
            RegisterState::Free => true,
            RegisterState::Tmp { valid } => *valid < current,
        });

        if let Some(free) = free {
            self.state[free] = RegisterState::Tmp { valid };
            let free = free as i8;
            self.max_used = self.max_used.max(free as u8);
            return Ok(Reg(free));
        }

        to_do!()
    }

    pub fn free(&mut self, reg: Reg) {
        debug_assert_ne!(self.state[reg.0 as usize], RegisterState::Free);
        self.state[reg.0 as usize] = RegisterState::Free;
    }
}