use std::num::NonZeroU32;

use crate::{
    variables::{SymbolUseOrder, Variables},
    Limits, Result,
};
use ast::NodeId;
use bc::{limits::MAX_REGISTERS, FarReg, Reg};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RegisterState {
    Free,
    Tmp,
    Symbol(SymbolUseOrder),
}

pub struct Registers {
    state: [RegisterState; MAX_REGISTERS],
    variables: Variables,
    max_used: Reg,
    last_use: SymbolUseOrder,
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
    pub fn new(variables: Variables) -> Self {
        Registers {
            state: [RegisterState::Free; MAX_REGISTERS],
            variables,
            max_used: Reg::this_reg(),
            last_use: SymbolUseOrder(0),
        }
    }

    pub fn clear(&mut self) {
        self.state.fill(RegisterState::Free);
        self.max_used = Reg::this_reg()
    }

    pub fn alloc_tmp(&mut self) -> Result<Reg> {
        let free = self.state.iter().position(|x| match *x {
            RegisterState::Free => true,
            RegisterState::Tmp => false,
            RegisterState::Symbol(x) => x < self.last_use,
        });

        if let Some(free) = free {
            self.state[free] = RegisterState::Tmp;
            let free = free as i8;
            self.max_used = self.max_used.max(Reg(free));
            return Ok(Reg(free));
        }

        Err(crate::Error::ExceededLimits(Limits::Registers))
    }

    pub fn alloc_symbol(&mut self, symbol: NodeId<ast::Symbol>) -> Result<Reg> {
        let free = self.state.iter().position(|x| match *x {
            RegisterState::Free => true,
            RegisterState::Tmp => false,
            RegisterState::Symbol(x) => x < self.last_use,
        });

        let symbol_id = self.variables.use_to_symbol[symbol].id.unwrap();
        //self.variables.symbols[symbol_id].last_use
        //
        to_do!()

        /*
        if let Some(free) = free {
            self.state[free] = RegisterState::Symbol(until);
            let free = free as i8;
            self.max_used = self.max_used.max(Reg(free));
            return Ok(Reg(free));
        }

        Err(crate::Error::ExceededLimits(Limits::Registers))
            */
    }

    pub fn free_tmp(&mut self, reg: Reg) {
        if self.state[reg.0 as usize] == RegisterState::Tmp {
            self.state[reg.0 as usize] = RegisterState::Free;
        }
    }

    pub fn free(&mut self, reg: Reg) {
        debug_assert_ne!(self.state[reg.0 as usize], RegisterState::Free);
        self.state[reg.0 as usize] = RegisterState::Free;
    }
}
