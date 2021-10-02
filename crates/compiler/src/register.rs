//! Module containing functionality for managing register allocations.

use ast::SymbolId;

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Register(pub u8);

#[derive(Clone, Copy, Eq, PartialEq)]
enum AllocValue {
    Free,
    Temp,
    Symbol(SymbolId),
}

pub struct Registers {
    registers: [AllocValue; 255],
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            registers: [AllocValue::Free; 255],
        }
    }

    pub fn alloc_temp(&mut self) -> Register {
        let reg = self
            .registers
            .iter()
            .copied()
            .enumerate()
            .find(|x| x.1 == AllocValue::Free)
            .expect("could not find free register")
            .0;

        self.registers[reg] = AllocValue::Temp;
        Register(reg as u8)
    }

    pub fn free_temp(&mut self, register: Register) {
        self.registers[register.0 as usize] = AllocValue::Free;
    }

    pub fn alloc_symbol(&mut self, symbol: SymbolId) -> Register {
        let mut free = None;
        for (idx, r) in self.registers.iter().enumerate() {
            if *r == AllocValue::Symbol(symbol) {
                return Register(idx as u8);
            }
            if *r == AllocValue::Free && free.is_none() {
                free = Some(idx);
            }
        }
        let free = free.expect("could not find free register");
        self.registers[free] = AllocValue::Symbol(symbol);
        Register(free as u8)
    }
}
