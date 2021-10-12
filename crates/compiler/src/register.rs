//! Module containing functionality for managing register allocations.

use ast::SymbolId;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Register(pub u8);

#[derive(Clone, Copy, Eq, PartialEq)]
enum AllocValue {
    Free,
    Temp,
    Symbol(SymbolId),
    Global,
    Env(u8),
    Arg(SymbolId),
}

impl AllocValue {
    pub fn is_arg(&self) -> bool {
        match *self {
            AllocValue::Arg(_) => true,
            _ => false,
        }
    }
}

pub struct Registers {
    registers: [AllocValue; 255],
    cur_allocated: u8,
    max_allocated: u8,
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            registers: [AllocValue::Free; 255],
            cur_allocated: 0,
            max_allocated: 0,
        }
    }

    pub fn clear(&mut self) {
        for i in 0..self.max_allocated as usize {
            self.registers[i] = AllocValue::Free;
        }
        self.cur_allocated = 0;
        self.max_allocated = 0;
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

        self.cur_allocated += 1;
        self.max_allocated = self.max_allocated.max(self.cur_allocated);

        self.registers[reg] = AllocValue::Temp;
        Register(reg as u8)
    }

    pub fn free_temp(&mut self, register: Register) {
        if self.registers[register.0 as usize] == AllocValue::Temp {
            self.registers[register.0 as usize] = AllocValue::Free;
            self.cur_allocated -= 1;
        }
    }

    pub fn alloc_symbol(&mut self, symbol: SymbolId) -> Register {
        let mut free = None;
        for (idx, r) in self.registers.iter().enumerate() {
            if idx > self.max_allocated as usize {
                break;
            }
            if *r == AllocValue::Symbol(symbol) {
                return Register(idx as u8);
            }
            if *r == AllocValue::Arg(symbol) {
                return Register(idx as u8);
            }
            if *r == AllocValue::Free && free.is_none() {
                free = Some(idx);
            }
        }
        let free = free.expect("could not find free register");
        self.registers[free] = AllocValue::Symbol(symbol);
        self.cur_allocated += 1;
        self.max_allocated = self.max_allocated.max(self.cur_allocated);
        Register(free as u8)
    }

    pub fn alloc_arg(&mut self, symbol: SymbolId) -> Register {
        debug_assert!(
            self.cur_allocated == 0 || self.registers[self.cur_allocated as usize - 1].is_arg()
        );
        let res = Register(self.cur_allocated);
        self.registers[self.cur_allocated as usize] = AllocValue::Arg(symbol);
        self.cur_allocated += 1;
        self.max_allocated += 1;
        res
    }

    pub fn registers_needed(&self) -> u8 {
        self.max_allocated
    }
}
