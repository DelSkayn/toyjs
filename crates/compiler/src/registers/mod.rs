use bc::{limits::MAX_REGISTERS, Reg};
use common::hashmap::HashMap;

mod use_bitmap;

use self::use_bitmap::UseBitmap;
use crate::variables::{SymbolId, SymbolUseOrder};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RegisterState {
    Free,
    Tmp,
    Symbol(SymbolUseOrder),
}

#[derive(Debug)]
pub struct Registers {
    used: UseBitmap,
    tmp: UseBitmap,
    reservation: [Option<SymbolUseOrder>; MAX_REGISTERS],
    symbol_allocation: [Option<SymbolId>; MAX_REGISTERS],
    last_used_reg: Reg,
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            used: UseBitmap::empty(),
            tmp: UseBitmap::empty(),
            reservation: [None; MAX_REGISTERS],
            symbol_allocation: [None; MAX_REGISTERS],
            last_used_reg: Reg::this_reg(),
        }
    }

    pub fn clear(&mut self) {
        self.used = UseBitmap::empty();
        self.last_used_reg = Reg::this_reg();
    }

    pub fn next_free(&mut self) -> Option<Reg> {
        let free = self.used.next_free();
        if free < 128 {
            let reg = Reg(free as i8);
            self.last_used_reg = self.last_used_reg.max(reg);
            Some(reg)
        } else {
            None
        }
    }

    pub fn reserve_tmp(&mut self, reg: Reg) {
        let reg = u8::try_from(reg.0).unwrap();
        debug_assert!(!self.used.get(reg));
        debug_assert!(!self.tmp.get(reg));
        self.used.set(reg);
        self.tmp.set(reg);
    }

    pub fn alloc_tmp(&mut self) -> Option<Reg> {
        let reg = self.next_free()?;
        self.reserve_tmp(reg);
        Some(reg)
    }

    pub fn free_if_tmp(&mut self, reg: Reg) {
        let reg = u8::try_from(reg.0).unwrap();
        if !self.tmp.get(reg) {
            return;
        }
        debug_assert!(self.used.get(reg));
        self.used.unset(reg);
        self.tmp.unset(reg);
    }

    pub fn free_tmp(&mut self, reg: Reg) {
        let reg = u8::try_from(reg.0).unwrap();
        debug_assert!(self.used.get(reg));
        debug_assert!(self.tmp.get(reg));
        self.used.unset(reg);
        self.tmp.unset(reg);
    }

    pub fn reserve_until(&mut self, reg: Reg, symbol: SymbolId, until: SymbolUseOrder) {
        let reg = u8::try_from(reg.0).unwrap();
        debug_assert!(!self.used.get(reg));
        self.used.set(reg);
        self.reservation[reg as usize] = Some(until);
        self.symbol_allocation[reg as usize] = Some(symbol);
    }

    pub fn alloc_symbol(&mut self, symbol: SymbolId, until: SymbolUseOrder) -> Option<Reg> {
        let reg = self.next_free()?;
        self.reserve_until(reg, symbol, until);
        Some(reg)
    }

    pub fn find_symbol(&self, symbol: SymbolId) -> Option<Reg> {
        for i in 0..=self.last_used_reg.0 {
            if Some(symbol) == self.symbol_allocation[i as usize] {
                return Some(Reg(i));
            }
        }
        None
    }
}
