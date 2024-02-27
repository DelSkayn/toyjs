use std::collections::BinaryHeap;

use bc::{ Reg};
use common::hashmap::HashMap;

mod use_bitmap;

use self::use_bitmap::UseBitmap;
use crate::variables::{SymbolId, SymbolUseOrder};

// Lookups required:
// - Register -> tmp state
// - Reguster -> used
// - most recent symbol usage -> register
// - Symbolid -> location ->? register

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SymbolAllocation {
    SwappedOut(u32),
    Argument(u32),
    Register(u8),
    Global,
    GlobalTmp(u8),
    Upvalue(u32),
    UpvalueTmp(u8),
}


#[derive(Debug)]
pub struct Registers {
    used: UseBitmap,
    tmp: UseBitmap,
    reservations: BinaryHeap<Reservation>,
    used_symbols: HashMap<SymbolId, SymbolAllocation>,
    recent_reservation: SymbolUseOrder,
    current_use_order: SymbolUseOrder,
    last_used_reg: i32,
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            used: UseBitmap::empty(),
            tmp: UseBitmap::empty(),
            reservation: BinaryHeap<Reservation><
            current_use_order: SymbolUseOrder::first(),
            recent_reservation: SymbolUseOrder::last(),
            last_used_reg: -1,
        }
    }

    pub fn clear(&mut self) {
        self.used = UseBitmap::empty();
        self.last_used_reg = -1;
    }

    fn collect_variables(&mut self) {
        if self.recent_reservation > self.current_use_order {
            return;
        }
        let mut used_by_vars = !(self.used & !self.tmp);
        let mut next_free;
        self.recent_reservation = SymbolUseOrder::last();
        loop {
            next_free = used_by_vars.next_free();
            if next_free == 128 {
                break;
            }
            let reservation = self.reservation[next_free as usize].unwrap();
            if reservation <= self.current_use_order {
                self.reservation[next_free as usize] = None;
                self.used.unset(next_free);
            } else {
                self.recent_reservation = self.recent_reservation.min(reservation);
            }
            used_by_vars.set(next_free)
        }
    }

    pub fn next_free(&mut self) -> Option<Reg> {
        self.collect_variables();
        let free = self.used.next_free();
        if free < 128 {
            let reg = Reg(free as i8);
            self.last_used_reg = self.last_used_reg.max(free as i32);
            Some(reg)
        } else {
            None
        }
    }

    pub fn last_used(&self) -> i32 {
        self.last_used_reg
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
        self.recent_reservation = self.recent_reservation.min(until);
        self.symbol_allocation[reg as usize] = Some(symbol);
    }

    pub fn alloc_symbol(&mut self, symbol: SymbolId, until: SymbolUseOrder) -> Option<Reg> {
        let reg = self.next_free()?;
        self.reserve_until(reg, symbol, until);
        Some(reg)
    }

    pub fn find_symbol(&self, symbol: SymbolId) -> Option<Reg> {
        for i in 0..=self.last_used_reg {
            // TODO: Test reg properly.
            if Some(symbol) == self.symbol_allocation[i as usize] {
                return Some(Reg(i as i8));
            }
        }
        None
    }

    pub fn advance_usage(&mut self, order: SymbolUseOrder) {
        self.current_use_order = order;
    }
}
