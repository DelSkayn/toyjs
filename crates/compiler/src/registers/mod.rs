use bc::{limits::MAX_REGISTERS, Reg};

use crate::{
    variables::{SymbolUseOrder, Variables},
    Error, Limits, Result,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RegisterState {
    Free,
    Tmp,
    Symbol(SymbolUseOrder),
}

pub struct Registers {
    registers_used: u128,
    reservation: [SymbolUseOrder; MAX_REGISTERS],
    variables: Variables,
    max_used: Reg,
    time: SymbolUseOrder,
    earliest_reservation: SymbolUseOrder,
}

impl Registers {
    pub fn new(variables: Variables) -> Self {
        Registers {
            registers_used: 0u128,
            reservation: [SymbolUseOrder(0); MAX_REGISTERS],
            variables,
            max_used: Reg::this_reg(),
            time: SymbolUseOrder::first(),
            earliest_reservation: SymbolUseOrder::last(),
        }
    }

    pub fn clear(&mut self) {
        self.registers_used = 0;
        self.max_used = Reg::this_reg()
    }

    pub fn advance(&mut self, new_use: SymbolUseOrder) {
        debug_assert!(self.time <= new_use);
        self.time = new_use;
    }

    /// Evict registers which have outlifed there reservation.
    pub fn evict(&mut self) {
        if self.earliest_reservation > self.time {
            // no eviction required.
            return;
        }

        self.earliest_reservation = SymbolUseOrder::last();
        let mut register_map = self.registers_used;
        // a bit scan, might actually be slower then a normal loop.
        // TODO: profile
        loop {
            let idx = register_map.leading_zeros();
            let cur = self.reservation[idx as usize];
            if cur <= self.time {
                self.registers_used &= !(1 << idx);
            } else {
                self.earliest_reservation = cur.min(self.earliest_reservation);
            }
            register_map &= register_map - 1;
            if register_map == 0 {
                break;
            }
        }
    }

    pub fn next_free(&mut self) -> Result<Reg> {
        self.evict();

        let available = self.registers_used.leading_ones();
        if available == 128 {
            return Err(Error::ExceededLimits(Limits::Registers));
        }
        Ok(Reg(available as i8))
    }

    pub fn reserve_tmp(&mut self, reg: Reg) {
        debug_assert_eq!(self.registers_used & (1 << reg.0), 0);
        self.registers_used |= 1 << reg.0;
        self.reservation[reg.0 as usize] = SymbolUseOrder::last();
    }

    pub fn alloc_tmp(&mut self) -> Result<Reg> {
        let reg = self.next_free()?;
        self.reserve_tmp(reg);
        Ok(reg)
    }

    pub fn free_tmp(&mut self, reg: Reg) {
        debug_assert_eq!(self.reservation[reg.0 as usize], SymbolUseOrder::last());
        self.registers_used &= !(1 << reg.0);
    }

    pub fn reserve_until(&mut self, reg: Reg, until: SymbolUseOrder) {
        debug_assert_eq!(self.registers_used & (1 << reg.0), 0);
        self.registers_used |= 1 << reg.0;
        self.reservation[reg.0 as usize] = until;
    }

    pub fn alloc_until(&mut self, until: SymbolUseOrder) -> Result<Reg> {
        let reg = self.next_free()?;
        self.reserve_until(reg, until);
        Ok(reg)
    }
}
