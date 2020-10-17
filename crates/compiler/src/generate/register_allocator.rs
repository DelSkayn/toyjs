use crate::ssa::{Ssa, SsaId, SsaVec};
use bumpalo::{collections::Vec, Bump};
use common::{index::Index, newtype_slice};

struct Lifetimes<'alloc>(&'alloc mut [SsaId]);
newtype_slice!(struct Lifetimes<'alloc,>[SsaId] -> SsaId);

pub struct RegisterAllocator<'a, 'alloc> {
    ssa: &'a SsaVec<'alloc>,
    lifetimes: Lifetimes<'alloc>,
    registers: Vec<'alloc, SsaId>,
}

impl<'a, 'alloc> RegisterAllocator<'a, 'alloc> {
    pub fn new(alloc: &'alloc Bump, ssa: &'a SsaVec<'alloc>) -> Self {
        let mut lifetimes =
            Lifetimes(alloc.alloc_slice_fill_copy(ssa.len(), SsaId::from(Index::MAX)));
        for (idx, s) in ssa.iter().enumerate() {
            let idx = SsaId::from(idx);
            match *s {
                Ssa::GetGlobal
                | Ssa::CreateEnvironment
                | Ssa::CreateObject
                | Ssa::GetEnvironment { depth: _ }
                | Ssa::LoadConstant { constant: _ }
                | Ssa::Jump { to: _ } => {}
                Ssa::Binary { op: _, left, right }
                | Ssa::Alias { left, right }
                | Ssa::AssignEnvironment {
                    slot: _,
                    value: left,
                    env: right,
                }
                | Ssa::Index {
                    object: left,
                    key: right,
                } => {
                    lifetimes[idx] = idx;
                    lifetimes[left] = idx;
                    lifetimes[right] = idx;
                }
                Ssa::Assign { object, key, value } => {
                    lifetimes[object] = idx;
                    lifetimes[key] = idx;
                    lifetimes[value] = idx;
                }
                Ssa::Unary { op: _, operand: v }
                | Ssa::IndexEnvironment { env: v, slot: _ }
                | Ssa::ConditionalJump {
                    condition: v,
                    to: _,
                } => {
                    lifetimes[idx] = idx;
                    lifetimes[v] = idx;
                }
                Ssa::Return { expr } => {
                    if let Some(x) = expr {
                        lifetimes[x] = idx;
                    }
                }
            }
        }
        RegisterAllocator {
            lifetimes,
            registers: Vec::new_in(alloc),
            ssa,
        }
    }

    pub fn is_used(&self, id: SsaId) -> bool {
        self.lifetimes[id] != SsaId::from(Index::MAX)
    }

    fn is_never_killed(ssa: Ssa) -> bool {
        match ssa {
            Ssa::GetGlobal
            | Ssa::GetEnvironment { .. }
            | Ssa::LoadConstant { .. }
            | Ssa::IndexEnvironment { .. } => true,
            Ssa::Binary { .. } | Ssa::Unary { .. } | Ssa::Alias { .. } => false,
            Ssa::ConditionalJump { .. }
            | Ssa::Jump { .. }
            | Ssa::Return { .. }
            | Ssa::AssignEnvironment { .. } => panic!("not a value ssa instruction"),
            _ => todo!(),
        }
    }

    pub fn allocate(&mut self, value: SsaId) -> u8 {
        // Find a register which is either free or can be killed
        let lifetimes = &self.lifetimes;
        let register = self
            .registers
            .iter()
            .copied()
            .enumerate()
            .find(move |(_, v)| lifetimes[*v] < value)
            .map(|x| x.0)
            .unwrap_or_else(|| {
                let id = self.registers.len();
                self.registers.push(SsaId::from(0u32));
                id
            });
        self.registers[register] = value;
        assert!(register < runtime::MAX_REGISTERS as usize);
        register as u8
    }

    pub fn retrieve_register(&self, value: SsaId) -> u8 {
        self.registers
            .iter()
            .copied()
            .enumerate()
            .find(|(_, v)| value == *v)
            .map(|x| x.0)
            .unwrap() as u8
    }

    pub fn used_registers(&self) -> u8 {
        self.registers.len() as u8
    }
}
