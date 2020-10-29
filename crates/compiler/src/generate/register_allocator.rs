use crate::ssa::{Ssa, SsaId, SsaVec};
use bumpalo::{collections::Vec, Bump};
use common::{index::Index, newtype_slice};

#[derive(Debug)]
struct Lifetimes<'alloc>(&'alloc mut [SsaId]);
newtype_slice!(struct Lifetimes<'alloc,>[SsaId] -> SsaId);

#[derive(Debug)]
struct Allocations<'alloc>(&'alloc mut [Option<u8>]);
newtype_slice!(struct Allocations<'alloc,>[SsaId] -> Option<u8,>);

#[derive(Clone, Copy, Default, Debug)]
struct Alias {
    pub taken_from: Option<(SsaId, SsaId)>,
    pub merged_in: Option<SsaId>,
}

#[derive(Debug)]
struct Aliases<'alloc>(&'alloc mut [Alias]);
newtype_slice!(struct Aliases<'alloc,>[SsaId] -> Alias);

impl<'alloc> Aliases<'alloc> {
    pub fn traverse<F: FnMut(SsaId)>(&self, start: SsaId, f: &mut F) {
        f(start);
        self.traverse_up(start, f);
        self.traverse_down(start, f);
    }

    fn traverse_up<F: FnMut(SsaId)>(&self, cur: SsaId, f: &mut F) {
        if let Some((left, right)) = self[cur].taken_from {
            f(left);
            f(right);
            self.traverse_up(left, f);
            self.traverse_up(right, f);
        }
    }
    fn traverse_down<F: FnMut(SsaId)>(&self, cur: SsaId, f: &mut F) {
        if let Some(down) = self[cur].merged_in {
            f(down);
            let (left, right) = self[down].taken_from.unwrap();
            if left != cur {
                f(left);
                self.traverse_up(left, f);
            } else if right != cur {
                f(right);
                self.traverse_up(right, f);
            }
            self.traverse_down(down, f);
        }
    }
}

#[derive(Debug)]
pub struct RegisterAllocator<'a, 'alloc> {
    ssa: &'a SsaVec<'alloc>,
    aliases: Aliases<'alloc>,
    lifetimes: Lifetimes<'alloc>,
    allocations: Allocations<'alloc>,
    registers: Vec<'alloc, SsaId>,
}

impl<'a, 'alloc> RegisterAllocator<'a, 'alloc> {
    pub fn new(alloc: &'alloc Bump, ssa: &'a SsaVec<'alloc>) -> Self {
        let mut lifetimes =
            Lifetimes(alloc.alloc_slice_fill_copy(ssa.len(), SsaId::from(Index::MAX)));
        let mut aliases = Aliases(alloc.alloc_slice_fill_copy(ssa.len(), Alias::default()));
        let allocations = Allocations(alloc.alloc_slice_fill_copy(ssa.len(), None));
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
                Ssa::Alias { left, right } => {
                    aliases[idx].taken_from = Some((left, right));
                    aliases[left].merged_in = Some(idx);
                    aliases[right].merged_in = Some(idx);
                    aliases.traverse(idx, &mut |v| lifetimes[v] = idx);
                }
                Ssa::Assign { object, key, value } => {
                    lifetimes[object] = idx;
                    lifetimes[key] = idx;
                    lifetimes[value] = idx;
                }
                Ssa::Unary { operand: v, .. }
                | Ssa::IndexEnvironment { env: v, .. }
                | Ssa::ConditionalJump { condition: v, .. } => {
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
            aliases,
            allocations,
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
        self.allocate_tmp(value)
    }

    fn has_alias(&self, value: SsaId) -> bool {
        self.aliases[value].merged_in.is_some() || self.aliases[value].taken_from.is_some()
    }

    pub fn allocate_tmp(&mut self, value: SsaId) -> u8 {
        if self.has_alias(value) {
            if let Some(x) = self.allocations[value] {
                return x;
            }
        }

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
                self.registers.push(value);
                id
            });

        let to_free = self.registers[register];
        if self.allocations[to_free].is_some() {
            let allocations = &mut self.allocations;
            self.aliases
                .traverse(to_free, &mut |v| allocations[v] = None);
        }
        self.registers[register] = value;
        assert!(register < runtime::MAX_REGISTERS as usize);
        let allocations = &mut self.allocations;
        self.aliases
            .traverse(value, &mut |v| allocations[v] = Some(register as u8));
        register as u8
    }

    pub fn retrieve_register(&self, value: SsaId) -> u8 {
        self.allocations[value].unwrap()
    }

    pub fn used_registers(&self) -> u8 {
        self.registers.len() as u8
    }
}
