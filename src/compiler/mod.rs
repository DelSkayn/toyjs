use crate::{
    interner::Interner,
    runtime::{self, bc::Bytecode},
    ssa::{Instruction, Ssa},
};
mod gen;

pub struct Compiler;

impl Compiler {
    pub fn compile(ssa: &Ssa, interner: &Interner) -> Bytecode {
        let ranges = dbg!(Self::calc_live_ranges(&ssa));
        let reg_alloc = dbg!(Self::alloc_registers(&ssa, &ranges));
        Self::generate_bytecode(ssa, &reg_alloc, interner)
    }

    pub fn alloc_registers(ssa: &Ssa, live: &Vec<u32>) -> Vec<u8> {
        let mut active = [u32::max_value(); runtime::NUM_REGISTERS];
        let len = ssa.instructions.len();
        let mut res = Vec::with_capacity(len);
        for i in 0..len {
            let mut free = 0;
            let mut next = runtime::NUM_REGISTERS as u8;
            for j in 0..runtime::NUM_REGISTERS {
                if active[j] == u32::max_value() {
                    next = j as u8;
                    free += 1;
                    break;
                }
                if live[active[j] as usize] <= i as u32 {
                    next = j as u8;
                    free += 1;
                    active[j] = u32::max_value();
                    break;
                }
            }
            if free == 0 {
                //spill
                todo!();
            }
            res.push(next);
            active[next as usize] = i as u32;
        }
        res
    }

    pub fn calc_live_ranges(ssa: &Ssa) -> Vec<u32> {
        let len = ssa.instructions.len();
        // u32::max_value indicates that the value is never used
        let mut res = vec![u32::max_value(); len];
        ssa.instructions
            .iter()
            .enumerate()
            .for_each(|(idx, instr)| match instr {
                Instruction::Unary { kind: _, operand } => {
                    res[operand.0 as usize] = idx as u32;
                }
                Instruction::Binary {
                    kind: _,
                    left,
                    right,
                } => {
                    res[left.0 as usize] = idx as u32;
                    res[right.0 as usize] = idx as u32;
                }
                Instruction::CondJump {
                    negative: _,
                    condition,
                    target: _,
                } => {
                    res[condition.0 as usize] = idx as u32;
                }
                Instruction::Alias { left, right } => {
                    res[left.0 as usize] = idx as u32;
                    res[right.0 as usize] = idx as u32;
                }
                Instruction::Return { value } => {
                    res[value.0 as usize] = idx as u32;
                }
                Instruction::Jump { target: _ } => {}
                Instruction::LoadConstant { constant: _ } => {}
            });
        res
    }
}
