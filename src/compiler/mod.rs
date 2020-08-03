use crate::{
    interner::Interner,
    runtime::{self, bc::Bytecode},
    ssa::{Instruction, Ssa},
};
mod gen;

pub struct Compiler;

#[derive(Debug)]
pub struct LifeInfo {
    vars: Vec<VarInfo>,
    aliased: Vec<AliasedInfo>,
}

#[derive(Debug)]
pub struct AliasedInfo {
    parent: u32,
    left: u32,
    right: u32,
}

#[derive(Clone, Copy, Debug)]
pub struct VarInfo {
    live: u32,
    rematerializable: bool,
    aliased: Option<u32>,
}

impl Default for VarInfo {
    fn default() -> Self {
        VarInfo {
            live: u32::max_value(),
            rematerializable: false,
            aliased: None,
        }
    }
}

impl Compiler {
    pub fn needs_register(instruction: Instruction) -> bool {
        match instruction {
            Instruction::Unary {
                kind: _,
                operand: _,
            } => true,
            Instruction::Binary {
                kind: _,
                left: _,
                right: _,
            } => true,
            Instruction::LoadConstant { constant: _ } => true,
            Instruction::Alias { left: _, right: _ } => true,
            _ => false,
        }
    }

    pub fn compile(ssa: &Ssa, interner: &Interner) -> Bytecode {
        let live_info = dbg!(Self::calc_live_info(&ssa));
        let reg_alloc = dbg!(Self::alloc_registers(&ssa, &live_info));
        Self::generate_bytecode(ssa, &reg_alloc, interner)
    }

    pub fn alloc_registers(ssa: &Ssa, live: &LifeInfo) -> Vec<u8> {
        let mut active = [u32::max_value(); runtime::NUM_REGISTERS];
        let len = ssa.instructions.len();
        let mut res = Vec::with_capacity(len);
        for i in 0..len {
            if !Self::needs_register(ssa.instructions[i]) {
                res.push(0xff);
                continue;
            }
            let info = &live.vars[i];
            let mut free = 0;
            let mut next = runtime::NUM_REGISTERS as u8;
            if let Some(x) = info.aliased {
                // Handle aliased instructions
                // In case of the left hand instruction
                // just allocate it as normal
                let aliased = &live.aliased[x as usize];
                if aliased.right == i as u32 {
                    for j in 0..runtime::NUM_REGISTERS as u8 {
                        if active[j as usize] == aliased.left {
                            active[j as usize] = aliased.right;
                            next = j;
                            break;
                        }
                    }
                    assert!(
                        next != runtime::NUM_REGISTERS as u8,
                        "aliased register not present!"
                    );
                    res.push(next);
                    continue;
                } else if aliased.parent == i as u32 {
                    for j in 0..runtime::NUM_REGISTERS as u8 {
                        if active[j as usize] == aliased.right {
                            active[j as usize] = aliased.parent;
                            next = j;
                            break;
                        }
                    }
                    assert!(
                        next != runtime::NUM_REGISTERS as u8,
                        "aliased register not present!"
                    );
                    res.push(next);
                    continue;
                }
            }
            for j in 0..runtime::NUM_REGISTERS {
                if active[j] == u32::max_value() {
                    next = j as u8;
                    free += 1;
                    break;
                }
                if live.vars[active[j] as usize].live <= i as u32 {
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

    pub fn calc_live_info(ssa: &Ssa) -> LifeInfo {
        let len = ssa.instructions.len();
        // u32::max_value indicates that the value is never used
        let mut aliased = Vec::new();
        let mut vars = vec![VarInfo::default(); len];
        ssa.instructions
            .iter()
            .enumerate()
            .for_each(|(idx, instr)| match instr {
                Instruction::Unary { kind: _, operand } => {
                    vars[operand.0 as usize].live = idx as u32;
                }
                Instruction::Binary {
                    kind: _,
                    left,
                    right,
                } => {
                    vars[left.0 as usize].live = idx as u32;
                    vars[right.0 as usize].live = idx as u32;
                }
                Instruction::CondJump {
                    negative: _,
                    condition,
                    target: _,
                } => {
                    vars[condition.0 as usize].live = idx as u32;
                }
                Instruction::Alias { left, right } => {
                    vars[left.0 as usize].live = idx as u32;
                    vars[right.0 as usize].live = idx as u32;
                    let aliased_idx = aliased.len() as u32;
                    vars[idx].aliased = Some(aliased_idx);
                    vars[left.0 as usize].aliased = Some(aliased_idx);
                    vars[right.0 as usize].aliased = Some(aliased_idx);
                    aliased.push(AliasedInfo {
                        parent: idx as u32,
                        left: left.0 as u32,
                        right: right.0 as u32,
                    });
                }
                Instruction::Return { value } => {
                    vars[value.0 as usize].live = idx as u32;
                }
                Instruction::Jump { target: _ } => {}
                Instruction::LoadConstant { constant: _ } => {
                    vars[idx].rematerializable = true;
                }
            });
        LifeInfo { aliased, vars }
    }
}
