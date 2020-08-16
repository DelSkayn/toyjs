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
}

#[derive(Clone, Copy, Debug)]
pub struct VarInfo {
    live: u32,
    rematerializable: bool,
    alias_parent: Option<u32>,
    alias_childeren: Option<(u32, u32)>,
}

impl Default for VarInfo {
    fn default() -> Self {
        VarInfo {
            live: u32::max_value(),
            rematerializable: false,
            alias_parent: None,
            alias_childeren: None,
        }
    }
}

impl Compiler {
    pub fn needs_register(instruction: Instruction) -> bool {
        match instruction {
            Instruction::LoadGlobal => true,
            Instruction::ObjectGet { object: _, key: _ } => true,
            Instruction::ObjectSet {
                object: _,
                key: _,
                value: _,
            } => false,
            Instruction::Move { operand: _ } => true,
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
            Instruction::Jump { target: _ } => false,
            Instruction::CondJump {
                target: _,
                condition: _,
                negative: _,
            } => false,
            Instruction::Return { value: _ } => false,
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
            if let Some(x) = info.alias_parent {
                let (left, right) = live.vars[x as usize].alias_childeren.unwrap();
                if left.max(right) == i as u32 {
                    let last = if left == i as u32 { right } else { left };
                    for j in 0..runtime::NUM_REGISTERS as u8 {
                        if active[j as usize] == last {
                            active[j as usize] = i as u32;
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
            if let Some((left, right)) = info.alias_childeren {
                let last = left.max(right);
                for j in 0..runtime::NUM_REGISTERS as u8 {
                    if active[j as usize] == last {
                        active[j as usize] = i as u32;
                        next = j;
                        break;
                    }
                }
                assert!(
                    next != runtime::NUM_REGISTERS as u8,
                    "aliased register not present!"
                );
                res.push(next);
                if info.alias_parent.is_none() {
                    continue;
                }
            }
            for (j, a) in active.iter_mut().enumerate() {
                if *a == u32::max_value() {
                    next = j as u8;
                    free += 1;
                    break;
                }
                if live.vars[*a as usize].live <= i as u32 {
                    next = j as u8;
                    free += 1;
                    *a = u32::max_value();
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
        let mut vars = vec![VarInfo::default(); len];
        ssa.instructions
            .iter()
            .enumerate()
            .for_each(|(idx, instr)| match *instr {
                Instruction::Move { operand } => {
                    vars[operand.as_u32() as usize].live = idx as u32;
                }
                Instruction::ObjectGet { object, key } => {
                    vars[object.as_u32() as usize].live = idx as u32;
                    vars[key.as_u32() as usize].live = idx as u32;
                }
                Instruction::Unary { kind: _, operand } => {
                    vars[operand.as_u32() as usize].live = idx as u32;
                }
                Instruction::Binary {
                    kind: _,
                    left,
                    right,
                } => {
                    vars[left.as_u32() as usize].live = idx as u32;
                    vars[right.as_u32() as usize].live = idx as u32;
                }
                Instruction::CondJump {
                    negative: _,
                    condition,
                    target: _,
                } => {
                    vars[condition.as_u32() as usize].live = idx as u32;
                }
                Instruction::Alias { left, right } => {
                    vars[left.as_u32() as usize].live = idx as u32;
                    vars[right.as_u32() as usize].live = idx as u32;
                    assert!(vars[left.as_u32() as usize]
                        .alias_parent
                        .replace(idx as u32)
                        .is_none());
                    assert!(vars[right.as_u32() as usize]
                        .alias_parent
                        .replace(idx as u32)
                        .is_none());
                    assert!(vars[idx]
                        .alias_childeren
                        .replace((left.as_u32(), right.as_u32()))
                        .is_none());
                }
                Instruction::Return { value } => {
                    if value != InstrVar::null() {
                        vars[value.as_u32() as usize].live = idx as u32;
                    }
                }
                Instruction::Jump { target: _ } => {}
                Instruction::LoadGlobal => {
                    vars[idx].rematerializable = true;
                }
                Instruction::ObjectSet { object, key, value } => {
                    vars[value.as_u32() as usize].live = idx as u32;
                    vars[object.as_u32() as usize].live = idx as u32;
                    vars[key.as_u32() as usize].live = idx as u32;
                }
                Instruction::LoadConstant { constant: _ } => {
                    vars[idx].rematerializable = true;
                }
            });
        LifeInfo { vars }
    }
}
