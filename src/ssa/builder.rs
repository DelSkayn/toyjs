use crate::ssa::{Constant, ConstantId, InstrVar, Instruction, Ssa, SsaVar};
use fxhash::FxHashMap;
use std::mem;

pub struct JumpContext {
    true_list: Vec<SsaVar>,
    false_list: Vec<SsaVar>,
}

pub struct SsaBuilder {
    instructions: Vec<Instruction>,
    constants: Vec<Constant>,
    constant_location: FxHashMap<Constant, usize>,
    true_list: Vec<SsaVar>,
    false_list: Vec<SsaVar>,
}

impl SsaBuilder {
    pub fn new() -> Self {
        SsaBuilder {
            instructions: Vec::new(),
            constants: Vec::new(),
            constant_location: FxHashMap::default(),
            true_list: Vec::new(),
            false_list: Vec::new(),
        }
    }

    pub fn get_mut(&mut self, var: SsaVar) -> &mut Instruction {
        &mut self.instructions[var.0 as usize]
    }

    pub fn next_id(&self) -> SsaVar {
        SsaVar(self.instructions.len() as u32)
    }

    pub fn push_instruction(&mut self, instr: Instruction) -> SsaVar {
        assert!(
            self.instructions.len() < (u32::max_value() - 2) as usize,
            "to many instructions!"
        );
        let id = self.instructions.len() as u32;
        self.instructions.push(instr);
        SsaVar(id)
    }

    pub fn patch_jump_target(&mut self, instr: SsaVar, target: InstrVar) {
        let t = target;
        match self.instructions[instr.0 as usize] {
            Instruction::Jump { ref mut target } => *target = t,
            Instruction::CondJump {
                negative: _,
                condition: _,
                ref mut target,
            } => *target = t,
            _ => panic!("ssa instruction to be patched is not a jump"),
        }
    }

    pub fn patch_jump_target_next(&mut self, instr: SsaVar) {
        self.patch_jump_target(instr, self.next_id().into())
    }

    pub fn load_constant<T: Into<Constant>>(&mut self, t: T) -> SsaVar {
        self.load_constant_inner(t.into())
    }

    pub fn take_jump_context(&mut self) -> JumpContext {
        JumpContext {
            true_list: mem::replace(&mut self.true_list, Vec::new()),
            false_list: mem::replace(&mut self.false_list, Vec::new()),
        }
    }

    pub fn clear_jump_context(&mut self) {
        self.true_list.clear();
        self.false_list.clear();
    }

    pub fn push_context_jump(&mut self, cond: SsaVar, truthy: bool) -> SsaVar {
        let instr = self.push_instruction(Instruction::CondJump {
            negative: !truthy,
            condition: cond.into(),
            target: InstrVar::null(),
        });
        if truthy {
            self.true_list.push(instr);
        } else {
            self.false_list.push(instr);
        }
        instr
    }

    pub fn patch_context_jump_target(&mut self, instr: SsaVar, ctx: &JumpContext, thruthy: bool) {
        let targets = if thruthy {
            &ctx.true_list
        } else {
            &ctx.false_list
        };
        for x in targets.iter().copied() {
            self.patch_jump_target(x, instr.into());
        }
    }

    fn load_constant_inner(&mut self, c: Constant) -> SsaVar {
        let const_id = if let Some(x) = self.constant_location.get(&c) {
            *x
        } else {
            let len = self.constants.len();
            self.constants.push(c.clone());
            self.constant_location.insert(c, len);
            len
        };
        assert!(
            self.constants.len() < (u32::max_value() - 2) as usize,
            "to many constants!"
        );
        let res = self.push_instruction(Instruction::LoadConstant {
            constant: ConstantId(const_id as u32),
        });
        res
    }

    pub fn build(self) -> Ssa {
        Ssa {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}
