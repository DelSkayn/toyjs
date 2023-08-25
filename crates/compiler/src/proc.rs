use std::collections::VecDeque;

use bc::{ByteCode, Function, Instruction, LongOffset, Offset};
use common::span::Span;

use crate::{Compiler, Error};

impl<'a> Compiler<'a> {
    pub fn into_bc(mut self) -> Result<ByteCode, Error> {
        if self.instructions.len() > isize::MAX as usize {
            return Err(Error::ExceededLimits);
        }
        // First calculate size prefix sums.
        // Also keep track of jump instructions.
        let mut prefix = Vec::with_capacity(self.instructions.len());
        let mut jumps = Vec::new();
        let mut acc = 0;
        for (idx, instr) in self.instructions.iter().enumerate() {
            prefix.push(acc);
            acc += instr.size();
            // Ignore long jumps, they will never need to be upgraded.
            if let Instruction::Jump { .. }
            | Instruction::JumpTrue { .. }
            | Instruction::JumpFalse { .. } = instr
            {
                jumps.push(idx);
            }
        }

        // Now we have the approximate size of all instructions see if any jump instruction needs
        // to be upgraded to a long jump
        let mut upgrades = VecDeque::new();
        loop {
            let mut error = false;

            // we remove any upgrade jumps, because the wont be upgraded again.
            jumps.retain(|jump| {
                let (Instruction::Jump { ref mut dst }
                | Instruction::JumpTrue { ref mut dst, .. }
                | Instruction::JumpFalse { ref mut dst, .. }) = self.instructions[*jump]
                else {
                    unreachable!()
                };

                let current_byte = prefix[*jump] as isize;
                let Some(target_instruction) = (*jump as isize).checked_add(dst.0 as isize) else {
                    error = true;
                    return false;
                };
                let target_byte = prefix[target_instruction as usize] as isize;
                let Ok(byte_offset) = i32::try_from(target_byte - current_byte) else {
                    error = true;
                    return false;
                };
                if byte_offset as i16 as i32 == byte_offset {
                    true
                } else {
                    upgrades.push_front(*jump);
                    false
                }
            });

            if error {
                return Err(Error::ExceededLimits);
            }

            let Some(cur_upgrade) = upgrades.pop_back() else {
                break;
            };

            // update the prefix sums.
            let mut addition = 2;
            for (idx, prefix) in prefix[cur_upgrade..].iter_mut().enumerate() {
                // increase the extra size added for every instruction upgraded.
                if Some(idx) == upgrades.back().copied() {
                    upgrades.pop_back();
                    addition += 2;
                }
                *prefix += addition;
            }
        }

        // Atleast the size of instructions;
        let mut bytes = Vec::with_capacity(self.instructions.len());

        // At this point all jumps which should be upgraded are upgraded.
        for (idx, mut instr) in self.instructions.iter().copied().enumerate() {
            match instr {
                Instruction::Jump { ref mut dst }
                | Instruction::JumpTrue { ref mut dst, .. }
                | Instruction::JumpFalse { ref mut dst, .. } => {
                    let current_byte = prefix[idx] as isize;
                    let target_instruction = (idx as isize).checked_add(dst.0 as isize).unwrap();
                    let target_byte = prefix[target_instruction as usize] as isize;
                    let byte_offset = i16::try_from(target_byte - current_byte).unwrap();
                    *dst = Offset(byte_offset);
                }
                Instruction::LongJump { ref mut dst }
                | Instruction::LongJumpTrue { ref mut dst, .. }
                | Instruction::LongJumpFalse { ref mut dst, .. } => {
                    let current_byte = prefix[idx] as isize;
                    let target_instruction = (idx as isize).checked_add(dst.0 as isize).unwrap();
                    let target_byte = prefix[target_instruction as usize] as isize;
                    let byte_offset = i32::try_from(target_byte - current_byte).unwrap();
                    *dst = LongOffset(byte_offset);
                }
                _ => {}
            }
            instr.write(&mut bytes);
        }

        let len: u32 = bytes.len().try_into().map_err(|_| Error::ExceededLimits)?;

        Ok(ByteCode {
            functions: vec![Function {
                offset: 0,
                len,
                reflect_info: None,
                span: Span::empty(),
            }]
            .into_boxed_slice(),
            instructions: bytes.into_boxed_slice(),
        })
    }
}
