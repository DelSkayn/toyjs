use std::usize;

use bc::{util, LongOffset, OpCode, Reg};

use crate::{Compiler, Error, Limits, PendingArg, Result};

impl<'a> Compiler<'a> {
    pub fn restructure_jumps(&mut self, start: u32) {
        let slice = &mut self.instructions[(start as usize)..];
        let mut address_adjust = vec![(0, 0)];
        let mut patch_jumps = Vec::new();

        let mut offset = 0;
        let mut idx = 0;
        while idx < slice.len() {
            let opcode = OpCode::from_u8(slice[idx]).unwrap();

            match opcode {
                OpCode::LongJump => {
                    let dst: LongOffset = util::from_slice(&slice[idx + 1..]);
                    if dst.0 as i64 as i32 == dst.0 {
                        util::to_slice(&mut slice[idx + 1 - offset..], bc::Offset(dst.0 as i16));
                        address_adjust.push((idx, offset));
                        offset += 2;
                    } else {
                        util::to_slice(&mut slice[idx + 1 - offset..], dst);
                    }
                    patch_jumps.push((idx - offset, idx + 5));
                    idx += 5;
                }
                OpCode::LongJumpTrue | OpCode::LongJumpFalse => {
                    let dst: LongOffset = util::from_slice(&slice[idx + 1..]);
                    if dst.0 as i64 as i32 == dst.0 {
                        util::to_slice(&mut slice[idx + 1 - offset..], bc::Offset(dst.0 as i16));
                        address_adjust.push((idx, offset));
                        offset += 2;
                    } else {
                        util::to_slice(&mut slice[idx + 1 - offset..], dst);
                    }
                    patch_jumps.push((idx - offset, idx + 6));
                    idx += 6;
                }
                _ => {
                    let size = opcode.size();
                    slice.copy_within(idx..(idx + size), idx - offset);
                    idx += size;
                }
            }
        }
        address_adjust.push((slice.len(), offset));
    }

    pub fn patch_args(&mut self) -> Result<()> {
        // TODO: Optimize instruction usage.
        // For now we assume all the registers used in the function are used when calling a
        // function. This isn't always the case.

        let max_regs = self.function_stack_size;

        if max_regs > i32::MAX as u32 - self.max_arg as u32 {
            return Err(Error::ExceededLimits(Limits::Registers));
        }

        let register_count = max_regs + self.max_arg as u32;

        for i in 0..self.arg_patch.len() {
            let PendingArg {
                instruction,
                offset,
            } = self.arg_patch[i];
            let arg_reg = register_count - offset as u32;
            if arg_reg < Reg::MAX as u32 {
                self.patch_dst(dbg!(instruction), dbg!(Reg(arg_reg as i8)))
            } else {
                to_do!()
            }
        }

        self.arg_patch.clear();
        self.max_arg = 0;

        Ok(())
    }

    pub fn finalize_instructions(&mut self, start: u32) -> Result<()> {
        self.patch_args()?;
        // self.restructure_jumps();

        Ok(())
    }
}
