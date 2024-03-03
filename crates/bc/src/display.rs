use std::fmt;

use common::hashmap::HashMap;

use crate::{ByteCode, Instruction, SafeByteCodeReader};

fn fmt_instructions(instr: &[u8], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, " - INSTRUCTIONS -")?;
    let mut label_id = 0;
    let mut jumps_label = HashMap::<usize, usize>::new();
    let mut reader = SafeByteCodeReader::from_bc(instr);
    while let Some(x) = reader.read_instruction() {
        match x {
            Instruction::LongJumpFalse { dst, .. }
            | Instruction::LongJumpTrue { dst, .. }
            | Instruction::LongJump { dst } => {
                let tgt = reader.offset() as isize + dst.0 as isize;
                jumps_label.entry(tgt as usize).or_insert_with(|| {
                    let res = label_id;
                    label_id += 1;
                    res
                });
            }
            Instruction::JumpFalse { dst, .. }
            | Instruction::JumpTrue { dst, .. }
            | Instruction::Jump { dst } => {
                let tgt = reader.offset() as isize + dst.0 as isize;
                jumps_label.entry(tgt as usize).or_insert_with(|| {
                    let res = label_id;
                    label_id += 1;
                    res
                });
            }
            _ => {}
        }
    }

    if let Some(id) = jumps_label.get(&0) {
        writeln!(f, "L{id}")?;
    }

    let mut reader = SafeByteCodeReader::from_bc(instr);
    while let Some(instr) = reader.read_instruction() {
        match instr {
            Instruction::LongJumpFalse { dst, .. }
            | Instruction::LongJumpTrue { dst, .. }
            | Instruction::LongJump { dst } => {
                let tgt = reader.offset() as isize + dst.0 as isize;
                let label = jumps_label.get(&(tgt as usize)).unwrap();
                write!(f, "    {}", instr)?;
                writeln!(f, "\t= L{label}")?;
            }
            Instruction::JumpFalse { dst, .. }
            | Instruction::JumpTrue { dst, .. }
            | Instruction::Jump { dst } => {
                let tgt = reader.offset() as isize + dst.0 as isize;
                let label = jumps_label.get(&(tgt as usize)).unwrap();
                write!(f, "    {}", instr)?;
                writeln!(f, "\t= L{label}")?;
            }
            _ => {
                writeln!(f, "    {}", instr)?;
            }
        }

        if let Some(id) = jumps_label.get(&reader.offset()) {
            writeln!(f, "L{id}:")?;
        }
    }

    Ok(())
}

impl fmt::Display for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, func) in self.functions.iter().enumerate() {
            writeln!(
                f,
                "FUNCTION #{idx} SIZE: {} REGISTERS: {} UPVALUES: {}",
                func.len, func.registers, func.upvalues
            )?;
            let range = (func.offset as usize)..((func.offset + func.len) as usize);
            fmt_instructions(&self.instructions[range], f)?;
        }

        Ok(())
    }
}
