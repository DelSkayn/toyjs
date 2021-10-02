#![allow(dead_code)]

use std::fmt;
pub mod gc;
pub use gc::{Gc, GcArena};
pub mod instructions;
pub mod value;
use instructions::{Instruction, InstructionBuffer, InstructionReader};
pub use value::JSValue;

pub struct ByteCode {
    pub constants: Box<[JSValue]>,
    pub instructions: InstructionBuffer,
}

impl fmt::Display for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut reader = InstructionReader::new(&self.instructions);
        let mut idx = 0;

        writeln!(f, " # CONSTANTS")?;
        for (idx, c) in self.constants.iter().enumerate() {
            writeln!(f, "{:>4}: {:?}", idx, c)?;
        }

        writeln!(f)?;
        writeln!(f, " # INSTRUCTIONS")?;
        while !reader.at_end() {
            write!(f, "{:>4}: ", idx)?;
            Instruction::format_byte_instruction(f, &mut reader)?;
            writeln!(f)?;
            idx += 1;
        }
        Ok(())
    }
}
