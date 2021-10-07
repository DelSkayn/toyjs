#![allow(dead_code)]

use std::fmt;
pub mod gc;
pub use gc::{Gc, GcArena};
pub mod instructions;
pub mod value;
use instructions::{Instruction, InstructionBuffer, InstructionReader};
pub use value::JSValue;
pub mod object;
use object::Object;

pub struct ByteFunction {
    pub offset: usize,
    pub size: usize,
    pub registers: u8,
}

pub struct ByteCode {
    pub constants: Box<[JSValue]>,
    pub functions: Box<[ByteFunction]>,
    pub instructions: InstructionBuffer,
}

impl fmt::Display for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "> CONSTANTS")?;
        for (idx, c) in self.constants.iter().enumerate() {
            writeln!(f, "{:>4}: {:?}", idx, c)?;
        }
        writeln!(f)?;
        writeln!(f, "> INSTRUCTIONS")?;
        for (idx, func) in self.functions.iter().enumerate() {
            writeln!(
                f,
                "= FUNC:{:<4} registers:{:<2} instructions:{}",
                idx, func.registers, func.size
            )?;

            let mut reader = InstructionReader::new(&self.instructions, func.offset, func.size);
            let mut idx = 0;

            while !reader.at_end() {
                write!(f, "{:>4}: ", idx)?;
                Instruction::format_byte_instruction(f, &mut reader)?;
                writeln!(f)?;
                idx += 1;
            }
        }
        Ok(())
    }
}

pub struct Realm {
    global: Gc<Object>,
}
