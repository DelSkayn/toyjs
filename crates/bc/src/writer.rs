use std::marker::PhantomData;

use crate::{Instruction, InstructionType};

#[derive(Clone, Debug, Default)]
pub struct ByteCodeWriter {
    instructions: Vec<Instruction>,
}

#[derive(Clone, Copy, Debug)]
pub struct WriteOffset<T>(usize, PhantomData<T>);

impl ByteCodeWriter {
    /// Create a new ByteCodeWriter.
    pub fn new() -> Self {
        ByteCodeWriter {
            instructions: Vec::new(),
        }
    }
}
