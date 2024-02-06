use std::usize;

use bc::{ByteCode, Function};
use common::span::Span;

use crate::{Compiler, Error, Limits};

impl<'a> Compiler<'a> {
    pub fn into_bc(self) -> Result<ByteCode, Error> {
        if self.instructions.len() > u32::MAX as usize {
            return Err(Error::ExceededLimits(Limits::BytecodeSize));
        }

        // TODO: Shorten long jumps into normal jumps wherever possible.
        // TODO: Maybe do a single pass of some peephole optimizations.

        let len = self.instructions.len() as u32;

        Ok(ByteCode {
            functions: vec![Function {
                offset: 0,
                len,
                reflect_info: None,
                span: Span::empty(),
            }]
            .into_boxed_slice(),
            strings: Default::default(),
            string_buffer: Default::default(),
            instructions: self.instructions.into_boxed_slice(),
        })
    }
}
