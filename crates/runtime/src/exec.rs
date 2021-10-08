use crate::{
    gc::Gc,
    instructions::{opcode, InstructionReader},
    value, ByteCode, JSValue, Realm,
};

impl Realm {
    pub unsafe fn execute(&mut self, instr: &mut InstructionReader, bc: Gc<ByteCode>) -> JSValue {
        loop {
            let op = instr.read_u8();
            match op {
                opcode::LoadConst => {
                    let dst = instr.read_u8();
                    let cons = instr.read_u16();
                    self.stack
                        .write(dst, *bc.constants.get_unchecked(cons as usize));
                }
                opcode::Add => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    if Self::both_int(left, right) {
                        let res =
                            Self::coerce_int(left.into_int() as i64 + right.into_int() as i64);
                        self.stack.write(dst, res);
                    } else {
                        todo!()
                    }
                }
                opcode::Sub => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    if Self::both_int(left, right) {
                        let res =
                            Self::coerce_int(left.into_int() as i64 - right.into_int() as i64);
                        self.stack.write(dst, res);
                    } else {
                        todo!()
                    }
                }
                opcode::Jump => {
                    instr.read_u8();
                    let tgt = instr.read_i16();
                    instr.jump(tgt as i32)
                }
                opcode::JumpFalse => {
                    let cond = self.stack.read(instr.read_u8());
                    let tgt = instr.read_i16();
                    if self.is_nullish(cond) {
                        instr.jump(tgt as i32)
                    }
                }
                opcode::JumpTrue => {
                    let cond = self.stack.read(instr.read_u8());
                    let tgt = instr.read_i16();
                    if !self.is_nullish(cond) {
                        instr.jump(tgt as i32)
                    }
                }
                opcode::ReturnUndefined => return JSValue::undefined(),
                opcode::Return => {
                    let reg = instr.read_u8();
                    return self.stack.read(reg);
                }
                _ => todo!(),
            }
        }
    }

    unsafe fn is_nullish(&mut self, value: JSValue) -> bool {
        match value.tag() {
            value::TAG_INT => value.into_int() == 0,
            value::TAG_BASE => match value.0.bits {
                value::VALUE_NULL => true,
                value::VALUE_UNDEFINED => true,
                value::VALUE_FALSE => true,
                _ => false,
            },
            value::TAG_STRING => *value.into_string() == "",
            value::TAG_OBJECT => false,
            value::TAG_FUNCTION => false,
            value::TAG_SYMBOL => todo!(),
            _ => panic!("invalid tag"),
        }
    }

    #[inline]
    fn both_int(a: JSValue, b: JSValue) -> bool {
        a.is_int() && b.is_int()
    }

    fn coerce_int(int: i64) -> JSValue {
        if int as i32 as i64 == int {
            JSValue::from(int as i32)
        } else {
            JSValue::from(int as f64)
        }
    }
}
