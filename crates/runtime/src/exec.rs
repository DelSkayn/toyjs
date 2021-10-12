use crate::{
    function::Function,
    gc::Gc,
    instructions::{opcode, InstructionReader},
    value, ByteCode, JSValue, Object, Realm,
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
                opcode::LoadGlobal => {
                    let dst = instr.read_u8();
                    instr.read_u16();
                    self.stack.write(dst, JSValue::from(self.global));
                }
                opcode::LoadFunction => {
                    self.gc.collect_debt(&(&*self, bc));
                    let dst = instr.read_u8();
                    let tgt = instr.read_u16();
                    let func = Function::from_bc(bc, tgt as usize);
                    let res = JSValue::from(self.gc.allocate(func));
                    self.stack.write(dst, res);
                }
                opcode::Move => {
                    let dst = instr.read_u8();
                    let src = instr.read_u16() as u8;
                    self.stack.write(dst, self.stack.read(src))
                }
                opcode::CreateObject => {
                    self.gc.collect_debt(&(&*self, bc));
                    let dst = instr.read_u8();
                    instr.read_u16();
                    let object = Object::new();
                    let res = JSValue::from(self.gc.allocate(object));
                    self.stack.write(dst, res)
                }
                opcode::IndexAssign => {
                    let obj = self.stack.read(instr.read_u8());
                    let key = self.stack.read(instr.read_u8());
                    let val = self.stack.read(instr.read_u8());
                    match obj.tag() {
                        value::TAG_OBJECT => {
                            let obj = obj.into_object();
                            self.gc.write_barrier(obj);
                            obj.index_set(key, val, self);
                        }
                        _ => todo!(),
                    }
                }
                opcode::Index => {
                    let dst = instr.read_u8();
                    let obj = self.stack.read(instr.read_u8());
                    let key = self.stack.read(instr.read_u8());
                    match obj.tag() {
                        value::TAG_OBJECT => {
                            let res = obj.into_object().index(key, self);
                            self.stack.write(dst, res)
                        }
                        _ => todo!(),
                    }
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
                opcode::IsNullish => {
                    let dst = instr.read_u8();
                    let src = self.stack.read(instr.read_u16() as u8);
                    let nullish = self.is_nullish(src);
                    self.stack.write(dst, JSValue::from(nullish))
                }
                opcode::Not => {
                    let dst = instr.read_u8();
                    let src = self.stack.read(instr.read_u16() as u8);
                    let falsish = self.is_falsish(src);
                    self.stack.write(dst, JSValue::from(falsish))
                }
                opcode::Jump => {
                    instr.read_u8();
                    let tgt = instr.read_i16();
                    instr.jump(tgt as i32)
                }
                opcode::JumpFalse => {
                    let cond = self.stack.read(instr.read_u8());
                    let tgt = instr.read_i16();
                    if self.is_falsish(cond) {
                        instr.jump(tgt as i32)
                    }
                }
                opcode::JumpTrue => {
                    let cond = self.stack.read(instr.read_u8());
                    let tgt = instr.read_i16();
                    if !self.is_falsish(cond) {
                        instr.jump(tgt as i32)
                    }
                }
                opcode::SetArg => {
                    let tgt = instr.read_u8();
                    let src = self.stack.read(instr.read_u16() as u8);
                    self.stack.write_arg(tgt, src);
                }

                opcode::Call => {
                    let dst = instr.read_u8();
                    let func = self.stack.read(instr.read_u8());
                    let _num = instr.read_u8();
                    if func.is_function() {
                        let res = func.into_function().call(self);
                        self.stack.write(dst, res);
                    } else {
                        todo!()
                    }
                }
                opcode::ReturnUndefined => return JSValue::undefined(),
                opcode::Return => {
                    let reg = instr.read_u8();
                    return self.stack.read(reg);
                }
                x => panic!("invalid opcode {}", x),
            }
        }
    }

    pub unsafe fn is_falsish(&mut self, value: JSValue) -> bool {
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

    pub unsafe fn is_nullish(&mut self, value: JSValue) -> bool {
        match value.tag() {
            value::TAG_BASE => match value.0.bits {
                value::VALUE_NULL => true,
                value::VALUE_UNDEFINED => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub unsafe fn coerce_string(&mut self, value: JSValue) -> String {
        match value.tag() {
            value::TAG_INT => format!("{}", value.into_int()),
            value::TAG_BASE => match value.0.bits {
                value::VALUE_NULL => "null".to_string(),
                value::VALUE_UNDEFINED => "undefined".to_string(),
                value::VALUE_TRUE => "true".to_string(),
                value::VALUE_FALSE => "false".to_string(),
                _ => panic!("invalid base value"),
            },
            // TODO find a better way?
            value::TAG_STRING => (*value.into_string()).clone(),
            value::TAG_OBJECT => "[object Object]".to_string(),
            value::TAG_FUNCTION => todo!(),
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
