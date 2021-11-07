use crate::{
    function::Function,
    gc::Gc,
    instructions::{opcode, ByteCode, InstructionOpcode, InstructionReader},
    realm::Realm,
    Object, Value,
};

impl Realm {
    pub unsafe fn execute(&mut self, instr: &mut InstructionReader, bc: Gc<ByteCode>) -> Value {
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
                    self.stack.write(dst, Value::from(self.global));
                }
                opcode::LoadFunction => {
                    self.gc.collect_debt(&(&*self, bc));
                    let dst = instr.read_u8();
                    let tgt = instr.read_u16();
                    let func = Function::from_bc(bc, tgt as usize);
                    let res = Value::from(self.gc.allocate(func));
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
                    let res = Value::from(self.gc.allocate(object));
                    self.stack.write(dst, res)
                }
                opcode::IndexAssign => {
                    let obj = self.stack.read(instr.read_u8());
                    let key = self.stack.read(instr.read_u8());
                    let val = self.stack.read(instr.read_u8());

                    if obj.is_object() {
                        let obj = obj.unsafe_cast_object();
                        self.gc.write_barrier(obj);
                        obj.unsafe_index_set(key, val, self);
                    } else {
                        todo!()
                    }
                }
                opcode::Index => {
                    let dst = instr.read_u8();
                    let obj = self.stack.read(instr.read_u8());
                    let key = self.stack.read(instr.read_u8());
                    if obj.is_object() {
                        let res = obj.unsafe_cast_object().unsafe_index(key, self);
                        self.stack.write(dst, res)
                    } else {
                        todo!()
                    }
                }
                opcode::Add => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    if Self::both_int(left, right) {
                        let res =
                            Self::coerce_int(left.cast_int() as i64 + right.cast_int() as i64);
                        self.stack.write(dst, res);
                    } else {
                        let res = self.add(left, right);
                        self.stack.write(dst, res);
                    }
                }
                opcode::Sub => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    if Self::both_int(left, right) {
                        let res =
                            Self::coerce_int(left.cast_int() as i64 - right.cast_int() as i64);
                        self.stack.write(dst, res);
                    } else {
                        let res = self.numeric_operator(left, right, InstructionOpcode::Sub);
                        self.stack.write(dst, res);
                    }
                }
                opcode::Mul => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    let res = self.numeric_operator(left, right, InstructionOpcode::Mul);
                    self.stack.write(dst, res);
                }
                opcode::Div => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    let res = self.numeric_operator(left, right, InstructionOpcode::Div);
                    self.stack.write(dst, res);
                }
                opcode::Mod => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    let res = self.numeric_operator(left, right, InstructionOpcode::Mod);
                    self.stack.write(dst, res);
                }
                opcode::Pow => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    let res = self.numeric_operator(left, right, InstructionOpcode::Pow);
                    self.stack.write(dst, res);
                }

                opcode::ShiftLeft => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    let left = self.convert_int(left);
                    let right = self.convert_int(right) as u32 % 32;
                    self.stack.write(dst, Value::from(left << right));
                }
                opcode::ShiftRight => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    let left = self.convert_int(left);
                    let right = self.convert_int(right) as u32 % 32;
                    self.stack.write(dst, Value::from(left >> right));
                }
                opcode::ShiftUnsigned => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    let left = self.convert_int(left) as u32;
                    let right = self.convert_int(right) as u32 % 32;
                    self.stack.write(dst, Value::from((left >> right) as i32));
                }
                opcode::SEqual => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    let res = self.strict_equal(left, right);
                    self.stack.write(dst, Value::from(res));
                }
                opcode::SNotEqual => {
                    let dst = instr.read_u8();
                    let left = self.stack.read(instr.read_u8());
                    let right = self.stack.read(instr.read_u8());
                    let res = !self.strict_equal(left, right);
                    self.stack.write(dst, Value::from(res));
                }

                opcode::IsNullish => {
                    let dst = instr.read_u8();
                    let src = self.stack.read(instr.read_u16() as u8);
                    let nullish = self.is_nullish(src);
                    self.stack.write(dst, Value::from(nullish))
                }
                opcode::Not => {
                    let dst = instr.read_u8();
                    let src = self.stack.read(instr.read_u16() as u8);
                    let falsish = self.is_falsish(src);
                    self.stack.write(dst, Value::from(falsish))
                }
                opcode::Negative => {
                    let dst = instr.read_u8();
                    let src = self.stack.read(instr.read_u16() as u8);
                    let number = self.coerce_number(src);
                    if number.is_int() {
                        let number = -(number.cast_int() as i64);
                        if number as i32 as i64 == number {
                            self.stack.write(dst, Value::from(number as i32))
                        } else {
                            self.stack.write(dst, Value::from(number as f64))
                        }
                    } else if number.is_float() {
                        self.stack.write(dst, Value::from(-number.cast_float()))
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
                        let res = func.unsafe_cast_function().call(self);
                        self.stack.write(dst, res);
                    } else {
                        todo!()
                    }
                }
                opcode::ReturnUndefined => return Value::undefined(),
                opcode::Return => {
                    let reg = instr.read_u8();
                    return self.stack.read(reg);
                }
                x => panic!("invalid opcode {}", x),
            }
        }
    }

    pub unsafe fn is_falsish(&mut self, value: Value) -> bool {
        if value.is_int() {
            value.cast_int() == 0
        } else if value.is_string() {
            *value.unsafe_cast_string() == ""
        } else {
            value.is_null() || value.is_undefined() || value.is_false()
        }
    }

    pub unsafe fn is_nullish(&mut self, value: Value) -> bool {
        value.is_null() || value.is_undefined()
    }

    pub unsafe fn coerce_string(&mut self, value: Value) -> String {
        if value.is_int() {
            format!("{}", value.cast_int())
        } else if value.is_null() {
            "null".to_string()
        } else if value.is_undefined() {
            "undefined".to_string()
        } else if value.is_true() {
            "true".to_string()
        } else if value.is_false() {
            "false".to_string()
        } else if value.is_string() {
            (*value.unsafe_cast_string()).clone()
        } else if value.is_object() {
            "[object Object]".to_string()
        } else {
            todo!()
        }
    }

    pub unsafe fn coerce_number(&mut self, value: Value) -> Value {
        if value.is_int() || value.is_float() {
            value
        } else if value.is_null() || value.is_undefined() || value.is_false() {
            Value::from(0i32)
        } else if value.is_string() {
            if let Ok(v) = value.unsafe_cast_string().parse::<f64>() {
                if v as i32 as f64 == v {
                    Value::from(v as i32)
                } else {
                    Value::from(v)
                }
            } else {
                Value::from(f64::NAN)
            }
        } else if value.is_object() || value.is_function() {
            let prim = self.to_primitive(value);
            self.coerce_number(prim)
        } else {
            todo!()
        }
    }

    pub unsafe fn convert_int(&mut self, value: Value) -> i32 {
        let number = self.coerce_number(value);
        if number.is_int() {
            return number.cast_int();
        } else if number.is_float() {
            let f = number.cast_float();
            if f.is_normal() {
                let res = f.abs().floor() as i32;
                if f.is_sign_positive() {
                    return res;
                } else {
                    return -res;
                }
            } else {
                return 0;
            }
        } else {
            todo!()
        }
    }

    pub unsafe fn strict_equal(&mut self, left: Value, right: Value) -> bool {
        if !left.same_type(right) {
            return false;
        }
        if left.is_int() {
            return left.cast_int() == right.cast_int();
        }
        if left.is_string() {
            return left.unsafe_cast_string() == right.unsafe_cast_string();
        }
        if left.is_object() {
            return left.unsafe_cast_object().ptr_eq(right.unsafe_cast_object());
        }
        if left.is_function() {
            return left
                .unsafe_cast_function()
                .ptr_eq(right.unsafe_cast_function());
        }
        if left.is_float() && right.is_float() {
            return left.cast_float() == right.cast_float();
        }
        todo!()
    }

    pub unsafe fn equal(&mut self, left: Value, right: Value) -> bool {
        if self.strict_equal(left, right) {
            return true;
        }
        if left.is_undefined() || left.is_null() {
            return right.is_undefined() || right.is_null();
        }
        if left.is_float() || left.is_int() && right.is_string() {
            return if let Ok(v) = right.unsafe_cast_string().parse::<f64>() {
                if v as i32 as f64 == v {
                    left.cast_float() == v
                } else {
                    left.cast_int() == v as i32
                }
            } else {
                false
            };
        }
        if right.is_float() || right.is_int() && left.is_string() {
            return if let Ok(v) = left.unsafe_cast_string().parse::<f64>() {
                if v as i32 as f64 == v {
                    right.cast_float() == v
                } else {
                    right.cast_int() == v as i32
                }
            } else {
                false
            };
        }
        todo!()
    }

    pub unsafe fn add(&mut self, left: Value, right: Value) -> Value {
        let left = self.to_primitive(left);
        let right = self.to_primitive(right);
        if left.is_string() || right.is_string() {
            let left = self.coerce_string(left);
            let right = self.coerce_string(right);
            return Value::from(self.gc.allocate(left.to_string() + &right.to_string()));
        }
        let left = self.coerce_number(left);
        let right = self.coerce_number(right);
        let left = if left.is_int() {
            left.cast_int() as f64
        } else {
            if left.is_float() {
                left.cast_float()
            } else {
                todo!()
            }
        };
        let right = if right.is_int() {
            right.cast_int() as f64
        } else {
            if right.is_float() {
                right.cast_float()
            } else {
                todo!()
            }
        };
        let res = left + right;
        if res as i32 as f64 == res {
            Value::from(res as i32)
        } else {
            Value::from(res)
        }
    }

    #[inline]
    pub unsafe fn numeric_operator(
        &mut self,
        left: Value,
        right: Value,
        op: InstructionOpcode,
    ) -> Value {
        let left = self.to_primitive(left);
        let right = self.to_primitive(right);
        let left = self.coerce_number(left);
        let right = self.coerce_number(right);
        let left = if left.is_int() {
            left.cast_int() as f64
        } else {
            if left.is_float() {
                left.cast_float()
            } else {
                todo!()
            }
        };
        let right = if right.is_int() {
            right.cast_int() as f64
        } else {
            if right.is_float() {
                right.cast_float()
            } else {
                todo!()
            }
        };
        let res = match op {
            InstructionOpcode::Sub => left - right,
            InstructionOpcode::Mul => left * right,
            InstructionOpcode::Div => left / right,
            InstructionOpcode::Mod => left % right,
            InstructionOpcode::Pow => left.powf(right),
            _ => panic!("invalid operator"),
        };
        if res as i32 as f64 == res {
            Value::from(res as i32)
        } else {
            Value::from(res)
        }
    }

    pub unsafe fn to_primitive(&mut self, v: Value) -> Value {
        if v.is_object() || v.is_function() {
            todo!()
        }
        return v;
    }

    #[inline]
    fn both_int(a: Value, b: Value) -> bool {
        a.is_int() && b.is_int()
    }

    fn coerce_int(int: i64) -> Value {
        if int as i32 as i64 == int {
            Value::from(int as i32)
        } else {
            Value::from(int as f64)
        }
    }
}
