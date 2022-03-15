use std::borrow::Cow;

use crate::{
    function::{Function, FunctionKind, VmFunction, RECURSIVE_FUNC_PANIC},
    instructions::{Instruction, Upvalue},
    object::Object,
    Gc, Value,
};

use super::{reader::InstructionReader, Realm};

pub enum NumericOperator {
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

impl Realm {
    pub unsafe fn execute(
        &mut self,
        mut instr: InstructionReader,
        mut function: Gc<Function>,
    ) -> Result<Value, ()> {
        loop {
            match instr.next() {
                Instruction::LoadConst { dst, cons } => {
                    self.stack.write(dst, instr.constant(cons as u32));
                }
                Instruction::LoadGlobal { dst } => {
                    self.stack.write(dst, Value::from(self.global));
                }
                Instruction::LoadFunction { dst, func } => {
                    self.gc.collect_debt(&(&*self, instr, function));
                    let func = self.construct_function(func, &instr, function);
                    let res = Value::from(self.gc.allocate(func));
                    self.stack.write(dst, res);
                }
                Instruction::Move { dst, src } => self.stack.write(dst, self.stack.read(src)),
                Instruction::CreateObject { dst } => {
                    self.gc.collect_debt(&(&*self, instr, function));
                    let object = Object::new();
                    let res = Value::from(self.gc.allocate(object));
                    self.stack.write(dst, res)
                }
                Instruction::IndexAssign { obj, key, val } => {
                    let obj = self.stack.read(obj);
                    let key = self.stack.read(key);
                    let val = self.stack.read(val);

                    if obj.is_object() {
                        let obj = obj.unsafe_cast_object();
                        self.gc.write_barrier(obj);
                        obj.unsafe_index_set(key, val, self.context());
                    } else {
                        todo!()
                    }
                }
                Instruction::Index { dst, obj, key } => {
                    let obj = self.stack.read(obj);
                    let key = self.stack.read(key);
                    if obj.is_object() {
                        let res = obj.unsafe_cast_object().unsafe_index(key, self.context());
                        self.stack.write(dst, res)
                    } else {
                        todo!()
                    }
                }
                Instruction::Upvalue { dst, slot } => {
                    let value = function.as_vm_function().upvalues[slot as usize].read();
                    self.stack.write(dst, value);
                }

                Instruction::UpvalueAssign { src, slot } => {
                    let value = self.stack.read(src);
                    function.as_vm_function().upvalues[slot as usize].write(value);
                }

                Instruction::Add { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    if Self::both_int(left, right) {
                        let res =
                            Self::coerce_int(left.cast_int() as i64 + right.cast_int() as i64);
                        self.stack.write(dst, res);
                    } else {
                        let res = self.add(left, right);
                        self.stack.write(dst, res);
                    }
                }
                Instruction::Sub { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    if Self::both_int(left, right) {
                        let res =
                            Self::coerce_int(left.cast_int() as i64 - right.cast_int() as i64);
                        self.stack.write(dst, res);
                    } else {
                        let res = self.numeric_operator(left, right, NumericOperator::Sub);
                        self.stack.write(dst, res);
                    }
                }
                Instruction::Mul { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.numeric_operator(left, right, NumericOperator::Mul);
                    self.stack.write(dst, res);
                }
                Instruction::Div { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.numeric_operator(left, right, NumericOperator::Div);
                    self.stack.write(dst, res);
                }
                Instruction::Mod { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.numeric_operator(left, right, NumericOperator::Mod);
                    self.stack.write(dst, res);
                }
                Instruction::Pow { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.numeric_operator(left, right, NumericOperator::Pow);
                    self.stack.write(dst, res);
                }

                Instruction::ShiftLeft { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let left = self.convert_int(left);
                    let right = self.convert_int(right) as u32 % 32;
                    self.stack.write(dst, Value::from(left << right));
                }
                Instruction::ShiftRight { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let left = self.convert_int(left);
                    let right = self.convert_int(right) as u32 % 32;
                    self.stack.write(dst, Value::from(left >> right));
                }
                Instruction::ShiftUnsigned { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let left = self.convert_int(left) as u32;
                    let right = self.convert_int(right) as u32 % 32;
                    self.stack.write(dst, Value::from((left >> right) as i32));
                }
                Instruction::SEqual { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.strict_equal(left, right);
                    self.stack.write(dst, Value::from(res));
                }
                Instruction::SNotEqual { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = !self.strict_equal(left, right);
                    self.stack.write(dst, Value::from(res));
                }

                Instruction::IsNullish { dst, op } => {
                    let src = self.stack.read(op as u8);
                    let nullish = self.is_nullish(src);
                    self.stack.write(dst, Value::from(nullish))
                }
                Instruction::Not { dst, src } => {
                    let src = self.stack.read(src as u8);
                    let falsish = self.is_falsish(src);
                    self.stack.write(dst, Value::from(falsish))
                }
                Instruction::Negative { dst, op } => {
                    let src = self.stack.read(op);
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
                Instruction::Jump { tgt } => instr.jump(tgt),
                Instruction::JumpFalse { cond, tgt } => {
                    let cond = self.stack.read(cond);
                    if self.is_falsish(cond) {
                        instr.jump(tgt)
                    }
                }
                Instruction::JumpTrue { cond, tgt } => {
                    let cond = self.stack.read(cond);
                    if !self.is_falsish(cond) {
                        instr.jump(tgt)
                    }
                }
                Instruction::Push { src } => {
                    let src = self.stack.read(src);
                    self.stack.push(src);
                }

                Instruction::Call { dst, func } => {
                    let func = self.stack.read(func);
                    if func.is_function() {
                        if let Some(value) =
                            self.call(func.unsafe_cast_function(), &mut function, &mut instr, dst)
                        {
                            self.stack.write(dst, value);
                        }
                    } else {
                        todo!()
                    }
                }
                Instruction::ReturnUndefined { .. } => match self.stack.pop() {
                    Some(x) => {
                        instr = x.reader;
                        function = x.function;
                        self.stack.write(x.dst, Value::undefined());
                    }
                    None => return Ok(Value::undefined()),
                },
                Instruction::Return { ret } => {
                    let return_value = self.stack.read(ret);
                    match self.stack.pop() {
                        Some(x) => {
                            instr = x.reader;
                            function = x.function;
                            self.stack.write(x.dst, return_value);
                        }
                        None => return Ok(return_value),
                    }
                }
                x => panic!("invalid Instruction {}", x),
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

    pub unsafe fn coerce_string<'a>(&mut self, value: Value) -> Cow<'a, str> {
        if value.is_int() {
            Cow::Owned(value.cast_int().to_string())
        } else if value.is_float() {
            Cow::Owned(value.cast_float().to_string())
        } else if value.is_null() {
            Cow::Borrowed("null")
        } else if value.is_undefined() {
            Cow::Borrowed("undefined")
        } else if value.is_true() {
            Cow::Borrowed("true")
        } else if value.is_false() {
            Cow::Borrowed("false")
        } else if value.is_string() {
            Cow::Borrowed(value.unsafe_cast_string().ref_static().as_ref())
        } else if value.is_object() {
            Cow::Borrowed("[object Object]")
        } else if value.is_function() {
            Cow::Borrowed("[function Function]")
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
        op: NumericOperator,
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
            NumericOperator::Sub => left - right,
            NumericOperator::Mul => left * right,
            NumericOperator::Div => left / right,
            NumericOperator::Mod => left % right,
            NumericOperator::Pow => left.powf(right),
        };
        if res as i32 as f64 == res {
            Value::from(res as i32)
        } else {
            Value::from(res)
        }
    }

    #[inline]
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

    #[inline]
    fn coerce_int(int: i64) -> Value {
        if int as i32 as i64 == int {
            Value::from(int as i32)
        } else {
            Value::from(int as f64)
        }
    }

    pub(crate) unsafe fn construct_function(
        &mut self,
        function_id: u16,
        reader: &InstructionReader,
        function: Gc<Function>,
    ) -> Function {
        let bc_function = reader.function(function_id);
        let function = function.as_vm_function();
        let upvalues = bc_function
            .upvalues
            .iter()
            .copied()
            .map(|x| match x {
                Upvalue::Local(register) => self.stack.create_upvalue(register, &self.gc),
                Upvalue::Parent(slot) => {
                    debug_assert!(function.upvalues.len() > slot as usize);
                    *function.upvalues.get_unchecked(slot as usize)
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Function::from_vm(VmFunction {
            bc: reader.bc,
            function: function_id,
            upvalues,
        })
    }

    pub(crate) unsafe fn construct_function_root(
        &mut self,
        reader: &InstructionReader,
    ) -> Gc<Function> {
        let bc_function = reader.function(0);
        debug_assert!(bc_function.upvalues.is_empty());
        self.gc.allocate(Function::from_vm(VmFunction {
            bc: reader.bc,
            function: 0,
            upvalues: Box::new([]),
        }))
    }

    unsafe fn call(
        &mut self,
        call_function: Gc<Function>,
        current_function: &mut Gc<Function>,
        instr: &mut InstructionReader,
        dst: u8,
    ) -> Option<Value> {
        match call_function.kind {
            FunctionKind::Vm(ref func) => {
                let bc = func.bc;
                let bc_function = &bc.functions[func.function as usize];
                self.stack
                    .enter_call(bc_function.registers, dst, *instr, *current_function);
                *current_function = call_function;
                *instr = InstructionReader::from_bc(bc, func.function);
                None
            }
            FunctionKind::Mutable(ref x) => {
                self.stack.enter(0);
                let ctx = self.context();
                let args = self.arguments();
                let res = x.try_borrow_mut().expect(RECURSIVE_FUNC_PANIC)(ctx, args);
                self.stack.pop();
                Some(res.unbind())
            }
            FunctionKind::Native(ref x) => {
                self.stack.enter(0);
                let ctx = self.context();
                let args = self.arguments();
                let res = (*x)(ctx, args);
                self.stack.pop();
                Some(res.unbind())
            }
        }
    }
}
