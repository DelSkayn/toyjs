use super::RunningExecution;
use crate::{
    bytecode::{op, Bytecode, Instruction, RuntimeFunction},
    environment::Environment,
    function::Function,
    gc::{Ctx, Gc, GcArena, Trace},
    object::Object,
    stack::Stack,
    value::{self, JSValue},
};
use std::{cell::RefCell, mem, num, ptr, rc::Rc};

impl<'a, R: Trace> RunningExecution<'a, R> {
    pub unsafe fn run(&mut self) -> JSValue {
        loop {
            match self.execute_instruction() {
                Some(x) => return x,
                None => {}
            }
        }
    }

    #[inline(always)]
    pub unsafe fn execute_instruction(&mut self) -> Option<JSValue> {
        match self.read_u8() {
            op::CreateObject => {
                let a = self.read_u8();
                let _d = self.read_u16();
                let v = JSValue::from(self.gc.allocate(Object::new()));
                self.write(a, v);
            }
            op::CreateFunction => {
                let a = self.read_u8();
                let d = self.read_u16();
                let function_id = if d == u16::MAX {
                    self.read_u32()
                } else {
                    d as u32
                };
            }

            op::LoadGlobal => {
                let a = self.read_u8();
                let _d = self.read_u16();
                let v = JSValue::from(self.global);
                self.write(a, v);
            }
            op::LoadData => {
                let a = self.read_u8();
                let d = self.read_u16();
                let d = if d == u16::MAX {
                    self.read_u32()
                } else {
                    d as u32
                };
                self.write(a, self.data[d as usize].clone())
            }
            op::LoadString => {
                let a = self.read_u8();
                let d = self.read_u16();
                let d = if d == u16::MAX {
                    self.read_u32()
                } else {
                    d as u32
                };
                let res = JSValue::from(self.gc.allocate(self.strings[d as usize].clone()));
                self.write(a, res)
            }

            op::StackPush => {
                let a = self.read_u8();
                let _d = self.read_u16();
                self.stack.push_frame(a);
            }

            op::StackPop => {
                let a = self.read_u8();
                let _d = self.read_u16();
                self.stack.pop_frame(a);
            }

            op::Index => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                if b.is_undefined() {
                    panic!("errors not yet implemented")
                }
                //TODO strings,
                if !b.is_object() {
                    self.write(a, JSValue::undefined())
                } else {
                    let c = self.read(c);
                    let value = b.into_object().get(c, self);
                    self.write(a, value)
                }
            }
            op::IndexAssign => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let a = self.read(a);
                if a.is_object() {
                    let b = self.read(b);
                    let c = self.read(c);
                    let mut borrow = a.into_object().get_ptr(&self.gc);
                    (*borrow).set(b, c, self);
                }
            }

            op::GetEnv => {
                let a = self.read_u8();
                let d = self.read_u16();
                let parent = self.environment.lookup_parent(d);
                self.write(a, JSValue::from_const_ptr(Rc::into_raw(parent) as *const _))
            }
            op::EnvAssign => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let env = self.read(a).into_const_ptr() as *const Environment;
                let b = self.read(b);
                (*env).set(c as u32, b);
            }
            op::EnvIndex => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let env = self.read(b).into_const_ptr() as *const Environment;
                let v = (*env).get(c as u32);
                self.write(a, v);
            }

            op::Add => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();

                let b = self.read(b);
                let c = self.read(c);

                if Self::both_int(b, c) {
                    let b = b.into_int() as i64;
                    let c = c.into_int() as i64;
                    let res = b + c;
                    let res = if (res as i32) as i64 == res {
                        JSValue::from(res as i32)
                    } else {
                        JSValue::from(res as f64)
                    };
                    self.write(a, res);
                } else {
                    let res = self.bin_addition(b, c);
                    self.write(a, res);
                }
            }
            op::Sub => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();

                let b = self.read(b);
                let c = self.read(c);

                if Self::both_int(b, c) {
                    let b = b.into_int() as i64;
                    let c = c.into_int() as i64;
                    let res = b - c;
                    let res = if (res as i32) as i64 == res {
                        JSValue::from(res as i32)
                    } else {
                        JSValue::from(res as f64)
                    };
                    self.write(a, res);
                } else {
                    let res = Self::bin_arithmatic(b, c, op::Sub);
                    self.write(a, res);
                }
            }
            op::Mul => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                let res = Self::bin_arithmatic(b, c, op::Mul);
                self.write(a, res);
            }
            op::Div => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                let res = Self::bin_arithmatic(b, c, op::Div);
                self.write(a, res);
            }
            op::Pow => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                let res = Self::bin_arithmatic(b, c, op::Pow);
                self.write(a, res);
            }
            op::Mod => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                let res = Self::bin_arithmatic(b, c, op::Mod);
                self.write(a, res);
            }
            op::BinaryOr => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = Self::convert_int(self.read(b));
                let c = Self::convert_int(self.read(c));
                self.write(a, JSValue::from(b | c))
            }
            op::BinaryAnd => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = Self::convert_int(self.read(b));
                let c = Self::convert_int(self.read(c));
                self.write(a, JSValue::from(b & c))
            }
            op::BinaryXor => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = Self::convert_int(self.read(b));
                let c = Self::convert_int(self.read(c));
                self.write(a, JSValue::from(b ^ c))
            }
            op::ShiftLeft => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = Self::convert_int(self.read(b));
                let c = Self::convert_int(self.read(c)) as u32;
                let c = (c & 0x1f) as i32;
                self.write(a, JSValue::from((b << c)))
            }
            op::ShiftRight => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = Self::convert_int(self.read(b));
                let c = Self::convert_int(self.read(c)) as u32;
                let c = (c & 0x1f) as i32;
                self.write(a, JSValue::from((b >> c)))
            }
            op::ShiftUnsigned => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = Self::convert_int(self.read(b)) as u32;
                let c = Self::convert_int(self.read(c)) as u32;
                let c = (c & 0x1f);
                self.write(a, JSValue::from((b >> c) as i32))
            }

            op::Equal => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                self.write(a, JSValue::from(Self::eq(b, c)))
            }
            op::NotEqual => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                self.write(a, JSValue::from(!Self::eq(b, c)))
            }
            op::StrictEqual => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                self.write(a, JSValue::from(Self::strict_eq(b, c)))
            }
            op::StrictNotEqual => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                self.write(a, JSValue::from(!Self::strict_eq(b, c)))
            }

            op::Less => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                if Self::both_int(b, c) {
                    self.write(a, JSValue::from(b.into_int() < c.into_int()))
                } else {
                    self.write(a, JSValue::from(Self::less(b, c)))
                }
            }
            op::LessEqual => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                if Self::both_int(b, c) {
                    self.write(a, JSValue::from(b.into_int() <= c.into_int()))
                } else {
                    self.write(a, JSValue::from(!Self::less(c, b)))
                }
            }
            op::Greater => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                if Self::both_int(b, c) {
                    self.write(a, JSValue::from(b.into_int() > c.into_int()))
                } else {
                    self.write(a, JSValue::from(Self::less(c, b)))
                }
            }
            op::GreaterEqual => {
                let a = self.read_u8();
                let b = self.read_u8();
                let c = self.read_u8();
                let b = self.read(b);
                let c = self.read(c);
                if Self::both_int(b, c) {
                    self.write(a, JSValue::from(b.into_int() >= c.into_int()))
                } else {
                    self.write(a, JSValue::from(!Self::less(b, c)))
                }
            }

            op::ToNumber => {
                let a = self.read_u8();
                let d = self.read_u16();
                let d = self.read(d as u8);
                let d = Self::float_to_val(Self::convert_float(d));
                self.write(a, d);
            }
            op::Negative => {
                let a = self.read_u8();
                let d = self.read_u16();
                let d = self.read(d as u8);
                let d = Self::float_to_val(-Self::convert_float(d));
                self.write(a, d);
            }
            op::Jump => {
                self.read_u8();
                let jump = self.read_u16() as i16;
                self.jump(jump as i32);
            }
            op::JumpTrue => {
                let cond = self.read_u8();
                let jump = self.read_u16() as i16;
                let cond = self.read(cond);
                if Self::convert_bool(cond) {
                    self.jump(jump as i32);
                }
            }
            op::JumpFalse => {
                let cond = self.read_u8();
                let jump = self.read_u16() as i16;
                let cond = self.read(cond);
                if !Self::convert_bool(cond) {
                    self.jump(jump as i32);
                }
            }
            op::Return => {
                let a = self.read_u8();
                let d = self.read_u16();
                let d = self.read(d as u8);
                self.stack.pop_frame(a);
                self.gc_collect(Some(d));
                return Some(d);
            }
            op::ReturnUndefined => {
                let a = self.read_u8();
                let _ = self.read_u16();
                self.stack.pop_frame(a);
                self.gc_collect(None);
                return Some(JSValue::undefined());
            }
            x => panic!("invalid op code: {:?}", x),
        }
        None
    }
}
