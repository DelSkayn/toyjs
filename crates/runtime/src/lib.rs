#![allow(unused)]

pub const MAX_REGISTERS: u8 = 255;

pub mod gc;
use gc::{Ctx, Gc, GcArena};

use std::{cell::RefCell, mem, num, ptr, rc::Rc};
#[macro_use]
mod macros;

pub mod bytecode;
pub mod environment;
pub mod object;
mod stack;
pub mod value;

use bytecode::{op, Bytecode, Instruction};
use environment::Environment;
use object::Object;
pub use stack::Stack;
use value::JSValue;

pub struct Root<'a> {
    stack: &'a Stack,
    global: Gc<Object>,
    environment: Rc<Environment>,
    ret: Option<JSValue>,
}

unsafe impl<'a> gc::Trace for Root<'a> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: Ctx) {
        ctx.mark(self.global);
        self.stack.trace(ctx);
        self.environment.trace(ctx);
        self.ret.as_ref().map(|x| x.trace(ctx));
    }
}

pub struct ExecutionContext<'a> {
    gc: &'a GcArena,
    global: Gc<Object>,
    environment: Rc<Environment>,
    instructions: &'a [bytecode::Instruction],
    instr_ptr: *const u8,
    data: &'a [JSValue],
    strings: &'a [String],
    stack: &'a mut Stack,
}

impl<'a> ExecutionContext<'a> {
    pub fn new(
        gc: &'a GcArena,
        global: Gc<Object>,
        environment: Rc<Environment>,
        data: &'a Bytecode,
        stack: &'a mut Stack,
    ) -> Self {
        println!("{}", data);
        ExecutionContext {
            gc,
            global,
            environment,
            instructions: &data.instructions,
            instr_ptr: data.instructions.as_ptr() as *mut _,
            data: &data.data,
            strings: &data.strings,
            stack,
        }
    }

    fn gc_collect(&self, ret: Option<JSValue>) -> Option<JSValue> {
        let root = Root {
            stack: &self.stack,
            global: self.global,
            environment: self.environment.clone(),
            ret,
        };
        unsafe {
            self.gc.collect_debt(&root);
        }
        return root.ret;
    }

    #[inline]
    unsafe fn write(&mut self, index: u8, value: JSValue) {
        debug_assert!(index < MAX_REGISTERS);
        self.stack.set(index, value)
    }

    #[inline]
    unsafe fn read(&self, index: u8) -> JSValue {
        self.stack.get(index)
    }

    unsafe fn read_u8(&mut self) -> u8 {
        debug_assert!(self.instr_ptr >= self.instructions.as_ptr() as *mut _);
        debug_assert!(
            self.instr_ptr < self.instructions.as_ptr().add(self.instructions.len()) as *mut _
        );
        let res = self.instr_ptr.read();
        self.instr_ptr = self.instr_ptr.add(mem::size_of::<u8>());
        res
    }

    unsafe fn read_u16(&mut self) -> u16 {
        debug_assert!(self.instr_ptr > self.instructions.as_ptr() as *mut _);
        debug_assert!(
            self.instr_ptr < self.instructions.as_ptr().add(self.instructions.len()) as *mut _
        );
        debug_assert_eq!(self.instr_ptr.align_offset(mem::align_of::<u16>()), 0);
        let res = (self.instr_ptr as *mut u16).read();
        self.instr_ptr = self.instr_ptr.add(mem::size_of::<u16>());
        res
    }

    unsafe fn read_u32(&mut self) -> u32 {
        debug_assert!(self.instr_ptr > self.instructions.as_ptr() as *mut _);
        debug_assert!(
            self.instr_ptr < self.instructions.as_ptr().add(self.instructions.len()) as *mut _
        );
        debug_assert_eq!(self.instr_ptr.align_offset(mem::align_of::<u32>()), 0);
        let res = (self.instr_ptr as *mut u32).read();
        self.instr_ptr = self.instr_ptr.add(mem::size_of::<u32>());
        res
    }

    unsafe fn jump(&mut self, jump: i32) {
        let target = jump as isize * mem::size_of::<Instruction>() as isize;
        self.instr_ptr = self.instr_ptr.offset(target);
    }

    pub unsafe fn run(&mut self) -> JSValue {
        loop {
            match self.read_u8() {
                op::CreateObject => {
                    let a = self.read_u8();
                    let _d = self.read_u16();
                    let v = JSValue::from(self.gc.allocate(Object::new()));
                    self.write(a, v);
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
                        let value = b.into_object().get(c);
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
                        let mut borrow = a.into_object().get_ptr(self.gc);
                        (*borrow).set(b, c);
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
                        self.write(a, JSValue::from(b.into_int() < c.into_int()))
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
                        self.write(a, JSValue::from(b.into_int() < c.into_int()))
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
                        self.write(a, JSValue::from(b.into_int() < c.into_int()))
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
                    return self.gc_collect(Some(d)).unwrap();
                }
                op::ReturnUndefined => {
                    let a = self.read_u8();
                    let _ = self.read_u16();
                    self.stack.pop_frame(a);
                    self.gc_collect(None);
                    return JSValue::undefined();
                }
                x => panic!("invalid op code: {:?}", x),
            }
        }
    }

    #[inline]
    unsafe fn both_int(b: JSValue, c: JSValue) -> bool {
        b.tag() == value::TAG_INT && c.tag() == value::TAG_INT
    }

    #[inline]
    fn float_to_val(val: f64) -> JSValue {
        if (val as i32 as f64).to_bits() == val.to_bits() {
            JSValue::from(val as i32)
        } else {
            JSValue::from(val)
        }
    }

    unsafe fn convert_int(val: JSValue) -> i32 {
        if val.is_int() {
            return val.into_int();
        }
        let val = Self::convert_float(val);
        let val = match val.classify() {
            num::FpCategory::Nan | num::FpCategory::Infinite | num::FpCategory::Zero => return 0,
            _ => val,
        };
        val as i32
    }

    unsafe fn convert_float(val: JSValue) -> f64 {
        match val.tag() {
            value::TAG_INT => val.into_int() as f64,
            value::TAG_STRING => {
                let s = val.into_string();
                let s = s.trim();
                if s.is_empty() {
                    0.0
                } else {
                    s.parse::<f64>().unwrap_or(f64::NAN)
                }
            }
            value::TAG_OBJECT => f64::NAN,
            value::TAG_BASE => match val.0.bits {
                value::VALUE_TRUE => 1.0,
                value::VALUE_FALSE => 0.0,
                value::VALUE_UNDEFINED => f64::NAN,
                value::VALUE_NULL => f64::NAN,
                _ => panic!("invalid js value!"),
            },
            value::TAG_BIGINT => todo!(),
            value::TAG_SYMBOL => todo!(),
            value::TAG_FUNCTION => todo!(),
            _ => val.into_float(),
        }
    }

    unsafe fn convert_string(val: JSValue) -> String {
        match val.tag() {
            value::TAG_STRING => (*val.into_string()).clone(),
            value::TAG_INT => val.into_int().to_string(),
            value::TAG_BASE => match val.0.bits {
                value::VALUE_NULL => "null".to_string(),
                value::VALUE_TRUE => "true".to_string(),
                value::VALUE_FALSE => "false".to_string(),
                value::VALUE_UNDEFINED => "undefined".to_string(),
                _ => panic!("invalid jsvalue"),
            },
            value::TAG_OBJECT => "[object Object]".to_string(),
            value::TAG_SYMBOL => todo!(),
            value::TAG_BIGINT => todo!(),
            value::TAG_FUNCTION => todo!(),
            _ => val.into_float().to_string(),
        }
    }

    unsafe fn convert_bool(val: JSValue) -> bool {
        match val.tag() {
            value::TAG_STRING => !val.into_string().is_empty(),
            value::TAG_INT => val.into_int() != 0,
            value::TAG_OBJECT => true,
            value::TAG_BASE => match val.0.bits {
                value::VALUE_NULL => false,
                value::VALUE_TRUE => true,
                value::VALUE_FALSE => false,
                value::VALUE_UNDEFINED => false,
                _ => panic!("invalid jsvalue"),
            },
            value::TAG_SYMBOL => todo!(),
            value::TAG_BIGINT => todo!(),
            value::TAG_FUNCTION => todo!(),
            _ => {
                let v = val.into_float();
                // Comparison is done this way to handle NaN correctly.
                // as NaN != 0.0 returns true but NaN > 0.0 || NaN < 0.0 returns false
                v > 0.0 || v < 0.0
            }
        }
    }

    unsafe fn bin_addition(&self, a: JSValue, b: JSValue) -> JSValue {
        if a.is_string() || b.is_string() {
            let mut s = a.into_string().to_string();
            s.push_str(&b.into_string());
            return JSValue::from(self.gc.allocate(s));
        }
        Self::bin_arithmatic(a, b, op::Add)
    }

    unsafe fn bin_arithmatic(a: JSValue, b: JSValue, op: u8) -> JSValue {
        let a = Self::convert_float(a);
        let b = Self::convert_float(b);

        let res = match op {
            op::Add => a + b,
            op::Sub => a + (-b),
            op::Mul => a * b,
            op::Div => a / b,
            op::Mod => a % b,
            op::Pow => a.powf(b),
            _ => panic!("invalid op"),
        };
        Self::float_to_val(res)
    }

    unsafe fn eq(a: JSValue, b: JSValue) -> bool {
        if a.is_float() {
            return a.into_float() == Self::convert_float(b);
        }
        if a.is_nullish() && b.is_nullish() {
            return true;
        }

        let a_tag = a.tag();
        let b_tag = b.tag();
        if a_tag == b_tag {
            match a_tag {
                value::TAG_INT => return a.0.bits == b.0.bits,
                value::TAG_BASE => {
                    if a.0.bits == b.0.bits {
                        return true;
                    }
                    if a.is_bool() {
                        let a_bool = a.into_bool();
                        let b_num = Self::convert_float(b);
                        return if a_bool { b_num == 1.0 } else { b_num == 0.0 };
                    }
                    panic!("invalid tag recieved");
                }
                value::TAG_STRING => return *a.into_string() == *b.into_string(),
                value::TAG_OBJECT => {
                    return ptr::eq(Gc::into_raw(a.into_object()), Gc::into_raw(b.into_object()))
                }
                _ => unreachable!(),
            }
        }

        match a_tag {
            value::TAG_INT => a.into_int() as f64 == Self::convert_float(b),
            value::TAG_OBJECT => todo!(),
            value::TAG_STRING => Self::convert_float(a) == Self::convert_float(b),
            _ => panic!(),
        }
    }

    unsafe fn strict_eq(a: JSValue, b: JSValue) -> bool {
        if a.is_float() && b.is_float() {
            return a.into_float() == b.into_float();
        }
        let a_tag = a.tag();
        if a_tag != b.tag() {
            return false;
        }
        match a_tag {
            value::TAG_INT | value::TAG_BASE => a.0.bits == b.0.bits,
            value::TAG_STRING => *a.into_string() == *b.into_string(),
            value::TAG_OBJECT => {
                ptr::eq(Gc::into_raw(a.into_object()), Gc::into_raw(b.into_object()))
            }
            _ => todo!(),
        }
    }

    unsafe fn less(a: JSValue, b: JSValue) -> bool {
        if a.is_string() && b.is_string() {
            let a_str = a.into_string();
            let b_str = b.into_string();
            if a_str.len() != b_str.len() {
                if a_str.starts_with(&*b_str) {
                    return false;
                }
                if b_str.starts_with(&*a_str) {
                    return true;
                }
            }
            for (a, b) in a_str.chars().zip(b_str.chars()) {
                if a != b {
                    return a < b;
                }
            }
            return false;
        }
        let a_num = Self::convert_float(a);
        let b_num = Self::convert_float(b);
        a_num < b_num
    }
}
