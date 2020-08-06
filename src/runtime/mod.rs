//! The runtime executing the code.
pub mod value;
pub use value::JSValue;
pub mod bc;
use bc::Bytecode;
pub mod object;
use object::Object;
pub mod rc;
use rc::ManualRc;
pub mod string;
use crate::util::{Offset, OffsetExt};
use bc::{op, DataValue, Instruction};
use std::{alloc, cell::UnsafeCell, f64, mem, num, ptr};
use string::StringRc;

pub const NUM_REGISTERS: usize = 8;

pub struct Runtime<'a> {
    stack: ptr::NonNull<JSValue>,
    size: Offset<JSValue>,
    code: &'a [Instruction],
    data: &'a [DataValue],
    code_ptr: *const u8,
    frame_ptr: *mut JSValue,
    stack_ptr: *mut JSValue,
    regs: [JSValue; NUM_REGISTERS],
    global: object::ObjectRc,
}

impl<'a> Drop for Runtime<'a> {
    fn drop(&mut self) {
        let layout = self.size.to_layout();
        unsafe { alloc::dealloc(self.stack.as_ptr().cast(), layout) }
    }
}

impl<'a> Runtime<'a> {
    pub unsafe fn new(code: &'a Bytecode) -> Self {
        debug_assert_eq!(mem::size_of::<u64>(), mem::size_of::<JSValue>());
        let data = &code.data;
        let code = &code.instructions;
        let stack = ptr::NonNull::dangling();
        let regs = mem::zeroed();
        Runtime {
            size: Offset::zero(),
            code,
            data,
            code_ptr: code.as_ptr() as *const _,
            frame_ptr: stack.as_ptr(),
            stack_ptr: stack.as_ptr(),
            stack,
            regs,
            global: rc::ManualRc::new(UnsafeCell::new(Object::new())),
        }
    }

    #[cfg(debug_assertions)]
    fn check_bounds<T>(&self) {
        assert!(
            self.code.as_ptr().cast::<u8>().offset_to(self.code_ptr)
                < Offset::<u8>::new(self.code.len() - mem::size_of::<T>())
        )
    }

    #[inline(always)]
    unsafe fn read_u8(&mut self) -> u8 {
        #[cfg(debug_assertions)]
        self.check_bounds::<u8>();
        let res = self.code_ptr.read();
        self.code_ptr = self.code_ptr.add(mem::size_of::<u8>());
        res
    }

    #[inline(always)]
    unsafe fn read_u16(&mut self) -> u16 {
        debug_assert_eq!(self.code_ptr.align_offset(mem::align_of::<u16>()), 0);
        #[cfg(debug_assertions)]
        self.check_bounds::<u16>();
        let res = (self.code_ptr as *const u16).read();
        self.code_ptr = self.code_ptr.add(mem::size_of::<u16>());
        res
    }

    #[inline(always)]
    unsafe fn read_u32(&mut self) -> u32 {
        debug_assert_eq!(self.code_ptr.align_offset(mem::align_of::<u32>()), 0);
        #[cfg(debug_assertions)]
        self.check_bounds::<u32>();
        let res = (self.code_ptr as *const u32).read();
        self.code_ptr = self.code_ptr.add(mem::size_of::<u32>());
        res
    }

    #[inline(always)]
    unsafe fn jump(&mut self, pc: u32) {
        self.code_ptr = Offset::<Instruction>::new(pc as usize)
            .apply_to_const(self.code.as_ptr())
            .cast::<u8>();
    }

    #[inline(always)]
    unsafe fn get(&mut self, idx: u8) -> &mut JSValue {
        debug_assert!(idx < NUM_REGISTERS as u8);
        self.regs.get_unchecked_mut(idx as usize)
    }

    pub unsafe fn run_unsafe(&mut self) -> Option<JSValue> {
        let res = self.run_loop();
        self.unwind();
        res
    }

    unsafe fn unwind(&mut self) {
        self.stack_ptr = self.stack_ptr.sub(1);
        while self.frame_ptr >= self.stack.as_ptr() {
            (*self.frame_ptr).drop();
            self.frame_ptr = self.frame_ptr.sub(1);
        }
    }

    unsafe fn alloc(&mut self, size: usize) {
        let offset = self.stack_ptr.offset_to(self.stack.as_ptr()) + Offset::new(size);
        if offset.next_power_of_two() <= self.size {
            return;
        }
        if self.size == Offset::zero() {
            let new_size = offset.next_power_of_two();
            let layout = new_size.to_layout();
            self.stack = ptr::NonNull::new_unchecked(alloc::alloc(layout).cast());
            self.stack_ptr = self.stack.as_ptr();
            self.frame_ptr = self.stack.as_ptr();
            return;
        }
        let layout = self.size.to_layout();
        let new_size = offset.next_power_of_two();
        let ptr =
            alloc::realloc(self.stack.as_ptr().cast(), layout, new_size.as_size_bytes()).cast();
        self.stack_ptr = self.stack.as_ptr().offset_to(self.stack_ptr).apply_to(ptr);
        self.frame_ptr = self.stack.as_ptr().offset_to(self.frame_ptr).apply_to(ptr);
        self.stack = ptr::NonNull::new_unchecked(ptr.cast());
        self.size = new_size;
    }

    unsafe fn push(&mut self, size: usize) {
        debug_assert!(size > 1);
        self.alloc(size);
        self.stack_ptr.write(JSValue {
            bits: self.stack_ptr as u64 - self.frame_ptr as u64,
        });
        self.frame_ptr = self.stack_ptr;
        self.stack_ptr = self.stack_ptr.add(1);
        self.stack_ptr = self.stack_ptr.add(size);
    }

    unsafe fn pop(&mut self) {
        debug_assert!(self.stack_ptr as usize != self.frame_ptr as usize);
        self.stack_ptr = self.stack_ptr.sub(1);
        while self.frame_ptr < self.stack_ptr {
            self.stack_ptr.read().drop();
            self.stack_ptr = self.stack_ptr.sub(1);
        }
        let val = self.stack_ptr.read().bits;
        self.frame_ptr = (self.frame_ptr as u64 - val) as *mut _;
    }

    unsafe fn stack(&mut self, idx: usize) -> &mut JSValue {
        debug_assert!(Offset::new(idx) < self.frame_ptr.offset_to(self.stack_ptr) - Offset::new(1));
        &mut *self.stack_ptr.sub(idx + 1)
    }

    unsafe fn run_loop(&mut self) -> Option<JSValue> {
        loop {
            let op = self.read_u8();
            match op {
                op::LGB => {
                    let op_a = self.read_u8();
                    self.read_u16();
                    *self.get(op_a) = JSValue::from(self.global);
                }

                op::LD => {
                    let op_a = self.read_u8();
                    let op_d = self.read_u16();
                    let idx = if op_d == 0xfff {
                        self.read_u32() as usize
                    } else {
                        op_d as usize
                    };
                    *self.get(op_a) = *self.stack(idx);
                }
                op::ST => {
                    let op_a = self.read_u8();
                    let op_d = self.read_u16();
                    let idx = if op_d == 0xfff {
                        self.read_u32() as usize
                    } else {
                        op_d as usize
                    };
                    *self.stack(idx) = *self.get(op_a);
                }
                op::PUSH => {
                    self.read_u8();
                    let op_d = self.read_u16();
                    let idx = if op_d == 0xfff {
                        self.read_u32() as usize
                    } else {
                        op_d as usize
                    };
                    self.push(idx)
                }
                op::POP => {
                    self.read_u8();
                    self.read_u16();
                    self.pop();
                }
                op::OSET => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let key = *self.get(op_b);
                    let val = *self.get(op_c);
                    let obj = if op_a == 0xff {
                        JSValue::from(self.global)
                    } else {
                        *self.get(op_a)
                    };
                    let key = Self::as_string(key);
                    (*obj.into_object().value().get())
                        .set(key, val.clone())
                        .map(|x| x.drop());
                }
                op::OGET => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let obj = if op_b == 0xff {
                        JSValue::from(self.global)
                    } else {
                        *self.get(op_b)
                    };
                    let key = *self.get(op_c);
                    let val = (*obj.into_object().value().get()).get(&Self::as_string(key));
                    *self.get(op_a) = val;
                }
                op::CLD => {
                    let dst = self.read_u8();
                    let val = self.read_u16();
                    let idx = if val != 0xffff {
                        val as usize
                    } else {
                        self.read_u32() as usize
                    };
                    let data = match self.data[idx].clone() {
                        DataValue::String(x) => JSValue::from(ManualRc::new(x)),
                        DataValue::Direct(x) => x,
                    };
                    (*self.get(dst)) = data;
                }
                op::MOV => {
                    let dst = self.read_u8();
                    let src = self.read_u16();
                    *self.get(dst) = *self.get(src as u8);
                }
                op::ADD => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let val1 = *self.get(op_b);
                    let val2 = *self.get(op_c);
                    if Self::both_int(val1, val2) {
                        let val1 = val1.into_int() as i64;
                        let val2 = val2.into_int() as i64;
                        let res = val1 + val2;
                        let res = if (res as i32) as i64 != res {
                            JSValue::from(res as f64)
                        } else {
                            JSValue::from(res as i32)
                        };
                        *self.get(op_a) = res;
                    } else {
                        let res = Self::bin_addition(val1, val2);
                        *self.get(op_a) = res;
                    }
                }
                op::SUB => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let val1 = *self.get(op_b);
                    let val2 = *self.get(op_c);
                    if Self::both_int(val1, val2) {
                        let val1 = val1.into_int() as i64;
                        let val2 = val2.into_int() as i64;
                        let res = val1 - val2;
                        let res = if (res as i32) as i64 != res {
                            JSValue::from(res as f64)
                        } else {
                            JSValue::from(res as i32)
                        };
                        *self.get(op_a) = res;
                    } else {
                        let res = Self::bin_arithmatic(val1, val2, op::SUB);
                        *self.get(op_a) = res;
                    }
                }
                op::MUL => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let val1 = *self.get(op_b);
                    let val2 = *self.get(op_c);
                    let res = Self::bin_arithmatic(val1, val2, op::MUL);
                    *self.get(op_a) = res;
                }
                op::DIV => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let val1 = *self.get(op_b);
                    let val2 = *self.get(op_c);
                    let res = Self::bin_arithmatic(val1, val2, op::DIV);
                    *self.get(op_a) = res;
                }
                op::POW => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let val1 = *self.get(op_b);
                    let val2 = *self.get(op_c);
                    let res = Self::bin_arithmatic(val1, val2, op::POW);
                    *self.get(op_a) = res;
                }
                op::MOD => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let val1 = *self.get(op_b);
                    let val2 = *self.get(op_c);
                    let res = Self::bin_arithmatic(val1, val2, op::MOD);
                    *self.get(op_a) = res;
                }
                op::BAND => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = Self::as_i32(*self.get(op_b));
                    let c = Self::as_i32(*self.get(op_c));
                    *self.get(op_a) = JSValue::from(b & c);
                }
                op::BOR => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = Self::as_i32(*self.get(op_b));
                    let c = Self::as_i32(*self.get(op_c));
                    *self.get(op_a) = JSValue::from(b | c);
                }
                op::BXOR => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = Self::as_i32(*self.get(op_b));
                    let c = Self::as_i32(*self.get(op_c));
                    *self.get(op_a) = JSValue::from(b ^ c);
                }
                op::POS => {
                    let op_a = self.read_u8();
                    let op_d = self.read_u16();
                    let val = *self.get(op_d as u8);
                    let val = Self::float_to_val(Self::as_float(val));
                    *self.get(op_a) = val;
                }
                op::NEG => {
                    let op_a = self.read_u8();
                    let op_d = self.read_u16();
                    let val = *self.get(op_d as u8);
                    let val = Self::float_to_val(-Self::as_float(val));
                    *self.get(op_a) = val;
                }
                op::BOOL => {
                    let op_a = self.read_u8();
                    let op_d = self.read_u16();
                    let val = *self.get(op_d as u8);
                    let val = Self::as_bool(val);
                    *self.get(op_a) = JSValue::from(val);
                }
                op::ISNUL => {
                    let op_a = self.read_u8();
                    let op_d = self.read_u16();
                    let val = *self.get(op_d as u8);
                    let val = val.is_null() || val.is_undefined();
                    *self.get(op_a) = JSValue::from(val);
                }
                op::SHL => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = Self::as_i32(*self.get(op_b));
                    let c = Self::as_u32(*self.get(op_c));
                    let c = (c & 0x1f) as i32;
                    *self.get(op_a) = JSValue::from(b << c);
                }
                op::SHR => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = Self::as_i32(*self.get(op_b));
                    let c = Self::as_u32(*self.get(op_c));
                    let c = (c & 0x1f) as i32;
                    *self.get(op_a) = JSValue::from((b >> c) as i32);
                }
                op::USR => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = Self::as_i32(*self.get(op_b)) as u32;
                    let mut c = Self::as_u32(*self.get(op_c));
                    c &= 0x1f;
                    *self.get(op_a) = JSValue::from((b >> c) as i32);
                }
                op::SEQ => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = *self.get(op_b);
                    let c = *self.get(op_c);
                    *self.get(op_a) = JSValue::from(Self::strict_eq(b, c))
                }
                op::EQ => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = *self.get(op_b);
                    let c = *self.get(op_c);
                    *self.get(op_a) = JSValue::from(Self::eq(b, c))
                }
                op::SNEQ => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = *self.get(op_b);
                    let c = *self.get(op_c);
                    *self.get(op_a) = JSValue::from(!Self::strict_eq(b, c))
                }
                op::NEQ => {
                    let op_a = self.read_u8();
                    let op_b = self.read_u8();
                    let op_c = self.read_u8();
                    let b = *self.get(op_b);
                    let c = *self.get(op_c);
                    *self.get(op_a) = JSValue::from(!Self::eq(b, c))
                }
                op::JCO => {
                    let op_cond = self.read_u8();
                    let cond = *self.get(op_cond);
                    let neg = self.read_u16();
                    if Self::as_bool(cond) as u16 != neg {
                        let tar = self.read_u32();
                        self.jump(tar);
                    } else {
                        self.read_u32();
                    }
                }
                op::J => {
                    self.read_u8();
                    self.read_u16();
                    let tar = self.read_u32();
                    self.jump(tar);
                }
                op::RET => {
                    let val = self.read_u8();
                    let val = *self.get(val);
                    val.incr();
                    self.read_u16();
                    return Some(val);
                }
                op::RETU => {
                    self.read_u8();
                    self.read_u16();
                    return Some(JSValue::undefined());
                }
                _ => panic!("invalid instruction"),
            }
        }
    }

    unsafe fn as_i32(val: JSValue) -> i32 {
        let val = Self::as_float(val);
        let val = match val.classify() {
            num::FpCategory::Nan | num::FpCategory::Infinite | num::FpCategory::Zero => return 0,
            _ => val,
        };
        val as i32
    }

    unsafe fn as_u32(val: JSValue) -> u32 {
        let val = Self::as_float(val);
        let val = match val.classify() {
            num::FpCategory::Nan | num::FpCategory::Infinite | num::FpCategory::Zero => return 0,
            _ => val,
        };
        val as u32
    }

    #[inline]
    fn float_to_val(val: f64) -> JSValue {
        if val as i32 as f64 == val {
            JSValue::from(val as i32)
        } else {
            JSValue::from(val)
        }
    }

    unsafe fn as_bool(val: JSValue) -> bool {
        match val.tag() {
            value::TAG_STRING => val.into_string().value().len() != 0,
            value::TAG_INT => val.into_int() != 0,
            value::TAG_BOOL => val.into_bool(),
            value::TAG_OBJECT => true,
            value::TAG_UNDEFINED => false,
            value::TAG_NULL => false,
            value::TAG_AVAILABLE_5 => panic!("invalid value tag"),
            _ => {
                let v = val.into_float();
                // comparison is done this way to handle NaN correctly.
                v > 0.0 || v < 0.0
            }
        }
    }

    unsafe fn as_string(val: JSValue) -> String {
        match val.tag() {
            value::TAG_STRING => val.into_string().value().clone(),
            value::TAG_INT => val.into_int().to_string(),
            value::TAG_BOOL => val.into_bool().to_string(),
            value::TAG_OBJECT => "[object Object]".to_string(),
            value::TAG_UNDEFINED => "undefined".to_string(),
            value::TAG_NULL => "null".to_string(),
            value::TAG_AVAILABLE_5 => panic!("invalid value tag"),
            _ => val.into_float().to_string(),
        }
    }

    pub unsafe fn as_float(val: JSValue) -> f64 {
        match val.tag() {
            value::TAG_INT => val.into_int() as f64,
            value::TAG_STRING => {
                let val = val.into_string();
                let s = val.value().trim();
                if s.len() == 0 {
                    0.0
                } else {
                    val.value().parse::<f64>().unwrap_or(f64::NAN)
                }
            }
            value::TAG_UNDEFINED => f64::NAN,
            value::TAG_NULL => 0.0,
            value::TAG_OBJECT => f64::NAN,
            value::TAG_BOOL => {
                if val.into_bool() {
                    1.0
                } else {
                    0.0
                }
            }
            value::TAG_AVAILABLE_5 => panic!("invalid value tag"),
            _ => val.into_float(),
        }
    }

    unsafe fn strict_eq(b: JSValue, c: JSValue) -> bool {
        if b.is_float() && c.is_float() {
            return b.into_float() == c.into_float();
        }
        let b_tag = b.tag();
        if b_tag != c.tag() {
            return false;
        }
        match b_tag {
            value::TAG_INT | value::TAG_BOOL => b.bits == c.bits,
            value::TAG_UNDEFINED | value::TAG_NULL => true,
            value::TAG_STRING => b.into_string().value() == c.into_string().value(),
            value::TAG_OBJECT => b.into_raw_ptr() == c.into_raw_ptr(),
            _ => unreachable!(),
        }
    }

    unsafe fn eq(b: JSValue, c: JSValue) -> bool {
        if b.is_float() && c.is_float() {
            return b.into_float() == c.into_float();
        }
        let b_tag = b.tag();
        let c_tag = c.tag();
        if b_tag == c_tag {
            return match b_tag {
                value::TAG_INT | value::TAG_BOOL => b.bits == c.bits,
                value::TAG_UNDEFINED | value::TAG_NULL => true,
                value::TAG_STRING => b.into_string().value() == c.into_string().value(),
                value::TAG_OBJECT => b.into_raw_ptr() == c.into_raw_ptr(),
                _ => unreachable!(),
            };
        }

        if b_tag == value::TAG_UNDEFINED && c_tag == value::TAG_NULL {
            return true;
        }
        if b_tag == value::TAG_NULL && c_tag == value::TAG_UNDEFINED {
            return true;
        }
        if b.is_float() {
            return b.into_float() == Self::as_float(c);
        }
        match b_tag {
            value::TAG_UNDEFINED => c_tag == value::TAG_NULL,
            value::TAG_NULL => c_tag == value::TAG_UNDEFINED,
            value::TAG_INT => b.into_int() as f64 == Self::as_float(c),
            value::TAG_BOOL => {
                let c_num = Self::as_float(c);
                if b.into_bool() {
                    c_num == 1.0
                } else {
                    c_num == 0.0
                }
            }
            value::TAG_OBJECT => todo!(),
            value::TAG_STRING => Self::as_float(b) == Self::as_float(c),
            _ => panic!(),
        }
    }

    unsafe fn bin_addition(a: JSValue, b: JSValue) -> JSValue {
        if a.is_string() || b.is_string() {
            let mut s = Self::as_string(a);
            s.push_str(&Self::as_string(b));
            return JSValue::from(StringRc::new(s));
        }
        Self::bin_arithmatic(a, b, op::ADD)
    }

    pub unsafe fn bin_arithmatic(a: JSValue, b: JSValue, op: u8) -> JSValue {
        let a = Runtime::as_float(a);
        let b = Runtime::as_float(b);

        let res = match op {
            op::ADD => a + b,
            op::SUB => a + (-b),
            op::MUL => a * b,
            op::DIV => a / b,
            op::MOD => a % b,
            op::POW => a.powf(b),
            _ => panic!("invalid op"),
        };
        return Self::float_to_val(res);
    }

    #[inline]
    pub unsafe fn both_int(a: JSValue, b: JSValue) -> bool {
        a.bits & value::TAG_MASK == value::TAG_INT && b.bits & value::TAG_MASK == value::TAG_INT
    }
}
