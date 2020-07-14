//! The runtime executing the code.
pub mod value;
pub use value::JSValue;
pub mod bc;
use bc::Bytecode;
pub mod object;
use object::Object;
pub mod rc;
use rc::Rc;
pub mod string;
use bc::{op, DataValue, Instruction};
use std::{alloc, f64, mem, num, ptr};
use string::StringRc;

const INITIAL_STACK_SIZE: usize = mem::size_of::<JSValue>() * 8;
// Full range for now
// prop too big for final value
const NUM_REGISTERS: usize = 64;

pub struct Runtime<'a> {
    stack: *mut JSValue,
    size: usize,
    code: &'a [Instruction],
    data: &'a [DataValue],
    pc: usize,
    frame: *mut JSValue,
    regs: [JSValue; NUM_REGISTERS],
    global: rc::Rc<Object>,
}

impl<'a> Drop for Runtime<'a> {
    fn drop(&mut self) {
        let val_size = mem::size_of::<JSValue>();
        let layout = alloc::Layout::from_size_align(self.size, val_size).unwrap();
        unsafe { alloc::dealloc(self.stack as *mut _, layout) }
    }
}

impl<'a> Runtime<'a> {
    pub unsafe fn new(code: &'a Bytecode) -> Self {
        debug_assert_eq!(mem::size_of::<u64>(), mem::size_of::<JSValue>());
        let data = &code.data;
        let code = &code.instructions;
        let val_size = mem::size_of::<JSValue>();
        let layout = alloc::Layout::from_size_align(INITIAL_STACK_SIZE, val_size).unwrap();
        let stack = alloc::alloc(layout) as *mut _;
        let regs = mem::zeroed();
        Runtime {
            stack,
            size: INITIAL_STACK_SIZE,
            frame: stack,
            pc: 0,
            code,
            data,
            regs,
            global: rc::Rc::new(Object::new()),
        }
    }

    unsafe fn push(&mut self, js_value: JSValue) {
        if self.stack.add(self.size) == self.frame {
            let size = self.size << 1;
            let layout =
                alloc::Layout::from_size_align(self.size, mem::size_of::<JSValue>()).unwrap();
            let frame_offset = self.frame as usize - self.stack as usize;
            self.stack = alloc::realloc(self.stack as *mut _, layout, size) as *mut _;
            self.frame = self.stack.add(frame_offset / mem::size_of::<JSValue>());
        }
        self.frame.write(js_value);
        self.frame = self.frame.add(1);
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
        self.frame = self.frame.sub(1);
        while self.frame >= self.stack {
            (*self.frame).drop();
            self.frame = self.frame.sub(1);
        }
    }

    unsafe fn run_loop(&mut self) -> Option<JSValue> {
        loop {
            let instr = self.code[self.pc];
            let op = bc::op_op(instr);
            match op {
                op::OSET => {
                    let op_a = bc::op_a(instr);
                    let obj = if op_a == 0xff {
                        JSValue::from(self.global)
                    } else {
                        *self.get(op_a)
                    };
                    let key = *self.get(bc::op_b(instr));
                    let val = *self.get(bc::op_c(instr));
                    let key = Self::as_string(key);
                    obj.into_object()
                        .value()
                        .set(key, val.clone())
                        .map(|x| x.drop());
                }
                op::OGET => {
                    let op_b = bc::op_b(instr);
                    let obj = if op_b == 0xff {
                        JSValue::from(self.global)
                    } else {
                        *self.get(op_b)
                    };
                    let key = *self.get(bc::op_c(instr));
                    let val = obj.into_object().value().get(&Self::as_string(key));
                    *self.get(bc::op_a(instr)) = val;
                }
                op::CLL => {
                    let val = bc::op_d(instr);
                    *self.get(bc::op_a(instr)) = JSValue::from(val as i32);
                }
                op::CLH => {
                    let val = bc::op_d(instr) as u64;
                    self.get(bc::op_a(instr)).bits |= val << 16;
                }
                op::CLF => {
                    let val = self.code[self.pc + 1] as u64 | (self.code[self.pc + 2] as u64) << 32;
                    *self.get(bc::op_a(instr)) = JSValue::from(f64::from_bits(val));
                    self.pc += 2;
                }
                op::CLP => {
                    let prim = match bc::op_d(instr) {
                        bc::PRIM_VAL_NULL => JSValue::null(),
                        bc::PRIM_VAL_UNDEFINED => JSValue::undefined(),
                        bc::PRIM_VAL_TRUE => JSValue::from(true),
                        bc::PRIM_VAL_FALSE => JSValue::from(false),
                        _ => panic!("invalid primitive value!"),
                    };
                    *self.get(bc::op_a(instr)) = prim;
                }
                op::CLD => {
                    let val = bc::op_d(instr);
                    let idx = if val & 0x8000 == 0 {
                        val as usize
                    } else {
                        let js_value = self.get((val & 0xff) as u8);
                        if js_value.is_int() {
                            js_value.into_int() as usize
                        } else {
                            js_value.into_float() as usize
                        }
                    };
                    let dst = bc::op_a(instr);
                    let data = match self.data[idx].clone() {
                        DataValue::String(x) => JSValue::from(Rc::new(x)),
                        DataValue::Object(x) => JSValue::from(Rc::new(x)),
                    };
                    (*self.get(dst)) = data;
                    self.push(data);
                }
                op::MOV => {
                    let dst = bc::op_a(instr);
                    let src = bc::op_d(instr);
                    *self.get(dst) = *self.get(src as u8);
                }
                op::ADD => {
                    let val1 = *self.get(bc::op_b(instr));
                    let val2 = *self.get(bc::op_c(instr));
                    if Self::both_int(val1, val2) {
                        let val1 = val1.into_int() as i64;
                        let val2 = val2.into_int() as i64;
                        let res = val1 + val2;
                        let res = if (res as i32) as i64 != res {
                            JSValue::from(res as f64)
                        } else {
                            JSValue::from(res as i32)
                        };
                        *self.get(bc::op_a(instr)) = res;
                    } else {
                        let res = Self::bin_addition(val1, val2);
                        *self.get(bc::op_a(instr)) = res;
                    }
                }
                op::SUB => {
                    let val1 = *self.get(bc::op_b(instr));
                    let val2 = *self.get(bc::op_c(instr));
                    if Self::both_int(val1, val2) {
                        let val1 = val1.into_int() as i64;
                        let val2 = val2.into_int() as i64;
                        let res = val1 - val2;
                        let res = if (res as i32) as i64 != res {
                            JSValue::from(res as f64)
                        } else {
                            JSValue::from(res as i32)
                        };
                        *self.get(bc::op_a(instr)) = res;
                    } else {
                        let res = Self::bin_arithmatic(val1, val2, op::SUB);
                        *self.get(bc::op_a(instr)) = res;
                    }
                }
                op::MUL => {
                    let val1 = *self.get(bc::op_b(instr));
                    let val2 = *self.get(bc::op_c(instr));
                    let res = Self::bin_arithmatic(val1, val2, op::MUL);
                    *self.get(bc::op_a(instr)) = res;
                }
                op::DIV => {
                    let val1 = *self.get(bc::op_b(instr));
                    let val2 = *self.get(bc::op_c(instr));
                    let res = Self::bin_arithmatic(val1, val2, op::DIV);
                    *self.get(bc::op_a(instr)) = res;
                }
                op::POW => {
                    let val1 = *self.get(bc::op_b(instr));
                    let val2 = *self.get(bc::op_c(instr));
                    let res = Self::bin_arithmatic(val1, val2, op::POW);
                    *self.get(bc::op_a(instr)) = res;
                }
                op::MOD => {
                    let val1 = *self.get(bc::op_b(instr));
                    let val2 = *self.get(bc::op_c(instr));
                    let res = Self::bin_arithmatic(val1, val2, op::MOD);
                    *self.get(bc::op_a(instr)) = res;
                }
                op::BAND => {
                    let b = Self::as_i32(*self.get(bc::op_b(instr)));
                    let c = Self::as_i32(*self.get(bc::op_c(instr)));
                    *self.get(bc::op_a(instr)) = JSValue::from(b & c);
                }
                op::BOR => {
                    let b = Self::as_i32(*self.get(bc::op_b(instr)));
                    let c = Self::as_i32(*self.get(bc::op_c(instr)));
                    *self.get(bc::op_a(instr)) = JSValue::from(b | c);
                }
                op::BXOR => {
                    let b = Self::as_i32(*self.get(bc::op_b(instr)));
                    let c = Self::as_i32(*self.get(bc::op_c(instr)));
                    *self.get(bc::op_a(instr)) = JSValue::from(b ^ c);
                }
                op::POS => {
                    let val = *self.get(bc::op_d(instr) as u8);
                    let val = Self::float_to_val(Self::as_float(val));
                    *self.get(bc::op_a(instr)) = val;
                }
                op::NEG => {
                    let val = *self.get(bc::op_d(instr) as u8);
                    let val = Self::float_to_val(-Self::as_float(val));
                    *self.get(bc::op_a(instr)) = val;
                }
                op::SHL => {
                    let b = Self::as_i32(*self.get(bc::op_b(instr)));
                    let c = Self::as_u32(*self.get(bc::op_c(instr)));
                    let c = (c & 0x1f) as i32;
                    *self.get(bc::op_a(instr)) = JSValue::from(b << c);
                }
                op::SHR => {
                    let b = Self::as_i32(*self.get(bc::op_b(instr)));
                    let c = Self::as_u32(*self.get(bc::op_c(instr)));
                    let c = (c & 0x1f) as i32;
                    *self.get(bc::op_a(instr)) = JSValue::from((b >> c) as i32);
                }
                op::USR => {
                    let b = Self::as_i32(*self.get(bc::op_b(instr))) as u32;
                    let mut c = Self::as_u32(*self.get(bc::op_c(instr)));
                    c &= 0x1f;
                    *self.get(bc::op_a(instr)) = JSValue::from((b >> c) as i32);
                }
                op::SEQ => {
                    let b = *self.get(bc::op_b(instr));
                    let c = *self.get(bc::op_c(instr));
                    *self.get(bc::op_a(instr)) = JSValue::from(Self::strict_eq(b, c))
                }
                op::EQ => {
                    let b = *self.get(bc::op_b(instr));
                    let c = *self.get(bc::op_c(instr));
                    *self.get(bc::op_a(instr)) = JSValue::from(Self::eq(b, c))
                }
                op::SNEQ => {
                    let b = *self.get(bc::op_b(instr));
                    let c = *self.get(bc::op_c(instr));
                    *self.get(bc::op_a(instr)) = JSValue::from(!Self::strict_eq(b, c))
                }
                op::NEQ => {
                    let b = *self.get(bc::op_b(instr));
                    let c = *self.get(bc::op_c(instr));
                    *self.get(bc::op_a(instr)) = JSValue::from(!Self::eq(b, c))
                }
                op::RET => {
                    let val = *self.get(bc::op_a(instr));
                    val.incr();
                    return Some(val);
                }
                _ => panic!("invalid instruction"),
            }
            self.pc += 1;
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
                let s = val.into_string().value().trim();
                if s.len() == 0 {
                    0.0
                } else {
                    val.into_string().value().parse::<f64>().unwrap_or(f64::NAN)
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
