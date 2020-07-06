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
use std::{alloc, f64, mem, ptr};
use string::StringRc;

const INITIAL_STACK_SIZE: usize = mem::size_of::<JSValue>() * 8;
// Full range for now
// prop too big for final value
const NUM_REGISTERS: usize = 255;

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
        (*self.frame) = js_value;
        self.frame = self.frame.add(1);
    }

    #[inline(always)]
    pub unsafe fn get(&mut self, idx: u8) -> &mut JSValue {
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
        while self.stack.sub(1) != self.frame {
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
                        JSValue::object(self.global)
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
                        JSValue::object(self.global)
                    } else {
                        *self.get(op_b)
                    };
                    let key = *self.get(bc::op_c(instr));
                    let val = obj.into_object().value().get(&Self::as_string(key));
                    *self.get(bc::op_a(instr)) = val;
                }
                op::CLL => {
                    let val = bc::op_d(instr);
                    *self.get(bc::op_a(instr)) = JSValue::int(val as i32);
                }
                op::CLH => {
                    let val = bc::op_d(instr) as u64;
                    self.get(bc::op_a(instr)).0.bits |= val << 16;
                }
                op::CLF => {
                    let val = self.code[self.pc + 1] as u64 | (self.code[self.pc + 2] as u64) << 32;
                    self.get(bc::op_a(instr)).0.bits = val;
                    self.pc += 2;
                }
                op::CLP => {
                    let prim = match bc::op_d(instr) {
                        bc::PRIM_VAL_NULL => JSValue::null(),
                        bc::PRIM_VAL_UNDEFINED => JSValue::undefined(),
                        bc::PRIM_VAL_TRUE => JSValue::bool(true),
                        bc::PRIM_VAL_FALSE => JSValue::bool(false),
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
                        DataValue::String(x) => JSValue::string(Rc::new(x)),
                        DataValue::Object(x) => JSValue::object(Rc::new(x)),
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
                        let val1 = val1.0.int as i64;
                        let val2 = val2.0.int as i64;
                        let res = val1 + val2;
                        let res = if (res as i32) as i64 != res {
                            JSValue::float(res as f64)
                        } else {
                            JSValue::int(res as i32)
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
                        let val1 = val1.0.int as i64;
                        let val2 = val2.0.int as i64;
                        let res = val1 - val2;
                        let res = if (res as i32) as i64 != res {
                            JSValue::float(res as f64)
                        } else {
                            JSValue::int(res as i32)
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
                op::RET => return Some(self.regs[bc::op_a(instr) as usize]),
                _ => panic!("invalid instruction"),
            }
            self.pc += 1;
        }
    }

    pub unsafe fn bin_addition(a: JSValue, b: JSValue) -> JSValue {
        if a.is_string() || b.is_string() {
            let mut s = Self::as_string(a);
            s.push_str(&Self::as_string(b));
            return JSValue::string(StringRc::new(s));
        }
        Self::bin_arithmatic(a, b, op::ADD)
    }

    unsafe fn as_string(val: JSValue) -> String {
        match val.tag() {
            value::TAG_STRING => val.into_string().value().clone(),
            value::TAG_INT => val.into_int().to_string(),
            value::TAG_BOOL => val.into_bool().to_string(),
            value::TAG_OBJECT => "[object Object]".to_string(),
            value::TAG_UNDEFINED => "undefined".to_string(),
            value::TAG_NULL => "null".to_string(),
            value::TAG_AVAILABLE_5 => panic!("invalid tag"),
            _ => unreachable!(),
        }
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
        return if (res as i32) as f64 == res {
            JSValue::int(res as i32)
        } else {
            JSValue::float(res)
        };
    }

    pub unsafe fn as_float(val: JSValue) -> f64 {
        if val.is_float() {
            return val.0.float;
        }
        if val.is_int() {
            return val.into_int() as f64;
        }
        if val.is_string() {
            let s = val.into_string().value();
            if s.len() == 0 {
                return 0.0;
            };
            return val.into_string().value().parse::<f64>().unwrap_or(f64::NAN);
        }
        todo!()
    }

    #[inline]
    pub unsafe fn both_int(a: JSValue, b: JSValue) -> bool {
        a.0.bits & value::TAG_MASK == value::TAG_INT && b.0.bits & value::TAG_MASK == value::TAG_INT
    }
}
