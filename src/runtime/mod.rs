//! The runtime executing the code.
pub mod value;
pub use value::{JSValue, TAG_INT};
pub mod bytecode;
pub use bytecode::op;
use std::{alloc, mem, ptr};

const INITIAL_STACK_SIZE: usize = 1024;

pub struct Runtime<'a> {
    stack: *mut JSValue,
    size: usize,
    code: &'a [u8],
    pc: usize,
    frame: *mut JSValue,
}

impl<'a> Runtime<'a> {
    pub unsafe fn new(code: &'a [u8]) -> Self {
        debug_assert_eq!(mem::size_of::<u64>(), mem::size_of::<JSValue>());
        let val_size = mem::size_of::<JSValue>();
        let layout = alloc::Layout::from_size_align(INITIAL_STACK_SIZE, val_size).unwrap();
        let stack = alloc::alloc(layout) as *mut _;
        Runtime {
            stack,
            size: INITIAL_STACK_SIZE,
            frame: stack,
            pc: 0,
            code,
        }
    }

    pub unsafe fn run_unsafe(&mut self) -> Option<JSValue> {
        loop {
            if self.frame.add(6) > self.stack.add(self.size) {
                let size = self.size << 1;
                let layout =
                    alloc::Layout::from_size_align(self.size, mem::size_of::<JSValue>()).unwrap();
                let frame_offset = self.frame as usize - self.stack as usize;
                self.stack = alloc::realloc(self.stack as *mut _, layout, size) as *mut _;
                self.frame = self.stack.add(frame_offset / mem::size_of::<JSValue>());
            }
            let instr = self.code[self.pc];
            match instr {
                op::LD_INT => {
                    let val = self.get_i32();
                    self.pc += mem::size_of::<i32>();
                    self.frame.write(JSValue::int(val));
                    self.frame = self.frame.add(1);
                }
                op::LD_VAL => {
                    let val = self.get_val();
                    self.pc += mem::size_of::<JSValue>();
                    self.frame.write(val);
                    self.frame = self.frame.add(1);
                }
                op::ADD => {
                    self.frame = self.frame.sub(1);
                    let b = self.frame.read();
                    let a = self.frame.sub(1).read();
                    let res = if Runtime::both_int(a, b) {
                        let res = a.0.int as i64 + b.0.int as i64;
                        dbg!(res);
                        if res != (a.0.int + b.0.int) as i64 {
                            JSValue::float(res as f64)
                        } else {
                            JSValue::int(res as i32)
                        }
                    } else {
                        Self::bin_arithmatic(a, b, op::ADD)
                    };
                    self.frame.sub(1).write(res);
                }
                op::SUB => {
                    self.frame = self.frame.sub(1);
                    let b = self.frame.read();
                    let a = self.frame.sub(1).read();
                    let res = if Runtime::both_int(a, b) {
                        let res = a.0.int as i64 - b.0.int as i64;
                        if res != (a.0.int - b.0.int) as i64 {
                            JSValue::float(res as f64)
                        } else {
                            JSValue::int(res as i32)
                        }
                    } else {
                        Self::bin_arithmatic(a, b, op::SUB)
                    };
                    self.frame.sub(1).write(res);
                }
                op::MUL | op::DIV => {
                    self.frame = self.frame.sub(1);
                    let b = self.frame.read();
                    let a = self.frame.sub(1).read();
                    let res = Self::bin_arithmatic(a, b, instr);
                    self.frame.sub(1).write(res)
                }
                op::MOD => {
                    self.frame = self.frame.sub(1);
                    let b = self.frame.read();
                    let a = self.frame.sub(1).read();
                    let res = if Runtime::both_int(a, b) {
                        let res = a.0.int % b.0.int;
                        JSValue::int(res as i32)
                    } else {
                        Self::bin_arithmatic(a, b, op::MOD)
                    };
                    self.frame.sub(1).write(res)
                }
                op::POW => {
                    self.frame = self.frame.sub(1);
                    let b = self.frame.read();
                    let a = self.frame.sub(1).read();
                    let res = Self::bin_arithmatic(a, b, op::POW);
                    self.frame.sub(1).write(res)
                }
                op::RET => {
                    if self.frame != self.stack {
                        self.frame = self.frame.sub(1);
                        return Some(self.frame.read());
                    } else {
                        return None;
                    }
                }
                _ => panic!("invalid instruction"),
            }
            self.pc += 1;
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
        todo!()
    }

    #[inline]
    pub unsafe fn get_i32(&mut self) -> i32 {
        let ptr = self.code.as_ptr().add(self.pc + 1) as *mut i32;
        ptr.read_unaligned()
    }

    #[inline]
    pub unsafe fn get_val(&mut self) -> JSValue {
        let ptr = self.code.as_ptr().add(self.pc + 1) as *mut JSValue;
        ptr.read_unaligned()
    }

    #[inline]
    pub unsafe fn both_int(a: JSValue, b: JSValue) -> bool {
        a.0.bits & b.0.bits & TAG_INT == TAG_INT
    }
}
