#![allow(unused)]

use crate::{
    bytecode::{op, Bytecode, Instruction, Module, RuntimeFunction},
    environment::Environment,
    function::Function,
    gc::{Ctx, Gc, GcArena, Trace},
    object::Object,
    stack::Stack,
    value::{self, JSValue},
};

use std::{cell::RefCell, mem, num, ptr, rc::Rc};

mod exec_loop;

pub struct ExecutionContext {
    global: Gc<Object>,
    function: RuntimeFunction,
    environment: Rc<Environment>,
    instr_offset: usize,
    stack: Stack,
}

pub struct RunningExecution<'a, R: 'a> {
    pub(crate) instr_ptr: *const u8,
    #[cfg(debug_assertions)]
    pub(crate) instr_start: *const u8,
    #[cfg(debug_assertions)]
    pub(crate) instr_end: *const u8,
    pub(crate) data: &'a [JSValue],
    pub(crate) strings: &'a [String],
    pub(crate) stack: &'a mut Stack,
    pub(crate) global: Gc<Object>,
    pub(crate) module: Gc<Module>,
    pub(crate) environment: &'a Environment,
    pub(crate) gc: &'a GcArena,
    pub(crate) root: &'a R,
}

unsafe impl Trace for ExecutionContext {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: Ctx) {
        self.function.trace(ctx);
        self.stack.trace(ctx);
    }
}

impl ExecutionContext {
    pub fn new(
        global: Gc<Object>,
        function: RuntimeFunction,
        parent: Option<Rc<Environment>>,
    ) -> Self {
        let env = Environment::new(
            function.module.functions[function.function as usize].slots,
            parent,
        );
        ExecutionContext {
            global,
            environment: Rc::new(env),
            function,
            instr_offset: 0,
            stack: Stack::new(),
        }
    }

    pub unsafe fn into_running<'a, R: Trace + 'a>(
        &'a mut self,
        root: &'a R,
        gc: &'a GcArena,
    ) -> RunningExecution<'a, R> {
        let module_function = &self.function.module.functions[self.function.function as usize];
        let instruction = &self.function.module.bc.instructions;
        let instr_ptr = instruction.as_ptr().add(module_function.offset as usize);
        let data = self.function.module.bc.data.as_ref();
        let strings = self.function.module.bc.strings.as_ref();
        RunningExecution {
            instr_ptr: instr_ptr as *const _,
            #[cfg(debug_assertions)]
            instr_start: instr_ptr as *const _,
            #[cfg(debug_assertions)]
            instr_end: instr_ptr.add(module_function.len as usize) as *const _,
            data,
            strings,
            global: self.global,
            environment: &*self.environment,
            stack: &mut self.stack,
            module: self.function.module,
            gc,
            root,
        }
    }
}

impl<'a, R: 'a> RunningExecution<'a, R> {
    #[inline]
    unsafe fn write(&mut self, index: u8, value: JSValue) {
        debug_assert!(index < crate::MAX_REGISTERS);
        self.stack.set(index, value)
    }
    #[inline]
    unsafe fn read(&self, index: u8) -> JSValue {
        self.stack.get(index)
    }

    unsafe fn read_u8(&mut self) -> u8 {
        #[cfg(debug_assertions)]
        {
            debug_assert!(self.instr_ptr >= self.instr_start);
            debug_assert!(self.instr_ptr < self.instr_end);
        }
        let res = self.instr_ptr.read();
        self.instr_ptr = self.instr_ptr.add(mem::size_of::<u8>());
        res
    }

    unsafe fn read_u16(&mut self) -> u16 {
        #[cfg(debug_assertions)]
        {
            debug_assert!(self.instr_ptr >= self.instr_start);
            debug_assert!(self.instr_ptr < self.instr_end);
            debug_assert_eq!(self.instr_ptr.align_offset(mem::align_of::<u16>()), 0);
        }
        let res = (self.instr_ptr as *mut u16).read();
        self.instr_ptr = self.instr_ptr.add(mem::size_of::<u16>());
        res
    }

    unsafe fn read_u32(&mut self) -> u32 {
        #[cfg(debug_assertions)]
        {
            debug_assert!(self.instr_ptr >= self.instr_start);
            debug_assert!(self.instr_ptr < self.instr_end);
            debug_assert_eq!(self.instr_ptr.align_offset(mem::align_of::<u32>()), 0);
        }
        let res = (self.instr_ptr as *mut u32).read();
        self.instr_ptr = self.instr_ptr.add(mem::size_of::<u32>());
        res
    }

    unsafe fn jump(&mut self, jump: i32) {
        let target = jump as isize * mem::size_of::<Instruction>() as isize;
        self.instr_ptr = self.instr_ptr.offset(target);
        #[cfg(debug_assertions)]
        {
            debug_assert!(self.instr_ptr >= self.instr_start);
            debug_assert!(self.instr_ptr < self.instr_end);
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

    pub unsafe fn convert_string(&self, val: JSValue) -> String {
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

impl<'a, R: Trace> RunningExecution<'a, R> {
    unsafe fn gc_collect(&self, value: Option<JSValue>) {
        struct ExecRoot<'a, R> {
            root: &'a R,
            global: Gc<Object>,
            environment: &'a Environment,
            value: Option<JSValue>,
        };

        unsafe impl<'a, R: Trace> Trace for ExecRoot<'a, R> {
            fn trace(&self, ctx: Ctx) {
                self.root.trace(ctx);
                ctx.mark(self.global);
                self.environment.trace(ctx);
                self.value.as_ref().map(|x| x.trace(ctx));
            }
        }

        let r = ExecRoot {
            root: self.root,
            global: self.global,
            environment: self.environment,
            value,
        };

        self.gc.collect_debt(&r)
    }
}
