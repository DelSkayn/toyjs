use std::mem;

use crate::{
    gc::Trace,
    instructions::{Instruction, Upvalue},
    object::{FunctionKind, Object, ObjectFlags, VmFunction, RECURSIVE_FUNC_PANIC},
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

#[derive(Debug)]
pub struct ExecutionContext {
    pub function: Gc<Object>,
    pub this: Value,
    pub new_target: Value,
}

impl Copy for ExecutionContext {}
impl Clone for ExecutionContext {
    fn clone(&self) -> Self {
        *self
    }
}

unsafe impl Trace for ExecutionContext {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        ctx.mark(self.function);
        self.this.trace(ctx);
        self.new_target.trace(ctx);
    }
}

impl Realm {
    pub unsafe fn execute(
        &mut self,
        mut instr: InstructionReader,
        mut ctx: ExecutionContext,
    ) -> Result<Value, Value> {
        loop {
            match instr.next() {
                Instruction::LoadConst { dst, cons } => {
                    self.stack.write(dst, instr.constant(cons as u32));
                }
                Instruction::LoadGlobal { dst } => {
                    self.stack.write(dst, Value::from(self.global));
                }
                Instruction::LoadFunction { dst, func } => {
                    self.gc.collect_debt(&(&*self, &ctx));
                    let func = self.construct_function(func, &instr, ctx.function);
                    let res = Value::from(self.gc.allocate(func));
                    self.stack.write(dst, res);
                }
                Instruction::LoadConstructor { dst, func } => {
                    self.gc.collect_debt(&(&*self, &ctx));
                    let func = self.construct_constructor(func, &instr, ctx.function);
                    self.stack.write(dst, func.into());
                }
                Instruction::LoadThis { dst } => self.stack.write(dst, ctx.this),
                Instruction::Move { dst, src } => self.stack.write(dst, self.stack.read(src)),
                Instruction::CreateObject { dst } => {
                    self.gc.collect_debt(&(&*self, instr, ctx.function));
                    let object = Object::new(None);
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
                        obj.index_set(key, val, self);
                    } else {
                        // TODO proper error value
                        self.unwind_error(&mut instr, &mut ctx, Value::undefined())?
                    }
                }
                Instruction::Index { dst, obj, key } => {
                    let obj = self.stack.read(obj);
                    let key = self.stack.read(key);
                    if obj.is_object() {
                        let res = obj.unsafe_cast_object().index(key, self);
                        self.stack.write(dst, res)
                    } else {
                        // TODO proper error value
                        self.unwind_error(&mut instr, &mut ctx, Value::undefined())?
                    }
                }

                Instruction::GlobalIndex { dst, key } => {
                    let key = self.stack.read(key);
                    let src = self.global().index(key, self);
                    self.stack.write(dst, src);
                }

                Instruction::GlobalAssign { key, src } => {
                    self.gc.write_barrier(self.global());
                    self.global()
                        .index_set(self.stack.read(key), self.stack.read(src), self);
                }

                Instruction::Upvalue { dst, slot } => {
                    let value = ctx.function.as_vm_function().upvalues[slot as usize].read();
                    self.stack.write(dst, value);
                }

                Instruction::UpvalueAssign { src, slot } => {
                    let value = self.stack.read(src);
                    let upvalue = ctx.function.as_vm_function().upvalues[slot as usize];
                    self.gc.write_barrier(upvalue);
                    upvalue.write(value);
                }

                Instruction::TypeOf { dst, src } => {
                    let src = self.stack.read(src);
                    let res = self.type_of(src);
                    self.stack.write(dst, res.into());
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
                    let left = self.to_int32(left);
                    let right = self.to_int32(right) as u32 % 32;
                    self.stack.write(dst, Value::from(left << right));
                }
                Instruction::ShiftRight { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let left = self.to_int32(left);
                    let right = self.to_int32(right) as u32 % 32;
                    self.stack.write(dst, Value::from(left >> right));
                }
                Instruction::ShiftUnsigned { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let left = self.to_int32(left) as u32;
                    let right = self.to_int32(right) as u32 % 32;
                    self.stack.write(dst, Value::from((left >> right) as i32));
                }
                Instruction::Equal { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.equal(left, right);
                    self.stack.write(dst, Value::from(res));
                }
                Instruction::NotEqual { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = !self.equal(left, right);
                    self.stack.write(dst, Value::from(res));
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

                Instruction::Greater { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.less_then(left, right, true);
                    if res.is_undefined() {
                        self.stack.write(dst, false.into());
                    } else {
                        self.stack.write(dst, res);
                    }
                }
                Instruction::GreaterEq { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.less_then(left, right, false);
                    let res = res.is_false() && !res.is_undefined();
                    self.stack.write(dst, res.into())
                }

                Instruction::Less { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.less_then(left, right, false);
                    if res.is_undefined() {
                        self.stack.write(dst, false.into());
                    } else {
                        self.stack.write(dst, res);
                    }
                }
                Instruction::LessEq { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = self.less_then(left, right, true);
                    let res = res.is_false() && !res.is_undefined();
                    self.stack.write(dst, res.into())
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
                    let number = self.to_number(src);
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

                Instruction::Try { dst, tgt } => self
                    .stack
                    .push_try(dst, instr.absolute_offset(tgt) as usize),

                Instruction::Untry { _ignore: () } => {
                    self.stack.pop_try();
                }

                Instruction::Push { src } => {
                    let src = self.stack.read(src);
                    self.stack.push(src);
                }

                Instruction::Call { dst, func } => {
                    let func = self.stack.read(func);
                    if let Some(value) = self._call(func, &mut instr, &mut ctx, dst)? {
                        self.stack.write(dst, value);
                    }
                }
                Instruction::CallConstruct { dst, func, obj } => {
                    let func = self.stack.read(func);
                    let obj = self.stack.read(obj);
                    if let Some(value) = self.call_construct(func, obj, &mut instr, &mut ctx)? {
                        self.stack.write(dst, value);
                    }
                }

                Instruction::Throw { src } => {
                    let error = self.stack.read(src);
                    self.unwind_error(&mut instr, &mut ctx, error)?;
                }

                Instruction::ReturnUndefined { .. } => match self.stack.pop(&self.gc) {
                    Some(frame) => {
                        ctx = frame.ctx;
                        instr = frame.instr;
                        self.stack.write(frame.dst, Value::undefined());
                    }
                    None => return Ok(Value::undefined()),
                },
                Instruction::Return { ret } => {
                    let return_value = self.stack.read(ret);
                    match self.stack.pop(&self.gc) {
                        Some(frame) => {
                            ctx = frame.ctx;
                            instr = frame.instr;
                            self.stack.write(frame.dst, return_value);
                        }
                        None => return Ok(return_value),
                    }
                }
                x => panic!("invalid Instruction {}", x),
            }
        }
    }

    pub unsafe fn unwind<R, O, F>(
        &mut self,
        instr: &mut InstructionReader,
        ctx: &mut ExecutionContext,
        f: F,
    ) -> Result<Option<O>, Value>
    where
        F: FnOnce(&mut Self, &mut InstructionReader, &mut ExecutionContext) -> Result<R, Value>,
        R: Into<Option<O>>,
    {
        match f(self, instr, ctx) {
            Ok(x) => Ok(x.into()),
            Err(e) => {
                self.unwind_error(instr, ctx, e)?;
                Ok(None)
            }
        }
    }

    pub unsafe fn unwind_error(
        &mut self,
        instr: &mut InstructionReader,
        ctx: &mut ExecutionContext,
        error: Value,
    ) -> Result<(), Value> {
        loop {
            match dbg!(self.stack.unwind(&self.gc)) {
                Some(Ok(catch)) => {
                    self.stack.write(catch.dst, error);
                    instr.absolute_jump(catch.ip_offset);
                    return Ok(());
                }
                Some(Err(frame)) => {
                    *ctx = frame.ctx;
                    *instr = frame.instr;
                }
                None => {
                    return Err(error);
                }
            }
        }
    }

    pub unsafe fn is_falsish(&mut self, value: Value) -> bool {
        if value.is_int() {
            value.cast_int() == 0
        } else if value.is_string() {
            value.unsafe_cast_string().is_empty()
        } else {
            value.is_null() || value.is_undefined() || value.is_false()
        }
    }

    pub unsafe fn is_nullish(&mut self, value: Value) -> bool {
        value.is_null() || value.is_undefined()
    }

    /// Implements type conversion [`Tostring`](https://tc39.es/ecma262/#sec-tostring)
    pub unsafe fn to_string(&mut self, value: Value) -> Gc<String> {
        if value.is_int() {
            self.gc.allocate(value.cast_int().to_string())
        } else if value.is_float() {
            self.gc.allocate(value.cast_float().to_string())
        } else if value.is_null() {
            self.gc.allocate("null".to_string())
        } else if value.is_undefined() {
            self.gc.allocate("undefined".to_string())
        } else if value.is_true() {
            self.gc.allocate("true".to_string())
        } else if value.is_false() {
            self.gc.allocate("false".to_string())
        } else if value.is_string() {
            value.unsafe_cast_string()
        } else if value.is_object() {
            self.gc.allocate("[object Object]".to_string())
        } else {
            todo!()
        }
    }

    pub unsafe fn to_object(&mut self, value: Value) -> Gc<Object> {
        if value.is_null() || value.is_undefined() {
            return self.create_object();
        }
        todo!()
    }

    /// Implements type conversion [`ToNumber`](https://tc39.es/ecma262/#sec-tonumber)
    pub unsafe fn to_number(&mut self, value: Value) -> Value {
        if value.is_int() || value.is_float() {
            value
        } else if value.is_undefined() {
            Value::nan()
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
        } else if value.is_object() {
            let prim = self.to_primitive(value);
            self.to_number(prim)
        } else {
            todo!()
        }
    }

    /// Implements type conversion [`ToInt32`](https://tc39.es/ecma262/#sec-toint32)
    pub unsafe fn to_int32(&mut self, value: Value) -> i32 {
        let number = self.to_number(value);
        if number.is_int() {
            number.cast_int()
        } else if number.is_float() {
            let f = number.cast_float();
            if f.is_normal() {
                let res = f.abs().floor() as i32;
                if f.is_sign_positive() {
                    res
                } else {
                    -res
                }
            } else {
                0
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
        if left.is_float() && right.is_float() {
            return left.cast_float() == right.cast_float();
        }
        todo!()
    }

    pub unsafe fn equal(&mut self, left: Value, right: Value) -> bool {
        if left.same_type(right) {
            return self.strict_equal(left, right);
        }

        if left.is_undefined() && right.is_null() {
            return true;
        }
        if right.is_undefined() && left.is_null() {
            return true;
        }
        if left.is_number() && right.is_string() {
            let right = self.to_number(right);
            return self.equal(left, right);
        }
        if right.is_number() && left.is_string() {
            let left = self.to_number(left);
            return self.equal(left, right);
        }
        if left.is_bool() {
            let left = self.to_number(left);
            return self.equal(left, right);
        }
        if right.is_bool() {
            let right = self.to_number(right);
            return self.equal(left, right);
        }
        if !left.is_object() && right.is_object() {
            let right = self.to_primitive(right);
            return self.equal(left, right);
        }
        if !right.is_object() && left.is_object() {
            let left = self.to_primitive(left);
            return self.equal(left, right);
        }
        false
    }

    pub unsafe fn less_then(&mut self, left: Value, right: Value, swap: bool) -> Value {
        let (left, right) = if swap {
            let r = self.to_primitive(left);
            let l = self.to_primitive(right);
            (l, r)
        } else {
            (self.to_primitive(left), self.to_primitive(right))
        };
        if left.is_string() || right.is_string() {
            let left = self.to_string(left);
            let right = self.to_string(right);
            if left.as_str().starts_with(right.as_str()) {
                return false.into();
            }
            if right.as_str().starts_with(left.as_str()) {
                return true.into();
            }
            let mut left = left.as_str().chars();
            let mut right = right.as_str().chars();
            loop {
                let left = left.next().unwrap();
                let right = right.next().unwrap();
                if left != right {
                    return (left < right).into();
                }
            }
        }

        let left = self.to_number(left);
        let right = self.to_number(right);
        if left.is_int() && right.is_string() {
            return (left.cast_int() < right.cast_int()).into();
        }
        let left = if left.is_float() {
            left.cast_float()
        } else {
            left.cast_int() as f64
        };
        let right = if right.is_float() {
            right.cast_float()
        } else {
            right.cast_int() as f64
        };

        if left.is_nan() || right.is_nan() {
            return Value::undefined();
        }

        if left.to_bits() == f64::NEG_INFINITY.to_bits()
            || right.to_bits() == f64::INFINITY.to_bits()
        {
            return true.into();
        }

        if left.to_bits() == f64::INFINITY.to_bits()
            || right.to_bits() == f64::NEG_INFINITY.to_bits()
        {
            return false.into();
        }
        (left < right).into()
    }

    pub unsafe fn add(&mut self, left: Value, right: Value) -> Value {
        let left = self.to_primitive(left);
        let right = self.to_primitive(right);
        if left.is_string() || right.is_string() {
            let left = self.to_string(left);
            let right = self.to_string(right);
            return Value::from(self.gc.allocate(left.to_string() + &right.to_string()));
        }
        let left = self.to_number(left);
        let right = self.to_number(right);
        let left = if left.is_int() {
            left.cast_int() as f64
        } else if left.is_float() {
            left.cast_float()
        } else {
            todo!()
        };
        let right = if right.is_int() {
            right.cast_int() as f64
        } else if right.is_float() {
            right.cast_float()
        } else {
            todo!()
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
        let left = self.to_number(left);
        let right = self.to_number(right);
        let left = if left.is_int() {
            left.cast_int() as f64
        } else if left.is_float() {
            left.cast_float()
        } else {
            todo!()
        };
        let right = if right.is_int() {
            right.cast_int() as f64
        } else if right.is_float() {
            right.cast_float()
        } else {
            todo!()
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
    pub unsafe fn type_of(&mut self, v: Value) -> Gc<String> {
        if v.is_undefined() {
            self.create_string("undefined")
        } else if v.is_null() {
            self.create_string("object")
        } else if v.is_bool() {
            self.create_string("boolean")
        } else if v.is_number() {
            self.create_string("number")
        } else if v.is_string() {
            self.create_string("string")
        } else if v.is_object() {
            if v.unsafe_cast_object().is_function() {
                self.create_string("function")
            } else {
                self.create_string("object")
            }
        } else {
            unreachable!()
        }
    }

    #[inline]
    pub unsafe fn to_primitive(&mut self, v: Value) -> Value {
        if v.is_object() {
            todo!()
        }
        v
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
        function: Gc<Object>,
    ) -> Object {
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
        Object::from_vm(
            self,
            VmFunction {
                bc: reader.bc,
                function: function_id,
                upvalues,
            },
        )
    }

    pub(crate) unsafe fn construct_constructor(
        &mut self,
        function_id: u16,
        reader: &InstructionReader,
        function: Gc<Object>,
    ) -> Gc<Object> {
        let mut function = self.construct_function(function_id, reader, function);
        function.flags |= ObjectFlags::CONSTRUCTOR;
        let function = self.gc.allocate(function);
        let proto = self.create_object();
        proto.raw_index_set(
            self.builtin.key_construct.unwrap().into(),
            function.into(),
            self,
        );
        function.raw_index_set(self.builtin.key_proto.unwrap().into(), proto.into(), self);
        function
    }

    /// Execution only version of call
    unsafe fn _call(
        &mut self,
        function: Value,
        instr: &mut InstructionReader,
        ctx: &mut ExecutionContext,
        dst: u8,
    ) -> Result<Option<Value>, Value> {
        self.unwind(instr, ctx, |this, instr, ctx| {
            if !function.is_object() {
                // TODO proper error value
                return Err(Value::undefined());
            }
            let function = function.unsafe_cast_object();
            let kind = if let Some(x) = function.function.as_ref() {
                x
            } else {
                // TODO proper error value
                return Err(Value::undefined());
            };
            match kind {
                FunctionKind::Vm(ref func) => {
                    let bc = func.bc;
                    let bc_function = &bc.functions[func.function as usize];
                    this.stack
                        .enter_call(bc_function.registers, dst, *instr, *ctx);
                    ctx.function = function;
                    *instr = InstructionReader::from_bc(bc, func.function);
                    Ok(None)
                }
                FunctionKind::Static(x) => {
                    let function = mem::replace(&mut ctx.function, function);
                    this.stack.enter(0);
                    let res = x(this, ctx)?;
                    this.stack.pop(&this.gc);
                    ctx.function = function;
                    Ok(Some(res))
                }
                FunctionKind::Mutable(ref x) => {
                    let function = mem::replace(&mut ctx.function, function);
                    this.stack.enter(0);
                    let res = x.try_borrow_mut().expect(RECURSIVE_FUNC_PANIC)(this, ctx)?;
                    this.stack.pop(&this.gc);
                    ctx.function = function;
                    Ok(Some(res))
                }
                FunctionKind::Shared(ref x) => {
                    let function = mem::replace(&mut ctx.function, function);
                    this.stack.enter(0);
                    let res = (*x)(this, ctx)?;
                    this.stack.pop(&this.gc);
                    ctx.function = function;
                    Ok(Some(res))
                }
            }
        })
    }

    unsafe fn call_construct(
        &mut self,
        function: Value,
        target_obj: Value,
        instr: &mut InstructionReader,
        ctx: &mut ExecutionContext,
    ) -> Result<Option<Value>, Value> {
        self.unwind(instr, ctx, |this, _instr, ctx| {
            if !function.is_object() {
                //TODO proper error value
                return Err(Value::undefined());
            }

            let function = function.unsafe_cast_object();
            let kind = if let Some(x) = function.function.as_ref() {
                x
            } else {
                //TODO proper error value
                return Err(Value::undefined());
            };
            if !function.is_constructor() {
                //TODO proper error value
                return Err(Value::undefined());
            }

            match kind {
                FunctionKind::Vm(ref func) => {
                    let bc = func.bc;
                    let bc_function = &bc.functions[func.function as usize];
                    this.stack.enter(bc_function.registers);

                    let proto = function.index(this.builtin.key_proto.unwrap().into(), this);
                    let this_object = if proto.is_object() {
                        this.create_object_proto(Some(proto.unsafe_cast_object()))
                    } else {
                        this.create_object()
                    };

                    let instr = InstructionReader::from_bc(bc, func.function);
                    let res = this.execute(
                        instr,
                        ExecutionContext {
                            function,
                            this: this_object.into(),
                            new_target: target_obj,
                        },
                    )?;
                    if res.is_object() {
                        Ok(res)
                    } else {
                        Ok(this_object.into())
                    }
                }
                FunctionKind::Static(func) => {
                    let function = mem::replace(&mut ctx.function, function);
                    this.stack.enter(0);
                    let res = func(this, ctx)?;
                    this.stack.pop(&this.gc);
                    ctx.function = function;
                    Ok(res)
                }
                _ => {
                    //TODO proper error value
                    return Err(Value::undefined());
                }
            }
        })
    }
}
