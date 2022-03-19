use std::borrow::Cow;

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

pub struct ExecutionContext<U: 'static> {
    pub instr: InstructionReader,
    pub function: Gc<Object<U>>,
    pub this: Value,
    pub new_target: Value,
}

impl<U> Copy for ExecutionContext<U> {}
impl<U> Clone for ExecutionContext<U> {
    fn clone(&self) -> Self {
        *self
    }
}

unsafe impl<U> Trace for ExecutionContext<U> {
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

impl<U: Trace> Realm<U> {
    pub unsafe fn execute(&mut self, mut ctx: ExecutionContext<U>) -> Result<Value, Value> {
        loop {
            match ctx.instr.next() {
                Instruction::LoadConst { dst, cons } => {
                    self.stack.write(dst, ctx.instr.constant(cons as u32));
                }
                Instruction::LoadGlobal { dst } => {
                    self.stack.write(dst, Value::from(self.global));
                }
                Instruction::LoadFunction { dst, func } => {
                    self.gc.collect_debt(&(&*self, &ctx));
                    let func = self.construct_function(func, &ctx.instr, ctx.function);
                    let res = Value::from(self.gc.allocate(func));
                    self.stack.write(dst, res);
                }
                Instruction::LoadConstructor { dst, func } => {
                    self.gc.collect_debt(&(&*self, &ctx));
                    let func = self.construct_constructor(func, &ctx.instr, ctx.function);
                    self.stack.write(dst, func.into());
                }
                Instruction::LoadThis { dst } => self.stack.write(dst, ctx.this),
                Instruction::Move { dst, src } => self.stack.write(dst, self.stack.read(src)),
                Instruction::CreateObject { dst } => {
                    self.gc.collect_debt(&(&*self, ctx.instr, ctx.function));
                    let object = Object::<U>::new(None);
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
                        todo!()
                    }
                }
                Instruction::Index { dst, obj, key } => {
                    let obj = self.stack.read(obj);
                    let key = self.stack.read(key);
                    if obj.is_object() {
                        let res = obj.unsafe_cast_object().index(key, self);
                        self.stack.write(dst, res)
                    } else {
                        todo!()
                    }
                }

                Instruction::GlobalIndex { dst, key } => {
                    let key = self.stack.read(key);
                    let src = self.global().index(key, self);
                    self.stack.write(dst, src);
                }

                Instruction::GlobalAssign { key, src } => {
                    self.global()
                        .index_set(self.stack.read(key), self.stack.read(src), self);
                }

                Instruction::Upvalue { dst, slot } => {
                    let value = ctx.function.as_vm_function().upvalues[slot as usize].read();
                    self.stack.write(dst, value);
                }

                Instruction::UpvalueAssign { src, slot } => {
                    let value = self.stack.read(src);
                    ctx.function.as_vm_function().upvalues[slot as usize].write(value);
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
                        self.stack.write(dst, res.into());
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
                Instruction::Jump { tgt } => ctx.instr.jump(tgt),
                Instruction::JumpFalse { cond, tgt } => {
                    let cond = self.stack.read(cond);
                    if self.is_falsish(cond) {
                        ctx.instr.jump(tgt)
                    }
                }
                Instruction::JumpTrue { cond, tgt } => {
                    let cond = self.stack.read(cond);
                    if !self.is_falsish(cond) {
                        ctx.instr.jump(tgt)
                    }
                }

                Instruction::Try { dst, tgt } => self
                    .stack
                    .push_try(dst, ctx.instr.absolute_offset(tgt) as usize),

                Instruction::Untry { _ignore: () } => {
                    self.stack.pop_try();
                }

                Instruction::Push { src } => {
                    let src = self.stack.read(src);
                    self.stack.push(src);
                }

                Instruction::Call { dst, func } => {
                    let func = self.stack.read(func);
                    match self.call(func, &mut ctx, dst) {
                        Ok(Some(value)) => {
                            self.stack.write(dst, value);
                        }
                        Ok(None) => {}
                        Err(error) => {
                            self.unwind_error(&mut ctx, error)?;
                        }
                    }
                }
                Instruction::Construct { dst, func, obj } => {
                    let func = self.stack.read(func);
                    let obj = self.stack.read(obj);
                    match self.construct(func, obj, &mut ctx) {
                        Ok(Some(value)) => self.stack.write(dst, value),
                        Ok(None) => {}
                        Err(error) => {
                            self.unwind_error(&mut ctx, error)?;
                        }
                    }
                }

                Instruction::Throw { src } => {
                    let error = self.stack.read(src);
                    self.unwind_error(&mut ctx, error)?;
                }

                Instruction::ReturnUndefined { .. } => match self.stack.pop() {
                    Some(frame) => {
                        ctx = frame.ctx;
                        self.stack.write(frame.dst, Value::undefined());
                    }
                    None => return Ok(Value::undefined()),
                },
                Instruction::Return { ret } => {
                    let return_value = self.stack.read(ret);
                    match self.stack.pop() {
                        Some(frame) => {
                            ctx = frame.ctx;
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
        ctx: &mut ExecutionContext<U>,
        f: F,
    ) -> Result<Option<O>, Value>
    where
        F: FnOnce(&mut Self, &mut ExecutionContext<U>) -> Result<R, Value>,
        R: Into<Option<O>>,
    {
        match f(self, ctx) {
            Ok(x) => Ok(x.into()),
            Err(e) => {
                self.unwind_error(ctx, e)?;
                Ok(None)
            }
        }
    }

    pub unsafe fn unwind_error(
        &mut self,
        ctx: &mut ExecutionContext<U>,
        error: Value,
    ) -> Result<(), Value> {
        loop {
            match self.stack.unwind() {
                Some(Ok(catch)) => {
                    self.stack.write(catch.dst, error);
                    ctx.instr.absolute_jump(catch.ip_offset);
                    return Ok(());
                }
                Some(Err(frame)) => {
                    *ctx = frame.ctx;
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
    pub unsafe fn to_string<'a>(&mut self, value: Value) -> Cow<'a, str> {
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
        } else {
            todo!()
        }
    }

    pub unsafe fn to_object(&mut self, value: Value) -> Gc<Object<U>> {
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
            return left
                .unsafe_cast_object::<U>()
                .ptr_eq(right.unsafe_cast_object());
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
            if left.as_ref().starts_with(right.as_ref()) {
                return false.into();
            }
            if right.as_ref().starts_with(left.as_ref()) {
                return true.into();
            }
            let mut left = left.as_ref().chars();
            let mut right = right.as_ref().chars();
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
        function: Gc<Object<U>>,
    ) -> Object<U> {
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
        function: Gc<Object<U>>,
    ) -> Gc<Object<U>> {
        let mut function = self.construct_function(function_id, reader, function);
        function.flags |= ObjectFlags::CONSTRUCTOR;
        let function = self.gc.allocate(function);
        let proto = self.create_object();
        let key = self.create_string("constructor").into();
        proto.raw_index_set(key, function.into(), self);
        let key = self.create_string("prototype").into();
        function.raw_index_set(key, proto.into(), self);
        function
    }

    pub(crate) unsafe fn construct_function_root(
        &mut self,
        reader: &InstructionReader,
    ) -> Gc<Object<U>> {
        let bc_function = reader.function(0);
        debug_assert!(bc_function.upvalues.is_empty());
        self.gc.allocate(Object::from_vm(
            self,
            VmFunction {
                bc: reader.bc,
                function: 0,
                upvalues: Box::new([]),
            },
        ))
    }

    unsafe fn call(
        &mut self,
        function: Value,
        ctx: &mut ExecutionContext<U>,
        dst: u8,
    ) -> Result<Option<Value>, Value> {
        self.unwind(ctx, |this, ctx| {
            if !function.is_object() {
                todo!()
            }
            let function = function.unsafe_cast_object();
            let kind = if let Some(x) = function.function.as_ref() {
                x
            } else {
                todo!()
            };
            match kind {
                FunctionKind::Vm(ref func) => {
                    let bc = func.bc;
                    let bc_function = &bc.functions[func.function as usize];
                    this.stack.enter_call(bc_function.registers, dst, *ctx);
                    ctx.function = function;
                    ctx.instr = InstructionReader::from_bc(bc, func.function);
                    return Ok(None);
                }
                FunctionKind::Static(x) => {
                    this.stack.enter(0);
                    let res = x(this, ctx)?;
                    this.stack.pop();
                    Ok(Some(res))
                }
                FunctionKind::Mutable(ref x) => {
                    this.stack.enter(0);
                    let res = x.try_borrow_mut().expect(RECURSIVE_FUNC_PANIC)(this, ctx)?;
                    this.stack.pop();
                    Ok(Some(res))
                }
                FunctionKind::Shared(ref x) => {
                    this.stack.enter(0);
                    let res = (*x)(this, ctx)?;
                    this.stack.pop();
                    Ok(Some(res))
                }
            }
        })
    }

    unsafe fn construct(
        &mut self,
        function: Value,
        target_obj: Value,
        ctx: &mut ExecutionContext<U>,
    ) -> Result<Option<Value>, Value> {
        self.unwind(ctx, |this, ctx| {
            if !function.is_object() {
                todo!()
            }

            let function = function.unsafe_cast_object();
            let kind = if let Some(x) = function.function.as_ref() {
                x
            } else {
                todo!()
            };
            if !function.is_constructor() {
                todo!()
            }
            match kind {
                FunctionKind::Vm(ref func) => {
                    let bc = func.bc;
                    let bc_function = &bc.functions[func.function as usize];
                    this.stack.enter(bc_function.registers);
                    let this_object = this.create_object();
                    let res = this.execute(ExecutionContext {
                        function,
                        this: this_object.into(),
                        instr: InstructionReader::from_bc(bc, func.function),
                        new_target: target_obj,
                    })?;
                    if res.is_object() {
                        Ok(res)
                    } else {
                        Ok(this_object.into())
                    }
                }
                FunctionKind::Static(func) => Ok(func(this, ctx)?),
                _ => todo!(),
            }
        })
    }
}
