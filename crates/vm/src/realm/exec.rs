use std::{mem, u8};

use crate::{
    gc::Trace,
    instructions::{Instruction, Upvalue},
    object::{FunctionKind, Object, ObjectFlags, VmFunction, RECURSIVE_FUNC_PANIC},
    Gc, Value,
};

use super::{builtin, reader::InstructionReader, Realm};

const NOT_A_FUNCTION: &str = "tried to call value which was not a function";
const NOT_A_CONSTRUCTOR: &str = "value is not an constructor";
const INDEX_NON_OBJECT: &str = "tried to index value which as not a object";

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

macro_rules! catch_unwind(
    ($realm:expr,$instr:expr,$ctx:expr,$v:expr) => {
        match $v{
            Ok(x) => x,
            Err(e) => {
                $realm.unwind_error(&mut $instr, &mut $ctx, e)?;
                continue;
            }
        }
    };
);

impl Realm {
    pub unsafe fn execute(
        &self,
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
                    self.vm.borrow().collect_debt(&ctx);
                    let func = self.construct_function(func, &instr, ctx.function);
                    let func = Object::alloc_function(
                        self,
                        self.builtin.function_proto,
                        ObjectFlags::empty(),
                        FunctionKind::Vm(func),
                    );
                    self.stack.write(dst, func.into());
                }
                Instruction::LoadConstructor { dst, func } => {
                    self.vm.borrow().collect_debt(&ctx);
                    let func = self.construct_constructor(func, &instr, ctx.function);
                    self.stack.write(dst, func.into());
                }
                Instruction::LoadThis { dst } => self.stack.write(dst, ctx.this),
                Instruction::Move { dst, src } => self.stack.write(dst, self.stack.read(src)),
                Instruction::CreateObject { dst } => {
                    self.vm.borrow().collect_debt(&ctx);
                    let object =
                        Object::alloc(self, self.builtin.object_proto, ObjectFlags::empty());
                    let res = Value::from(object);
                    self.stack.write(dst, res)
                }
                Instruction::CreateArray { dst } => {
                    self.vm.borrow().collect_debt(&ctx);
                    let object =
                        Object::alloc(self, self.builtin.object_proto, ObjectFlags::empty());
                    let res = Value::from(object);
                    self.stack.write(dst, res)
                }
                Instruction::IndexAssign { obj, key, val } => {
                    let obj = self.stack.read(obj);
                    let key = self.stack.read(key);
                    let val = self.stack.read(val);

                    if obj.is_object() {
                        let obj = obj.unsafe_cast_object();
                        self.vm.borrow().write_barrier(obj);
                        catch_unwind!(self, instr, ctx, obj.index_set(key, val, self));
                    } else {
                        // TODO proper error value
                        self.unwind_error(&mut instr, &mut ctx, Value::undefined())?
                    }
                }
                Instruction::Index { dst, obj, key } => {
                    let obj = self.stack.read(obj);
                    let key = self.stack.read(key);
                    if obj.is_object() {
                        let res = catch_unwind!(
                            self,
                            instr,
                            ctx,
                            obj.unsafe_cast_object().index(key, self)
                        );
                        self.stack.write(dst, res)
                    } else {
                        // TODO proper error value
                        self.unwind_error(&mut instr, &mut ctx, Value::undefined())?
                    }
                }

                Instruction::GlobalIndex { dst, key } => {
                    let key = self.stack.read(key);
                    let obj = self.global;
                    let src = catch_unwind!(self, instr, ctx, obj.index(key, self));
                    self.stack.write(dst, src);
                }

                Instruction::GlobalAssign { key, src } => {
                    let obj = self.global;
                    self.vm.borrow().write_barrier(obj);
                    catch_unwind!(
                        self,
                        instr,
                        ctx,
                        obj.index_set(self.stack.read(key), self.stack.read(src), self)
                    );
                }

                Instruction::Upvalue { dst, slot } => {
                    let value = ctx.function.as_vm_function().upvalues[slot as usize].read();
                    self.stack.write(dst, value);
                }

                Instruction::UpvalueAssign { src, slot } => {
                    let value = self.stack.read(src);
                    let upvalue = ctx.function.as_vm_function().upvalues[slot as usize];
                    self.vm.borrow().write_barrier(upvalue);
                    upvalue.write(value);
                }

                Instruction::TypeOf { dst, src } => {
                    let src = self.stack.read(src);
                    let res = self.type_of(src);
                    let res = self.vm().allocate::<String>(res.into());
                    self.stack.write(dst, res.into());
                }

                Instruction::InstanceOf { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let v = catch_unwind!(self, instr, ctx, self.instance_of(left, right));
                    self.stack.write(dst, v.into());
                }

                Instruction::Add { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    if Self::both_int(left, right) {
                        let res =
                            Self::coerce_int(left.cast_int() as i64 + right.cast_int() as i64);
                        self.stack.write(dst, res);
                    } else {
                        let res = catch_unwind!(self, instr, ctx, self.add(left, right));
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
                        let res = catch_unwind!(
                            self,
                            instr,
                            ctx,
                            self.numeric_operator(left, right, NumericOperator::Sub)
                        );
                        self.stack.write(dst, res);
                    }
                }
                Instruction::Mul { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = catch_unwind!(
                        self,
                        instr,
                        ctx,
                        self.numeric_operator(left, right, NumericOperator::Mul)
                    );
                    self.stack.write(dst, res);
                }
                Instruction::Div { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = catch_unwind!(
                        self,
                        instr,
                        ctx,
                        self.numeric_operator(left, right, NumericOperator::Div)
                    );
                    self.stack.write(dst, res);
                }
                Instruction::Mod { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = catch_unwind!(
                        self,
                        instr,
                        ctx,
                        self.numeric_operator(left, right, NumericOperator::Mod)
                    );
                    self.stack.write(dst, res);
                }
                Instruction::Pow { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = catch_unwind!(
                        self,
                        instr,
                        ctx,
                        self.numeric_operator(left, right, NumericOperator::Pow)
                    );
                    self.stack.write(dst, res);
                }

                Instruction::ShiftLeft { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let left = catch_unwind!(self, instr, ctx, self.to_int32(left));
                    let right = catch_unwind!(self, instr, ctx, self.to_int32(right)) as u32 % 32;
                    self.stack.write(dst, Value::from(left << right));
                }
                Instruction::ShiftRight { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let left = catch_unwind!(self, instr, ctx, self.to_int32(left));
                    let right = catch_unwind!(self, instr, ctx, self.to_int32(right)) as u32 % 32;
                    self.stack.write(dst, Value::from(left >> right));
                }
                Instruction::ShiftUnsigned { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let left = catch_unwind!(self, instr, ctx, self.to_int32(left));
                    let right = catch_unwind!(self, instr, ctx, self.to_int32(right)) as u32 % 32;
                    self.stack.write(dst, Value::from((left >> right) as i32));
                }
                Instruction::Equal { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = catch_unwind!(self, instr, ctx, self.equal(left, right));
                    self.stack.write(dst, Value::from(res));
                }
                Instruction::NotEqual { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = !catch_unwind!(self, instr, ctx, self.equal(left, right));
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
                    let res = catch_unwind!(self, instr, ctx, self.less_then(left, right, true));
                    if res.is_undefined() {
                        self.stack.write(dst, false.into());
                    } else {
                        self.stack.write(dst, res);
                    }
                }
                Instruction::GreaterEq { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = catch_unwind!(self, instr, ctx, self.less_then(left, right, false));
                    let res = res.is_false() && !res.is_undefined();
                    self.stack.write(dst, res.into())
                }

                Instruction::Less { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = catch_unwind!(self, instr, ctx, self.less_then(left, right, false));
                    if res.is_undefined() {
                        self.stack.write(dst, false.into());
                    } else {
                        self.stack.write(dst, res);
                    }
                }
                Instruction::LessEq { dst, left, righ } => {
                    let left = self.stack.read(left);
                    let right = self.stack.read(righ);
                    let res = catch_unwind!(self, instr, ctx, self.less_then(left, right, true));
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
                    let number = catch_unwind!(self, instr, ctx, self.to_number(src));
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
                Instruction::Positive { dst, op } => {
                    let src = self.stack.read(op);
                    let number = catch_unwind!(self, instr, ctx, self.to_number(src));
                    self.stack.write(dst, number);
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

                Instruction::Untry { dst } => {
                    self.stack.write(dst, Value::empty());
                    self.stack.pop_try();
                }

                Instruction::Push { src } => {
                    let src = self.stack.read(src);
                    self.stack.push(src);
                }

                Instruction::Call { dst, func } => {
                    let func = self.stack.read(func);
                    let value = catch_unwind!(
                        self,
                        instr,
                        ctx,
                        self._call(func, &mut instr, &mut ctx, dst)
                    );
                    if let Some(value) = value {
                        self.stack.write(dst, value);
                    }
                }
                Instruction::CallMethod { dst, obj, key } => {
                    let obj = self.stack.read(obj);
                    let key = self.stack.read(key);
                    let value = catch_unwind!(
                        self,
                        instr,
                        ctx,
                        self.call_method(obj, key, &mut instr, &mut ctx, dst)
                    );
                    if let Some(value) = value {
                        self.stack.write(dst, value);
                    }
                }
                Instruction::CallConstruct { dst, func, obj } => {
                    let func = self.stack.read(func);
                    let obj = self.stack.read(obj);
                    let value = catch_unwind!(
                        self,
                        instr,
                        ctx,
                        self.call_construct(func, obj, &mut instr, &mut ctx)
                    );
                    self.stack.write(dst, value);
                }

                Instruction::Throw { src } => {
                    let error = self.stack.read(src);
                    if !error.is_empty() {
                        self.unwind_error(&mut instr, &mut ctx, error)?;
                    }
                }

                Instruction::ReturnUndefined { .. } => {
                    match self.stack.pop(self.vm.borrow().gc()) {
                        Some(frame) => {
                            ctx = frame.ctx;
                            instr = frame.instr;
                            self.stack.write(frame.dst, Value::undefined());
                        }
                        None => return Ok(Value::undefined()),
                    }
                }
                Instruction::Return { ret } => {
                    let return_value = self.stack.read(ret);
                    match self.stack.pop(self.vm.borrow().gc()) {
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
        &self,
        instr: &mut InstructionReader,
        ctx: &mut ExecutionContext,
        f: F,
    ) -> Result<Option<O>, Value>
    where
        F: FnOnce(&Realm, &mut InstructionReader, &mut ExecutionContext) -> Result<R, Value>,
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
        &self,
        instr: &mut InstructionReader,
        ctx: &mut ExecutionContext,
        error: Value,
    ) -> Result<(), Value> {
        loop {
            match self.stack.unwind(self.vm.borrow().gc()) {
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

    pub unsafe fn is_falsish(&self, value: Value) -> bool {
        if value.is_int() {
            value.cast_int() == 0
        } else if value.is_string() {
            value.unsafe_cast_string().is_empty()
        } else {
            value.is_null() || value.is_undefined() || value.is_false()
        }
    }

    pub unsafe fn is_nullish(&self, value: Value) -> bool {
        value.is_null() || value.is_undefined()
    }

    /// Implements type conversion [`Tostring`](https://tc39.es/ecma262/#sec-tostring)
    pub unsafe fn to_string(&self, value: Value) -> Result<Gc<String>, Value> {
        if value.is_int() {
            Ok(self.vm.borrow().allocate(value.cast_int().to_string()))
        } else if value.is_float() {
            Ok(self.vm.borrow().allocate(value.cast_float().to_string()))
        } else if value.is_null() {
            Ok(self.vm.borrow().allocate("null".to_string()))
        } else if value.is_undefined() {
            Ok(self.vm.borrow().allocate("undefined".to_string()))
        } else if value.is_true() {
            Ok(self.vm.borrow().allocate("true".to_string()))
        } else if value.is_false() {
            Ok(self.vm.borrow().allocate("false".to_string()))
        } else if value.is_string() {
            Ok(value.unsafe_cast_string())
        } else if value.is_object() {
            let primitive = self.to_primitive(value, true)?;
            Ok(self.to_string(primitive)?)
        } else {
            todo!()
        }
    }

    pub unsafe fn to_object(&self, value: Value) -> Gc<Object> {
        if value.is_null() || value.is_undefined() {
            return Object::alloc(self, self.builtin.object_proto, ObjectFlags::empty());
        }
        todo!()
    }

    /// Implements type conversion [`ToNumber`](https://tc39.es/ecma262/#sec-tonumber)
    pub unsafe fn to_number(&self, value: Value) -> Result<Value, Value> {
        if value.is_int() || value.is_float() {
            Ok(value)
        } else if value.is_undefined() {
            Ok(Value::nan())
        } else if value.is_null() || value.is_undefined() || value.is_false() {
            Ok(Value::from(0i32))
        } else if value.is_string() {
            if let Ok(v) = value.unsafe_cast_string().parse::<f64>() {
                if v as i32 as f64 == v {
                    Ok(Value::from(v as i32))
                } else {
                    Ok(Value::from(v))
                }
            } else {
                Ok(Value::from(f64::NAN))
            }
        } else if value.is_object() {
            let prim = self.to_primitive(value, false)?;
            Ok(self.to_number(prim)?)
        } else {
            todo!()
        }
    }

    /// Implements type conversion [`ToInt32`](https://tc39.es/ecma262/#sec-toint32)
    pub unsafe fn to_int32(&self, value: Value) -> Result<i32, Value> {
        let number = self.to_number(value)?;
        Ok(if number.is_int() {
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
        })
    }

    pub unsafe fn strict_equal(&self, left: Value, right: Value) -> bool {
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

    pub unsafe fn equal(&self, left: Value, right: Value) -> Result<bool, Value> {
        if left.same_type(right) {
            return Ok(self.strict_equal(left, right));
        }

        if left.is_undefined() && right.is_null() {
            return Ok(true);
        }
        if right.is_undefined() && left.is_null() {
            return Ok(true);
        }
        if left.is_number() && right.is_string() {
            // Should not return error
            let right = self.to_number(right).unwrap();
            return self.equal(left, right);
        }
        if right.is_number() && left.is_string() {
            // Should not return error
            let left = self.to_number(left).unwrap();
            return self.equal(left, right);
        }
        if left.is_bool() {
            // Should not return error
            let left = self.to_number(left).unwrap();
            return self.equal(left, right);
        }
        if right.is_bool() {
            // Should not return error
            let right = self.to_number(right).unwrap();
            return self.equal(left, right);
        }
        if !left.is_object() && right.is_object() {
            let right = self.to_primitive(right, true)?;
            return self.equal(left, right);
        }
        if !right.is_object() && left.is_object() {
            let left = self.to_primitive(left, true)?;
            return self.equal(left, right);
        }
        Ok(false)
    }

    pub unsafe fn less_then(&self, left: Value, right: Value, swap: bool) -> Result<Value, Value> {
        let (left, right) = if swap {
            let r = self.to_primitive(left, false)?;
            let l = self.to_primitive(right, false)?;
            (l, r)
        } else {
            (
                self.to_primitive(left, false)?,
                self.to_primitive(right, false)?,
            )
        };
        if left.is_string() || right.is_string() {
            let left = self.to_string(left)?;
            let right = self.to_string(right)?;
            if left.as_str().starts_with(right.as_str()) {
                return Ok(false.into());
            }
            if right.as_str().starts_with(left.as_str()) {
                return Ok(true.into());
            }
            let mut left = left.as_str().chars();
            let mut right = right.as_str().chars();
            loop {
                let left = left.next().unwrap();
                let right = right.next().unwrap();
                if left != right {
                    return Ok((left < right).into());
                }
            }
        }

        // Left and right are already primitives so they cannot return an error
        let left = self.to_number(left).unwrap();
        let right = self.to_number(right).unwrap();
        if left.is_int() && right.is_string() {
            return Ok((left.cast_int() < right.cast_int()).into());
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
            return Ok(Value::undefined());
        }

        if left.to_bits() == f64::NEG_INFINITY.to_bits()
            || right.to_bits() == f64::INFINITY.to_bits()
        {
            return Ok(true.into());
        }

        if left.to_bits() == f64::INFINITY.to_bits()
            || right.to_bits() == f64::NEG_INFINITY.to_bits()
        {
            return Ok(false.into());
        }
        Ok((left < right).into())
    }

    pub unsafe fn add(&self, left: Value, right: Value) -> Result<Value, Value> {
        let left = self.to_primitive(left, true)?;
        let right = self.to_primitive(right, true)?;
        if left.is_string() || right.is_string() {
            let left = self.to_string(left)?;
            let right = self.to_string(right)?;
            let v = self
                .vm
                .borrow()
                .allocate(left.to_string() + &right.to_string());
            return Ok(v.into());
        }
        let left = self.to_number(left)?;
        let right = self.to_number(right)?;
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
            Ok(Value::from(res as i32))
        } else {
            Ok(Value::from(res))
        }
    }

    #[inline]
    pub unsafe fn numeric_operator(
        &self,
        left: Value,
        right: Value,
        op: NumericOperator,
    ) -> Result<Value, Value> {
        let left = self.to_number(left)?;
        let right = self.to_number(right)?;
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
            Ok(Value::from(res as i32))
        } else {
            Ok(Value::from(res))
        }
    }

    pub unsafe fn type_of(&self, v: Value) -> &str {
        if v.is_undefined() {
            "undefined"
        } else if v.is_null() {
            "object"
        } else if v.is_bool() {
            "boolean"
        } else if v.is_number() {
            "number"
        } else if v.is_string() {
            "string"
        } else if v.is_object() {
            if v.unsafe_cast_object().is_function() {
                "function"
            } else {
                "object"
            }
        } else {
            unreachable!()
        }
    }

    pub unsafe fn instance_of(&self, left: Value, right: Value) -> Result<bool, Value> {
        if !right.is_object() {
            return Err(self.create_type_error("invalid `instanceof` operand"));
        }

        if !left.is_object() {
            return Ok(false);
        }

        let right = right.unsafe_cast_object();
        let left = left.unsafe_cast_object();
        // TODO implement @@hasInstance method

        if right.is_function() {
            return Err(self.create_type_error("right hand instanceof operand is not a function"));
        }

        let tgt_proto = right
            .index(self.builtin.keys.as_ref().unwrap().prototype.into(), self)
            .unwrap();
        if !tgt_proto.is_object() {
            return Err(self.create_type_error("object prototype is not an object"));
        }
        let tgt_proto = tgt_proto.unsafe_cast_object();

        let mut cur = left;
        while let Some(proto) = cur.prototype {
            if proto.ptr_eq(tgt_proto) {
                return Ok(true);
            }
            cur = proto;
        }
        Ok(false)
    }

    #[inline]
    pub unsafe fn to_primitive(&self, v: Value, prefer_string: bool) -> Result<Value, Value> {
        if v.is_object() {
            let v = v.unsafe_cast_object();
            //TODO @@toPrimitive

            let keys = if prefer_string {
                [
                    self.vm().allocate::<String>("toString".into()),
                    self.vm().allocate::<String>("valueOf".into()),
                ]
            } else {
                [
                    self.vm().allocate::<String>("valueOf".into()),
                    self.vm().allocate::<String>("toString".into()),
                ]
            };
            for k in keys {
                let func = v.index(k.into(), self).unwrap();
                if func.is_object() {
                    let func = func.unsafe_cast_object();
                    if func.is_function() {
                        let res = self.enter_method_call(func, v.into())?;
                        if !res.is_object() {
                            return Ok(res);
                        }
                    }
                }
            }
            return Err(self.create_type_error("Could not create a primitive from object"));
        }
        Ok(v)
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
        &self,
        function_id: u16,
        reader: &InstructionReader,
        function: Gc<Object>,
    ) -> VmFunction {
        let bc_function = reader.function(function_id);
        let function = function.as_vm_function();
        let upvalues = bc_function
            .upvalues
            .iter()
            .copied()
            .map(|x| match x {
                Upvalue::Local(register) => {
                    self.stack.create_upvalue(register, self.vm.borrow().gc())
                }
                Upvalue::Parent(slot) => {
                    debug_assert!(function.upvalues.len() > slot as usize);
                    *function.upvalues.get_unchecked(slot as usize)
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        VmFunction {
            bc: reader.bc,
            function: function_id,
            upvalues,
        }
    }

    pub(crate) unsafe fn construct_constructor(
        &self,
        function_id: u16,
        reader: &InstructionReader,
        function: Gc<Object>,
    ) -> Gc<Object> {
        let function = self.construct_function(function_id, reader, function);
        let function =
            Object::alloc_constructor(self, self.builtin.object_proto, FunctionKind::Vm(function));
        let proto = Object::alloc(self, self.builtin.object_proto, ObjectFlags::empty());
        proto
            .raw_index_set(
                self.builtin.keys.as_ref().unwrap().constructor.into(),
                function.into(),
                self,
            )
            .unwrap();
        function
            .raw_index_set(
                self.builtin.keys.as_ref().unwrap().prototype.into(),
                proto.into(),
                self,
            )
            .unwrap();
        function
    }

    unsafe fn call_method(
        &self,
        object: Value,
        key: Value,
        instr: &mut InstructionReader,
        ctx: &mut ExecutionContext,
        dst: u8,
    ) -> Result<Option<Value>, Value> {
        if !object.is_object() {
            return Err(self.create_type_error(INDEX_NON_OBJECT));
        }
        let object = object.unsafe_cast_object();
        let function = object.index(key, self)?;
        if !function.is_object() {
            return Err(self.create_type_error(NOT_A_FUNCTION));
        }
        let function = function.unsafe_cast_object();
        if !function.is_function() {
            return Err(self.create_type_error(NOT_A_FUNCTION));
        }

        let kind = function.function.as_ref().unwrap();
        match kind {
            FunctionKind::Vm(ref func) => {
                let bc = func.bc;
                let bc_function = &bc.functions[func.function as usize];
                self.stack
                    .enter_call(bc_function.registers, dst, *instr, *ctx);
                ctx.function = function;
                ctx.this = object.into();
                *instr = InstructionReader::from_bc(bc, func.function);
                Ok(None)
            }
            FunctionKind::Static(x) => {
                let function = mem::replace(&mut ctx.function, function);
                self.stack.enter(0);
                let res = x(self, ctx)?;
                self.stack.pop(self.vm.borrow().gc());
                ctx.function = function;
                Ok(Some(res))
            }
            FunctionKind::Mutable(ref x) => {
                let function = mem::replace(&mut ctx.function, function);
                let this = mem::replace(&mut ctx.this, object.into());
                self.stack.enter(0);
                let res = x.try_borrow_mut().expect(RECURSIVE_FUNC_PANIC)(self, ctx)?;
                self.stack.pop(self.vm.borrow().gc());
                ctx.this = this;
                ctx.function = function;
                Ok(Some(res))
            }
            FunctionKind::Shared(ref x) => {
                let function = mem::replace(&mut ctx.function, function);
                let this = mem::replace(&mut ctx.this, object.into());
                self.stack.enter(0);
                let res = (*x)(self, ctx)?;
                self.stack.pop(self.vm.borrow().gc());
                ctx.this = this;
                ctx.function = function;
                Ok(Some(res))
            }
        }
    }

    /// Execution only version of call
    unsafe fn _call(
        &self,
        funct: Value,
        instr: &mut InstructionReader,
        ctx: &mut ExecutionContext,
        dst: u8,
    ) -> Result<Option<Value>, Value> {
        if !funct.is_object() {
            return Err(self.create_type_error(NOT_A_FUNCTION));
        }
        let function = funct.unsafe_cast_object();
        let kind = if let Some(x) = function.function.as_ref() {
            x
        } else {
            return Err(self.create_type_error(NOT_A_FUNCTION));
        };
        match kind {
            FunctionKind::Vm(ref func) => {
                let bc = func.bc;
                let bc_function = &bc.functions[func.function as usize];
                self.stack
                    .enter_call(bc_function.registers, dst, *instr, *ctx);
                ctx.function = function;
                ctx.this = funct;
                *instr = InstructionReader::from_bc(bc, func.function);
                Ok(None)
            }
            FunctionKind::Static(x) => {
                let function = mem::replace(&mut ctx.function, function);
                self.stack.enter(0);
                let res = x(self, ctx)?;
                self.stack.pop(self.vm.borrow().gc());
                ctx.function = function;
                Ok(Some(res))
            }
            FunctionKind::Mutable(ref x) => {
                let function = mem::replace(&mut ctx.function, function);
                let this = mem::replace(&mut ctx.this, funct);
                self.stack.enter(0);
                let res = x.try_borrow_mut().expect(RECURSIVE_FUNC_PANIC)(self, ctx)?;
                self.stack.pop(self.vm.borrow().gc());
                ctx.this = this;
                ctx.function = function;
                Ok(Some(res))
            }
            FunctionKind::Shared(ref x) => {
                let function = mem::replace(&mut ctx.function, function);
                let this = mem::replace(&mut ctx.this, funct);
                self.stack.enter(0);
                let res = (*x)(self, ctx)?;
                self.stack.pop(self.vm.borrow().gc());
                ctx.this = this;
                ctx.function = function;
                Ok(Some(res))
            }
        }
    }

    unsafe fn call_construct(
        &self,
        function: Value,
        target_obj: Value,
        _instr: &mut InstructionReader,
        ctx: &mut ExecutionContext,
    ) -> Result<Value, Value> {
        if !function.is_object() {
            return Err(self.create_type_error(NOT_A_CONSTRUCTOR));
        }

        let function_obj = function.unsafe_cast_object();
        let kind = if let Some(x) = function_obj.function.as_ref() {
            x
        } else {
            return Err(self.create_type_error(NOT_A_CONSTRUCTOR));
        };
        if !function_obj.is_constructor() {
            return Err(self.create_type_error(NOT_A_CONSTRUCTOR));
        }

        match kind {
            FunctionKind::Vm(ref func) => {
                let bc = func.bc;
                let bc_function = &bc.functions[func.function as usize];
                self.stack.enter(bc_function.registers);

                let proto = function_obj
                    .index(self.builtin.keys.as_ref().unwrap().prototype.into(), self)
                    .unwrap();
                let this_object = if proto.is_object() {
                    Object::alloc(self, Some(proto.unsafe_cast_object()), ObjectFlags::empty())
                } else {
                    Object::alloc(self, self.builtin.object_proto, ObjectFlags::empty())
                };

                let instr = InstructionReader::from_bc(bc, func.function);
                let res = self.execute(
                    instr,
                    ExecutionContext {
                        function: function_obj,
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
                let function = mem::replace(&mut ctx.function, function_obj);
                self.stack.enter(0);
                let res = func(self, ctx)?;
                self.stack.pop(self.vm.borrow().gc());
                ctx.function = function;
                Ok(res)
            }
            FunctionKind::Shared(func) => {
                let function = mem::replace(&mut ctx.function, function_obj);
                self.stack.enter(0);
                let res = func(self, ctx)?;
                self.stack.pop(self.vm.borrow().gc());
                ctx.function = function;
                Ok(res)
            }
            FunctionKind::Mutable(func) => {
                let function = mem::replace(&mut ctx.function, function_obj);
                self.stack.enter(0);
                let res = func.try_borrow_mut().expect(RECURSIVE_FUNC_PANIC)(self, ctx)?;
                self.stack.pop(self.vm.borrow().gc());
                ctx.function = function;
                Ok(res)
            }
        }
    }

    pub unsafe fn create_type_error(&self, message: impl Into<String>) -> Value {
        let message = self.vm().allocate(message.into());
        builtin::error::construct(
            self,
            self.builtin.type_error_proto.unwrap(),
            Some(message),
            None,
        )
        .into()
    }

    pub unsafe fn create_syntax_error(&self, message: impl Into<String>) -> Value {
        let message = self.vm().allocate(message.into());
        builtin::error::construct(
            self,
            self.builtin.syntax_error_proto.unwrap(),
            Some(message),
            None,
        )
        .into()
    }
}
