use crate::{
    atom::Atoms,
    cell::CellOwner,
    gc::{Arena, Gc},
    instructions::{GcByteCode, Instruction, Upvalue},
    object::{ObjectKind, VmFunction},
    rebind, GcObject, Object, Realm, Value,
};

use super::{ExecutionContext, GcRealm};

pub enum NumericOperator {
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

#[derive(Clone, Copy)]
pub struct InstructionReader<'gc, 'cell> {
    bc: GcByteCode<'gc, 'cell>,
    cur: *const Instruction,
    #[cfg(debug_assertions)]
    first: *const Instruction,
    #[cfg(debug_assertions)]
    last: *const Instruction,
}

impl<'gc, 'cell> InstructionReader<'gc, 'cell> {
    /// # Safety
    ///
    /// `func` must be a smaller the amount of function present in the bytecode.
    ///
    pub unsafe fn new_unsafe(
        owner: &CellOwner<'cell>,
        bc: GcByteCode<'gc, 'cell>,
        func: u16,
    ) -> Self {
        debug_assert!(bc.borrow(owner).functions.len() > func as usize);
        let function = bc.borrow(owner).functions.get_unchecked(func as usize);
        let cur = bc
            .borrow(owner)
            .instructions
            .as_ptr()
            .add(function.offset as usize);

        #[cfg(debug_assertions)]
        let first = cur;
        #[cfg(debug_assertions)]
        let last = cur.add(function.size as usize);
        InstructionReader {
            bc,
            cur,
            #[cfg(debug_assertions)]
            first,
            #[cfg(debug_assertions)]
            last,
        }
    }

    pub fn bc(&self) -> GcByteCode<'gc, 'cell> {
        self.bc
    }

    /// # Safety
    ///
    /// User must ensure that the reader does not read past the end of the instruction buffer
    pub unsafe fn next(&mut self, _owner: &CellOwner<'cell>) -> Instruction {
        #[cfg(debug_assertions)]
        assert!(self.cur < self.last);
        let res = self.cur.read();
        self.cur = self.cur.add(1);
        res
    }

    pub unsafe fn jump(&mut self, mut offset: i16) {
        offset -= 1;
        #[cfg(debug_assertions)]
        assert!(self.cur.offset(offset.into()) < self.last);
        #[cfg(debug_assertions)]
        assert!(self.cur.offset(offset.into()) >= self.first);
        self.cur = self.cur.offset(offset.into());
    }

    /// # Safety
    ///
    /// `idx` must be smaller or equal to the amount of constants in the containted bytecode.
    pub unsafe fn constant(&self, idx: u16, owner: &CellOwner<'cell>) -> Value<'gc, 'cell> {
        debug_assert!(self.bc.borrow(owner).constants.len() > idx as usize);
        *self.bc.borrow(owner).constants.get_unchecked(idx as usize)
    }
}

macro_rules! rebind_try {
    ($arena:expr, $value:expr) => {
        match $value {
            Ok(x) => x,
            Err(e) => return Err(rebind!($arena, e)),
        }
    };
}

impl<'gc, 'cell> GcRealm<'gc, 'cell> {
    // Shorthand for Gc::borrow
    fn r<'a>(self, owner: &'a CellOwner<'cell>) -> &'a Realm<'gc, 'cell> {
        self.borrow(owner)
    }

    // Shorthand for Gc::unsafe_borrow_mut
    //
    // Should only be used as long as before each collection and return from the loop the a
    // write_barrier is done for the realm
    unsafe fn w<'a>(self, owner: &'a mut CellOwner<'cell>) -> &'a mut Realm<'gc, 'cell> {
        self.unsafe_borrow_mut(owner)
    }

    /// # Safety
    ///
    /// Instruction reader must read valid bytecode.
    #[allow(unused_unsafe)]
    pub unsafe fn run<'l>(
        self,
        arena: &'l mut Arena<'_, 'cell>,
        owner: &mut CellOwner<'cell>,
        atoms: &Atoms,
        upper_instr: &mut InstructionReader<'_, 'cell>,
        ctx: &ExecutionContext<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let mut instr = *upper_instr;
        loop {
            match instr.next(owner) {
                Instruction::LoadConst { dst, cons } => {
                    let con = instr.constant(cons, owner);
                    self.w(owner).stack.write(dst, con);
                }
                Instruction::LoadGlobal { dst } => {
                    let global = self.r(owner).builtin.global;
                    self.w(owner).stack.write(dst, global.into());
                }
                Instruction::LoadFunction { dst, func } => {
                    arena.write_barrier(self);
                    arena.collect(owner);

                    let fp = self.r(owner).builtin.function_proto;
                    let func = self.construct_function(owner, arena, func, &instr, ctx.function);
                    let func = Object::new_alloc(arena, Some(fp), ObjectKind::VmFn(func));
                    let func = arena.add(func);
                    self.w(owner).stack.write(dst, func.into());
                }

                Instruction::LoadThis { dst } => {
                    self.w(owner).stack.write(dst, ctx.this);
                }
                Instruction::LoadTarget { dst } => {
                    self.w(owner).stack.write(dst, ctx.new_target);
                }
                Instruction::Move { dst, src } => {
                    let src = self.r(owner).stack.read(src);
                    self.w(owner).stack.write(dst, src);
                }
                Instruction::CreateObject { dst } => {
                    // Always retrace the current realm to allow ring without a write barrier
                    // otherwise.
                    arena.write_barrier(self);
                    arena.collect(owner);
                    let op = self.r(owner).builtin.object_proto;
                    let object = arena.add(Object::new(Some(op), ObjectKind::Ordinary));
                    self.w(owner).stack.write(dst, object.into());
                }
                Instruction::CreateArray { dst } => {
                    // Always retrace the current realm to allow ring without a write barrier
                    // otherwise.
                    arena.write_barrier(self);
                    arena.collect(owner);
                    let object = arena.add(Object::new(None, ObjectKind::Ordinary));
                    self.w(owner).stack.write(dst, object.into());
                }
                Instruction::Index { dst, obj, key } => {
                    let key = self.r(owner).stack.read(key);
                    let obj = self.r(owner).stack.read(obj);
                    if let Some(obj) = obj.into_object() {
                        let res =
                            rebind_try!(arena, obj.index_value(owner, arena, atoms, self, key));
                        self.w(owner).stack.write(dst, res);
                    } else {
                        todo!("index {:?}", obj);
                    }
                }
                Instruction::IndexAssign { obj, key, src } => {
                    let src = self.r(owner).stack.read(src);
                    let key = self.r(owner).stack.read(key);
                    let obj = self.r(owner).stack.read(obj);
                    if let Some(obj) = obj.into_object() {
                        rebind_try!(
                            arena,
                            obj.index_set_value(owner, arena, self, atoms, key, src)
                        );
                    } else {
                        todo!("index {:?}", obj);
                    }
                }
                Instruction::GlobalIndex { dst, key } => {
                    let key = self.r(owner).stack.read(key);
                    let obj = self.r(owner).builtin.global;

                    let res = rebind_try!(arena, obj.index_value(owner, arena, atoms, self, key));
                    self.w(owner).stack.write(dst, res);
                }
                Instruction::GlobalAssign { key, src } => {
                    let src = self.r(owner).stack.read(src);
                    let key = self.r(owner).stack.read(key);
                    let obj = self.r(owner).builtin.global;
                    rebind_try!(
                        arena,
                        obj.index_set_value(owner, arena, self, atoms, key, src)
                    );
                }

                Instruction::Upvalue { dst, slot } => {
                    let upvalue =
                        ctx.function.borrow(owner).as_vm_function().upvalues[slot as usize];
                    let res = upvalue.borrow(owner).read();
                    self.w(owner).stack.write(dst, res);
                }

                Instruction::UpvalueAssign { src, slot } => {
                    let src = self.r(owner).stack.read(src);
                    let upvalue =
                        ctx.function.borrow(owner).as_vm_function().upvalues[slot as usize];
                    upvalue.borrow_mut(owner, arena).write(src);
                }

                Instruction::TypeOf { dst, src } => {
                    let src = self.r(owner).stack.read(src);
                    let res = self.type_of(owner, src);
                    let res = arena.add(String::from(res));
                    self.w(owner).stack.write(dst, res.into());
                }

                Instruction::InstanceOf { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);

                    let res = rebind_try!(arena, self.instance_of(owner, arena, left, right));
                    self.w(owner).stack.write(dst, res.into());
                }

                Instruction::Add { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);

                    if let (Some(left), Some(right)) = (left.into_int(), right.into_int()) {
                        let res = Self::coerce_int(left as i64 + right as i64);
                        self.w(owner).stack.write(dst, res);
                    } else {
                        let res = rebind_try!(arena, self.add(owner, arena, left, right));
                        self.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::Sub { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    if let (Some(left), Some(right)) = (left.into_int(), right.into_int()) {
                        let res = Self::coerce_int(left as i64 - right as i64);
                        self.w(owner).stack.write(dst, res);
                    } else {
                        let res = rebind_try!(
                            arena,
                            self.numeric_operator(owner, arena, left, right, NumericOperator::Sub)
                        );
                        self.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::Mul { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.numeric_operator(owner, arena, left, right, NumericOperator::Mul)
                    );
                    self.w(owner).stack.write(dst, res);
                }
                Instruction::Div { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.numeric_operator(owner, arena, left, right, NumericOperator::Div)
                    );
                    self.w(owner).stack.write(dst, res);
                }
                Instruction::Mod { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.numeric_operator(owner, arena, left, right, NumericOperator::Mod)
                    );
                    self.w(owner).stack.write(dst, res);
                }
                Instruction::Pow { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.numeric_operator(owner, arena, left, right, NumericOperator::Pow)
                    );
                    self.w(owner).stack.write(dst, res);
                }
                Instruction::BitwiseAnd { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, left));
                    let right = rebind_try!(arena, self.to_int32(owner, arena, right));
                    let res = left & right;
                    self.w(owner).stack.write(dst, res.into());
                }
                Instruction::BitwiseOr { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, left));
                    let right = rebind_try!(arena, self.to_int32(owner, arena, right));
                    let res = left | right;
                    self.w(owner).stack.write(dst, res.into());
                }
                Instruction::BitwiseXor { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, left));
                    let right = rebind_try!(arena, self.to_int32(owner, arena, right));
                    let res = left ^ right;
                    self.w(owner).stack.write(dst, res.into());
                }
                Instruction::BitwiseNot { dst, src } => {
                    let src = self.r(owner).stack.read(src);
                    let src = rebind_try!(arena, self.to_int32(owner, arena, src));
                    self.w(owner).stack.write(dst, (!src).into());
                }
                Instruction::ShiftLeft { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, left));
                    let right = rebind_try!(arena, self.to_int32(owner, arena, right)) as u32 % 32;
                    self.w(owner).stack.write(dst, Value::from(left << right));
                }
                Instruction::ShiftRight { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, left));
                    let right = rebind_try!(arena, self.to_int32(owner, arena, right)) as u32 % 32;
                    self.w(owner).stack.write(dst, Value::from(left >> right));
                }
                Instruction::ShiftUnsigned { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_uint32(owner, arena, left));
                    let right = rebind_try!(arena, self.to_int32(owner, arena, right)) as u32 % 32;
                    let v = left >> right;
                    if v > i32::MAX as u32 {
                        self.w(owner).stack.write(dst, Value::from(v as f64));
                    } else {
                        self.w(owner).stack.write(dst, Value::from(v as i32));
                    }
                }
                Instruction::Equal { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(arena, self.equal(owner, arena, left, right));
                    self.w(owner).stack.write(dst, Value::from(res));
                }
                Instruction::NotEqual { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = !rebind_try!(arena, self.equal(owner, arena, left, right));
                    self.w(owner).stack.write(dst, Value::from(res));
                }

                Instruction::SEqual { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = self.strict_equal(owner, left, right);
                    self.w(owner).stack.write(dst, Value::from(res));
                }
                Instruction::SNotEqual { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = !self.strict_equal(owner, left, right);
                    self.w(owner).stack.write(dst, Value::from(res));
                }

                Instruction::Greater { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(arena, self.less_then(owner, arena, left, right, true));
                    if res.is_undefined() {
                        self.w(owner).stack.write(dst, false.into());
                    } else {
                        self.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::GreaterEq { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(arena, self.less_then(owner, arena, left, right, false));
                    let res = res.is_false() && !res.is_undefined();
                    self.w(owner).stack.write(dst, res.into());
                }

                Instruction::Less { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(arena, self.less_then(owner, arena, left, right, false));
                    if res.is_undefined() {
                        self.w(owner).stack.write(dst, false.into());
                    } else {
                        self.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::LessEq { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(arena, self.less_then(owner, arena, left, right, true));
                    let res = res.is_false() && !res.is_undefined();
                    self.w(owner).stack.write(dst, res.into());
                }

                Instruction::IsNullish { dst, op } => {
                    let src = self.r(owner).stack.read(op);
                    let nullish = self.is_nullish(src);
                    self.w(owner).stack.write(dst, Value::from(nullish));
                }
                Instruction::Not { dst, src } => {
                    let src = self.r(owner).stack.read(src);
                    let falsish = self.is_falsish(owner, src);
                    self.w(owner).stack.write(dst, Value::from(falsish));
                }
                Instruction::Negative { dst, op } => {
                    let src = self.r(owner).stack.read(op);
                    {
                        let number = rebind_try!(arena, self.to_number(owner, arena, src));
                        if let Some(number) = number.into_int() {
                            let number = -(number as i64);
                            if number as i32 as i64 == number {
                                self.w(owner).stack.write(dst, Value::from(number as i32))
                            } else {
                                self.w(owner).stack.write(dst, Value::from(number as f64))
                            }
                        } else if let Some(number) = number.into_float() {
                            let number = -number;
                            if number as i32 as f64 == number {
                                self.w(owner).stack.write(dst, Value::from(number as i32));
                            } else {
                                self.w(owner).stack.write(dst, Value::from(number));
                            }
                        } else {
                            unreachable!()
                        }
                    }
                }
                Instruction::Positive { dst, op } => {
                    let src = self.r(owner).stack.read(op);
                    let number = rebind_try!(arena, self.to_number(owner, arena, src));
                    self.w(owner).stack.write(dst, number);
                }

                Instruction::Jump { tgt } => instr.jump(tgt),
                Instruction::JumpFalse { cond, tgt } => {
                    let cond = self.r(owner).stack.read(cond);
                    if self.is_falsish(owner, cond) {
                        instr.jump(tgt)
                    }
                }
                Instruction::JumpTrue { cond, tgt } => {
                    let cond = self.r(owner).stack.read(cond);
                    if !self.is_falsish(owner, cond) {
                        instr.jump(tgt)
                    }
                }

                Instruction::Try { dst, tgt } => {
                    match self.run(arena, owner, atoms, &mut instr, ctx) {
                        Ok(_) => {
                            self.w(owner).stack.write(dst, Value::empty());
                        }
                        Err(e) => {
                            self.w(owner).stack.write(dst, e);
                            instr.jump(tgt);
                        }
                    }
                }

                Instruction::Untry { dst: _ } => {
                    *upper_instr = instr;
                    return Ok(Value::empty());
                }

                Instruction::Throw { src } => {
                    let src = self.r(owner).stack.read(src);
                    if !src.is_empty() {
                        return Err(rebind!(arena, src));
                    }
                }

                Instruction::Call { dst, func } => {
                    let func = self.r(owner).stack.read(func);
                    if let Some(obj) = func.into_object() {
                        let ctx = ExecutionContext {
                            function: obj,
                            this: obj.into(),
                            new_target: Value::null(),
                        };
                        let res = rebind_try!(arena, self.call(owner, arena, atoms, ctx));
                        self.w(owner).stack.write(dst, res);
                    } else {
                        todo!("call not object")
                    }
                }

                Instruction::Push { src } => {
                    let src = self.r(owner).stack.read(src);
                    self.w(owner).stack.push(src);
                }

                Instruction::Return { ret } => {
                    let res = self.r(owner).stack.read(ret);
                    arena.write_barrier(self);
                    return Ok(rebind!(arena, res));
                }
                Instruction::ReturnUndefined { _ignore } => {
                    arena.write_barrier(self);
                    return Ok(Value::undefined());
                }
                x => todo!("Instruction {}", x),
            }
        }
    }

    pub fn is_falsish<'l>(self, owner: &CellOwner<'cell>, value: Value<'l, 'cell>) -> bool {
        if let Some(v) = value.into_int() {
            v == 0
        } else if let Some(v) = value.into_float() {
            v.is_nan()
        } else if let Some(v) = value.into_string() {
            v.borrow(owner).is_empty()
        } else {
            value.is_null() || value.is_undefined() || value.is_false()
        }
    }

    pub fn is_nullish<'l>(self, value: Value<'l, 'cell>) -> bool {
        value.is_null() || value.is_undefined()
    }

    pub fn to_string<'l>(
        self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        value: Value<'_, 'cell>,
    ) -> Result<Gc<'l, 'cell, String>, Value<'l, 'cell>> {
        if let Some(value) = value.into_string() {
            Ok(rebind!(arena, value))
        } else if let Some(value) = value.into_int() {
            Ok(arena.add(value.to_string()))
        } else if let Some(value) = value.into_float() {
            Ok(arena.add(value.to_string()))
        } else if value.is_null() {
            Ok(arena.add("null".to_string()))
        } else if value.is_undefined() {
            Ok(arena.add("undefined".to_string()))
        } else if value.is_true() {
            Ok(arena.add("true".to_string()))
        } else if value.is_false() {
            Ok(arena.add("false".to_string()))
        } else if value.is_object() {
            let primitive = self.to_primitive(owner, arena, value, true)?;
            Ok(self.to_string(owner, arena, primitive)?)
        } else {
            todo!("toString: {:?}", value);
        }
    }

    pub fn to_object<'l>(
        self,
        value: Value<'l, 'cell>,
    ) -> Result<GcObject<'l, 'cell>, Value<'l, 'cell>> {
        if let Some(x) = value.into_object() {
            return Ok(x);
        }
        todo!("toObject: {:?}", value);
    }

    pub fn to_number<'l>(
        self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        value: Value<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        if value.is_int() || value.is_float() {
            Ok(rebind!(arena, value))
        } else if value.is_undefined() {
            Ok(Value::nan())
        } else if value.is_null() || value.is_false() {
            Ok(Value::from(0i32))
        } else if let Some(value) = value.into_string() {
            if let Ok(v) = value.borrow(owner).parse::<f64>() {
                Ok(Value::from(v))
            } else {
                Ok(Value::from(f64::NAN))
            }
        } else if value.is_object() {
            let prim = self.to_primitive(owner, arena, value, false)?;
            Ok(self.to_number(owner, arena, prim)?)
        } else {
            todo!("toNumber {:?}", value)
        }
    }

    /// Implements type conversion [`ToInt32`](https://tc39.es/ecma262/#sec-toint32)
    pub fn to_int32<'l>(
        &self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        value: Value<'_, 'cell>,
    ) -> Result<i32, Value<'l, 'cell>> {
        let number = self.to_number(owner, arena, value)?;
        Ok(if let Some(number) = number.into_int() {
            number
        } else if let Some(f) = number.into_float() {
            if f.is_normal() {
                let res = f.abs().floor().copysign(f) as i64 % (2 << 32);
                if res >= (2 << 31) {
                    (res - (2 << 32)) as i32
                } else {
                    res as i32
                }
            } else {
                0
            }
        } else {
            unreachable!()
        })
    }

    /// Implements type conversion [`ToUint32`](https://tc39.es/ecma262/#sec-touint32)
    pub fn to_uint32<'l>(
        &self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        value: Value<'_, 'cell>,
    ) -> Result<u32, Value<'l, 'cell>> {
        let number = self.to_number(owner, arena, value)?;
        Ok(if let Some(number) = number.into_int() {
            number as u32
        } else if let Some(f) = number.into_float() {
            if f.is_normal() {
                (f.abs().floor().copysign(f) as i64 % (2 << 32)) as u32
            } else {
                0
            }
        } else {
            unreachable!()
        })
    }

    pub fn to_primitive<'l>(
        self,
        _owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        value: Value<'_, 'cell>,
        _prefer_string: bool,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        if let Some(_obj) = value.into_object() {
            todo!("to primitive")
        }
        return Ok(rebind!(arena, value));
    }

    pub fn strict_equal(
        self,
        owner: &CellOwner<'cell>,
        left: Value<'_, 'cell>,
        right: Value<'_, 'cell>,
    ) -> bool {
        if !left.same_type(right) {
            #[cfg(debug_assertions)]
            if left.is_number() && right.is_number() {
                let left = if let Some(x) = left.into_float() {
                    x
                } else {
                    left.into_int().unwrap() as f64
                };
                let right = if let Some(x) = right.into_float() {
                    x
                } else {
                    right.into_int().unwrap() as f64
                };
                assert_ne!(left, right);
            }
            return false;
        }
        if left.is_undefined() || left.is_null() {
            return true;
        }
        if let Some(left) = left.into_int() {
            return left == right.into_int().unwrap();
        }
        if let Some(left) = left.into_bool() {
            return left == right.into_bool().unwrap();
        }
        if let Some(left) = left.into_string() {
            return left.borrow(owner) == right.into_string().unwrap().borrow(owner);
        }
        if let Some(left) = left.into_object() {
            let right = right.into_object().unwrap();
            let left = left.into_ptr();
            let right = right.into_ptr();
            // Work around a weird lifetime conflict.
            return left.as_ptr() as usize == right.as_ptr() as usize;
        }
        if let (Some(left), Some(right)) = (left.into_float(), right.into_float()) {
            return left == right;
        }
        todo!("strict_equal left: {:?}, right: {:?}", left, right);
    }

    pub fn equal<'l>(
        self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        left: Value<'_, 'cell>,
        right: Value<'_, 'cell>,
    ) -> Result<bool, Value<'l, 'cell>> {
        if left.same_type(right) {
            return Ok(self.strict_equal(owner, left, right));
        }

        if left.is_undefined() && right.is_null() {
            return Ok(true);
        }
        if right.is_undefined() && left.is_null() {
            return Ok(true);
        }
        if left.is_number() && right.is_string() {
            // Should not return error
            let right = self.to_number(owner, arena, right).unwrap();
            return self.equal(owner, arena, left, right);
        }
        if right.is_number() && left.is_string() {
            // Should not return error
            let left = self.to_number(owner, arena, left).unwrap();
            return self.equal(owner, arena, left, right);
        }
        if left.is_bool() {
            // Should not return error
            let left = self.to_number(owner, arena, left).unwrap();
            return self.equal(owner, arena, left, right);
        }
        if right.is_bool() {
            // Should not return error
            let right = self.to_number(owner, arena, right).unwrap();
            return self.equal(owner, arena, left, right);
        }
        if !left.is_object() && right.is_object() {
            let right = self.to_primitive(owner, arena, right, true)?;
            return self.equal(owner, arena, left, right);
        }
        if !right.is_object() && left.is_object() {
            let left = self.to_primitive(owner, arena, left, true)?;
            return self.equal(owner, arena, left, right);
        }
        Ok(false)
    }

    pub fn less_then<'l>(
        self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        left: Value<'_, 'cell>,
        right: Value<'_, 'cell>,
        swap: bool,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let (left, right) = if swap {
            let r = self.to_primitive(owner, arena, left, false)?;
            let l = self.to_primitive(owner, arena, right, false)?;
            (l, r)
        } else {
            (
                self.to_primitive(owner, arena, left, false)?,
                self.to_primitive(owner, arena, right, false)?,
            )
        };
        if left.is_string() || right.is_string() {
            let left = self.to_string(owner, arena, left)?;
            let right = self.to_string(owner, arena, right)?;
            if left
                .borrow(owner)
                .as_str()
                .starts_with(right.borrow(owner).as_str())
            {
                return Ok(false.into());
            }
            if right
                .borrow(owner)
                .as_str()
                .starts_with(left.borrow(owner).as_str())
            {
                return Ok(true.into());
            }
            let mut left = left.borrow(owner).as_str().chars();
            let mut right = right.borrow(owner).as_str().chars();
            loop {
                let left = left.next().unwrap();
                let right = right.next().unwrap();
                if left != right {
                    return Ok((left < right).into());
                }
            }
        }

        // Left and right are already primitives so they cannot return an error
        let left = self.to_number(owner, arena, left).unwrap();
        let right = self.to_number(owner, arena, right).unwrap();
        let left = if let Some(left) = left.into_float() {
            left
        } else {
            left.into_int().unwrap() as f64
        };
        let right = if let Some(right) = right.into_float() {
            right
        } else {
            right.into_int().unwrap() as f64
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

    pub fn add<'l>(
        self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        left: Value<'_, 'cell>,
        right: Value<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let left = self.to_primitive(owner, arena, left, true)?;
        let right = self.to_primitive(owner, arena, right, true)?;
        if left.is_string() || right.is_string() {
            let left = self.to_string(owner, arena, left)?;
            let right = self.to_string(owner, arena, right)?;
            let v = arena.add(left.borrow(owner).to_string() + right.borrow(owner));
            return Ok(v.into());
        }
        let left = self.to_number(owner, arena, left)?;
        let right = self.to_number(owner, arena, right)?;
        let left = if let Some(left) = left.into_int() {
            left as f64
        } else if let Some(left) = left.into_float() {
            left
        } else {
            panic!("to_number did not return a number");
        };
        let right = if let Some(right) = right.into_int() {
            right as f64
        } else if let Some(right) = right.into_float() {
            right
        } else {
            panic!("to_number did not return a number");
        };
        let res = left + right;
        if res as i32 as f64 == res {
            Ok(Value::from(res as i32))
        } else {
            Ok(Value::from(res))
        }
    }

    #[inline]
    pub fn numeric_operator<'l>(
        self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        left: Value<'_, 'cell>,
        right: Value<'_, 'cell>,
        op: NumericOperator,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let left = self.to_number(owner, arena, left)?;
        let right = self.to_number(owner, arena, right)?;
        let left = if let Some(left) = left.into_int() {
            left as f64
        } else if let Some(left) = left.into_float() {
            left
        } else {
            panic!("to_number did not return a number");
        };
        let right = if let Some(right) = right.into_int() {
            right as f64
        } else if let Some(right) = right.into_float() {
            right
        } else {
            panic!("to_number did not return a number");
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

    pub fn type_of<'l>(self, owner: &CellOwner<'cell>, v: Value<'l, 'cell>) -> &'l str {
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
        } else if let Some(obj) = v.into_object() {
            if obj.borrow(owner).is_function() {
                "function"
            } else {
                "object"
            }
        } else {
            unreachable!()
        }
    }

    pub fn instance_of<'l>(
        self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        left: Value<'gc, 'cell>,
        right: Value<'gc, 'cell>,
    ) -> Result<bool, Value<'l, 'cell>> {
        let right = right
            .into_object()
            .ok_or_else(|| self.create_type_error(arena, "invalid `instanceof` operand"))?;

        let _left = if let Some(x) = left.into_object() {
            x
        } else {
            return Ok(false);
        };

        // TODO implement @@hasInstance method
        if right.borrow(owner).is_function() {
            return Err(
                self.create_type_error(arena, "right hand instanceof operand is not a function")
            );
        }

        todo!()
        /*
        let tgt_proto = right.index(self, atom::constant::prototype)?;
        let tgt_proto = tgt_proto
            .into_object()
            .ok_or_else(|| self.create_type_error(arena, "object prototype is not an object"))?;

        let mut cur = left;
        while let Some(proto) = cur.prototype {
            if proto.ptr_eq(tgt_proto) {
                return Ok(true);
            }
            cur = proto;
        }
        Ok(false)
        */
    }

    #[inline]
    fn both_int(a: Value, b: Value) -> bool {
        a.is_int() && b.is_int()
    }

    #[inline]
    fn coerce_int(int: i64) -> Value<'static, 'cell> {
        if int as i32 as i64 == int {
            Value::from(int as i32)
        } else {
            Value::from(int as f64)
        }
    }

    #[allow(unused_unsafe)]
    pub unsafe fn construct_function<'l>(
        self,
        owner: &CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        function_id: u16,
        read: &InstructionReader<'_, 'cell>,
        function: GcObject<'_, 'cell>,
    ) -> VmFunction<'l, 'cell> {
        let bc = &read.bc.borrow(owner).functions[function_id as usize];
        let up_func = function.borrow(owner).as_vm_function();
        let upvalues = bc
            .upvalues
            .iter()
            .copied()
            .map(|x| match x {
                Upvalue::Local(register) => {
                    let up = self.borrow(owner).stack.create_upvalue(register);
                    arena.add(up)
                }
                Upvalue::Parent(slot) => {
                    rebind!(arena, up_func.upvalues[slot as usize])
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let bc = rebind!(arena, read.bc);

        VmFunction {
            bc,
            function: function_id,
            upvalues,
        }
    }

    pub unsafe fn method_call<'l, 'a>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        function: GcObject<'a, 'cell>,
        obj: GcObject<'a, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let ctx = ExecutionContext {
            function,
            this: obj.into(),
            new_target: Value::undefined(),
        };

        self.call(owner, arena, atoms, ctx)
    }

    #[allow(unused_unsafe)]
    pub unsafe fn call<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        ctx: ExecutionContext<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        match ctx.function.borrow(owner).kind() {
            ObjectKind::VmFn(func) => {
                let bc = func.bc;
                let frame_size = bc.borrow(owner).functions[func.function as usize].size;
                let mut instr = InstructionReader::new_unsafe(owner, bc, func.function);

                let guard = self.w(owner).stack.push_frame(frame_size);
                let res = self.run(arena, owner, atoms, &mut instr, &ctx);
                let res = rebind!(arena, res);
                self.w(owner).stack.pop_frame(arena, guard);
                res
            }
            ObjectKind::StaticFn(x) => {
                rebind!(arena, x(arena, owner, self, &ctx))
            }
            _ => todo!(),
        }
    }

    #[cold]
    pub fn create_type_error<'l>(
        self,
        _arena: &'l Arena<'_, 'cell>,
        _message: impl Into<String>,
    ) -> Value<'l, 'cell> {
        todo!("create type error")
    }

    #[cold]
    pub fn create_syntax_error<'l>(
        self,
        _arena: &'l Arena<'_, 'cell>,
        _message: impl Into<String>,
    ) -> Value<'l, 'cell> {
        todo!("create syntax error")
    }
}
