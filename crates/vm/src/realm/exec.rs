use common::atom::{self, Atoms};

use crate::{
    cell::CellOwner,
    gc::{self, Arena, Gc},
    instructions::{GcByteCode, Instruction, Upvalue},
    object::{ObjectFlags, ObjectKind, VmFunction},
    rebind, rebind_try, root, root_clone, GcObject, Object, Realm, Value,
};

use super::{builtin, stack::Stack, ExecutionContext, GcRealm};

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
    /// `bc` must contain valid bytecode.
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

impl<'gc, 'cell> GcRealm<'gc, 'cell> {
    // Shorthand for Gc::borrow
    unsafe fn r<'a>(self, owner: &'a CellOwner<'cell>) -> &'a Realm<'gc, 'cell> {
        gc::rebind(self.borrow(owner))
    }

    // Shorthand for Gc::unsafe_borrow_mut
    //
    // Should only be used as long as before each collection and return from the loop the a
    // write_barrier is done for the realm
    unsafe fn w<'a>(self, owner: &'a mut CellOwner<'cell>) -> &'a mut Realm<'gc, 'cell> {
        gc::rebind(self.unsafe_borrow_mut(owner))
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
                    let func = Object::new_gc(
                        arena,
                        Some(fp),
                        ObjectFlags::ORDINARY,
                        ObjectKind::VmFn(func),
                    );
                    self.w(owner).stack.write(dst, func.into());
                }

                Instruction::LoadConstructor { dst, func } => {
                    arena.write_barrier(self);
                    arena.collect(owner);

                    let fp = self.r(owner).builtin.function_proto;
                    let func = self.construct_function(owner, arena, func, &instr, ctx.function);
                    let func = Object::new_gc(
                        arena,
                        Some(fp),
                        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
                        ObjectKind::VmFn(func),
                    );
                    let op = self.r(owner).builtin.object_proto;
                    let proto = Object::new_gc(
                        arena,
                        Some(op),
                        ObjectFlags::ORDINARY,
                        ObjectKind::Ordinary,
                    );
                    func.raw_index_set(owner, arena, atoms, atom::constant::prototype, proto);
                    proto.raw_index_set(owner, arena, atoms, atom::constant::constructor, func);
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
                    let object = arena.add(Object::new(
                        Some(op),
                        ObjectFlags::ORDINARY,
                        ObjectKind::Ordinary,
                    ));
                    self.w(owner).stack.write(dst, object.into());
                }
                Instruction::CreateArray { dst } => {
                    // Always retrace the current realm to allow ring without a write barrier
                    // otherwise.
                    arena.write_barrier(self);
                    arena.collect(owner);
                    let object = arena.add(Object::new(
                        None,
                        ObjectFlags::ORDINARY,
                        ObjectKind::Ordinary,
                    ));
                    self.w(owner).stack.write(dst, object.into());
                }
                Instruction::Index { dst, obj, key } => {
                    let key = self.r(owner).stack.read(key);
                    let obj = self.r(owner).stack.read(obj);
                    if let Some(obj) = obj.into_object() {
                        let res =
                            rebind_try!(arena, obj.index_value(owner, arena, atoms, self, key));
                        self.w(owner).stack.write(dst, res.empty_to_undefined());
                    } else {
                        return Err(self.create_runtime_error(
                            owner,
                            arena,
                            atoms,
                            "todo index: ",
                            Some(obj),
                        ));
                    }
                }
                Instruction::IndexAssign { obj, key, src } => {
                    let src = self.r(owner).stack.read(src);
                    let key = self.r(owner).stack.read(key);
                    let obj = self.r(owner).stack.read(obj);
                    if let Some(obj) = obj.into_object() {
                        rebind_try!(
                            arena,
                            obj.index_set_value(owner, arena, atoms, self, key, src)
                        );
                    } else {
                        return Err(self.create_runtime_error(
                            owner,
                            arena,
                            atoms,
                            "todo index assign: ",
                            Some(obj),
                        ));
                    }
                }
                Instruction::GlobalIndex { dst, key } => {
                    let key = self.r(owner).stack.read(key);
                    let obj = self.r(owner).builtin.global;

                    let res = rebind_try!(arena, obj.index_value(owner, arena, atoms, self, key));
                    self.w(owner).stack.write(dst, res.empty_to_undefined());
                }
                Instruction::GlobalAssign { key, src } => {
                    let src = self.r(owner).stack.read(src);
                    let key = self.r(owner).stack.read(key);
                    let obj = self.r(owner).builtin.global;
                    rebind_try!(
                        arena,
                        obj.index_set_value(owner, arena, atoms, self, key, src)
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

                    let res =
                        rebind_try!(arena, self.instance_of(owner, arena, atoms, left, right));
                    self.w(owner).stack.write(dst, res.into());
                }

                Instruction::Add { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);

                    if let (Some(left), Some(right)) = (left.into_int(), right.into_int()) {
                        let res = Self::coerce_int(left as i64 + right as i64);
                        self.w(owner).stack.write(dst, res);
                    } else {
                        let res = rebind_try!(arena, self.add(owner, arena, atoms, left, right));
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
                            self.numeric_operator(
                                owner,
                                arena,
                                atoms,
                                left,
                                right,
                                NumericOperator::Sub
                            )
                        );
                        self.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::Mul { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.numeric_operator(
                            owner,
                            arena,
                            atoms,
                            left,
                            right,
                            NumericOperator::Mul
                        )
                    );
                    self.w(owner).stack.write(dst, res);
                }
                Instruction::Div { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.numeric_operator(
                            owner,
                            arena,
                            atoms,
                            left,
                            right,
                            NumericOperator::Div
                        )
                    );
                    self.w(owner).stack.write(dst, res);
                }
                Instruction::Mod { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.numeric_operator(
                            owner,
                            arena,
                            atoms,
                            left,
                            right,
                            NumericOperator::Mod
                        )
                    );
                    self.w(owner).stack.write(dst, res);
                }
                Instruction::Pow { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.numeric_operator(
                            owner,
                            arena,
                            atoms,
                            left,
                            right,
                            NumericOperator::Pow
                        )
                    );
                    self.w(owner).stack.write(dst, res);
                }
                Instruction::BitwiseAnd { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, atoms, left));
                    let right = rebind_try!(arena, self.to_int32(owner, arena, atoms, right));
                    let res = left & right;
                    self.w(owner).stack.write(dst, res.into());
                }
                Instruction::BitwiseOr { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, atoms, left));
                    let right = rebind_try!(arena, self.to_int32(owner, arena, atoms, right));
                    let res = left | right;
                    self.w(owner).stack.write(dst, res.into());
                }
                Instruction::BitwiseXor { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, atoms, left));
                    let right = rebind_try!(arena, self.to_int32(owner, arena, atoms, right));
                    let res = left ^ right;
                    self.w(owner).stack.write(dst, res.into());
                }
                Instruction::BitwiseNot { dst, src } => {
                    let src = self.r(owner).stack.read(src);
                    let src = rebind_try!(arena, self.to_int32(owner, arena, atoms, src));
                    self.w(owner).stack.write(dst, (!src).into());
                }
                Instruction::ShiftLeft { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, atoms, left));
                    let right =
                        rebind_try!(arena, self.to_int32(owner, arena, atoms, right)) as u32 % 32;
                    self.w(owner).stack.write(dst, Value::from(left << right));
                }
                Instruction::ShiftRight { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_int32(owner, arena, atoms, left));
                    let right =
                        rebind_try!(arena, self.to_int32(owner, arena, atoms, right)) as u32 % 32;
                    self.w(owner).stack.write(dst, Value::from(left >> right));
                }
                Instruction::ShiftUnsigned { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, self.to_uint32(owner, arena, atoms, left));
                    let right =
                        rebind_try!(arena, self.to_int32(owner, arena, atoms, right)) as u32 % 32;
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
                    let res = rebind_try!(arena, self.equal(owner, arena, atoms, left, right));
                    self.w(owner).stack.write(dst, Value::from(res));
                }
                Instruction::NotEqual { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = !rebind_try!(arena, self.equal(owner, arena, atoms, left, right));
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
                    let res = rebind_try!(
                        arena,
                        self.less_then(owner, arena, atoms, left, right, true)
                    );
                    if res.is_undefined() {
                        self.w(owner).stack.write(dst, false.into());
                    } else {
                        self.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::GreaterEq { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.less_then(owner, arena, atoms, left, right, false)
                    );
                    let res = res.is_false() && !res.is_undefined();
                    self.w(owner).stack.write(dst, res.into());
                }

                Instruction::Less { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.less_then(owner, arena, atoms, left, right, false)
                    );
                    if res.is_undefined() {
                        self.w(owner).stack.write(dst, false.into());
                    } else {
                        self.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::LessEq { dst, left, righ } => {
                    let left = self.r(owner).stack.read(left);
                    let right = self.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        self.less_then(owner, arena, atoms, left, right, true)
                    );
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
                        let number = rebind_try!(arena, self.to_number(owner, arena, atoms, src));
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
                    let number = rebind_try!(arena, self.to_number(owner, arena, atoms, src));
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

                Instruction::Push { src } => {
                    let src = self.r(owner).stack.read(src);
                    self.w(owner).stack.push(src);
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
                        return Err(self.create_type_error(
                            owner,
                            arena,
                            atoms,
                            "tried to call non function value",
                        ));
                    }
                }
                Instruction::CallMethod { dst, key, obj } => {
                    let key = self.r(owner).stack.read(key);
                    let obj = self.r(owner).stack.read(obj);

                    if let Some(obj) = obj.into_object() {
                        let func =
                            rebind_try!(arena, obj.index_value(owner, arena, atoms, self, key));
                        if let Some(function) = func.into_object() {
                            root!(arena, function);
                            let res = rebind_try!(
                                arena,
                                self.method_call(owner, arena, atoms, function, obj)
                            );
                            self.w(owner).stack.write(dst, res);
                        }
                    } else {
                        return Err(self.create_type_error(
                            owner,
                            arena,
                            atoms,
                            "tried to index non object value",
                        ));
                    }
                }
                Instruction::CallConstruct { dst, func, obj } => {
                    let func = self.r(owner).stack.read(func);
                    let obj = self.r(owner).stack.read(obj);

                    if let Some(func) = func.into_object() {
                        if let Some(obj) = obj.into_object() {
                            let res = rebind_try!(
                                arena,
                                self.construct_call(owner, arena, atoms, func, obj)
                            );
                            self.w(owner).stack.write(dst, res);
                        }
                    } else {
                        return Err(self.create_type_error(
                            owner,
                            arena,
                            atoms,
                            "tried to call non function value",
                        ));
                    }
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
                _ => {
                    return Err(self.create_type_error(
                        owner,
                        arena,
                        atoms,
                        "runtime error: invalid instruction",
                    ))
                }
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
            value.is_null() || value.is_undefined() || value.is_false() || value.is_empty()
        }
    }

    pub fn is_nullish<'l>(self, value: Value<'l, 'cell>) -> bool {
        value.is_null() || value.is_undefined()
    }

    pub fn to_string<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        value: Value<'_, 'cell>,
    ) -> Result<Gc<'l, 'cell, String>, Value<'l, 'cell>> {
        let primitive = rebind_try!(arena, self.to_primitive(owner, arena, atoms, value, true));
        let primitive = rebind!(arena, primitive);
        let res = self
            .to_string_primitive(arena, atoms, primitive)
            .expect("to_primitive returned an object");

        Ok(rebind!(arena, res))
    }

    pub fn to_string_primitive<'l>(
        self,
        arena: &'l Arena<'_, 'cell>,
        _atoms: &Atoms,
        value: Value<'_, 'cell>,
    ) -> Option<Gc<'l, 'cell, String>> {
        if let Some(value) = value.into_string() {
            Some(rebind!(arena, value))
        } else if let Some(value) = value.into_int() {
            Some(arena.add(value.to_string()))
        } else if let Some(value) = value.into_float() {
            Some(arena.add(value.to_string()))
        } else if value.is_null() {
            Some(arena.add("null".to_string()))
        } else if value.is_undefined() {
            Some(arena.add("undefined".to_string()))
        } else if value.is_true() {
            Some(arena.add("true".to_string()))
        } else if value.is_false() {
            Some(arena.add("false".to_string()))
        } else if value.is_object() {
            None
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
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        value: Value<'_, 'cell>,
    ) -> Result<Value<'static, 'cell>, Value<'l, 'cell>> {
        let prim = rebind_try!(arena, self.to_primitive(owner, arena, atoms, value, false));
        let res = self
            .to_number_primitive(owner, prim)
            .expect("to primitive returned an object");
        Ok(res)
    }

    pub fn to_number_primitive(
        self,
        owner: &CellOwner<'cell>,
        value: Value<'_, 'cell>,
    ) -> Option<Value<'static, 'cell>> {
        if let Some(x) = value.into_int() {
            Some(x.into())
        } else if let Some(x) = value.into_float() {
            Some(Value::ensure_float(x))
        } else if value.is_undefined() {
            Some(Value::nan())
        } else if value.is_null() || value.is_false() {
            Some(Value::from(0i32))
        } else if let Some(value) = value.into_string() {
            if let Ok(v) = value.borrow(owner).parse::<f64>() {
                Some(Value::from(v))
            } else {
                Some(Value::nan())
            }
        } else {
            None
        }
    }

    /// Implements type conversion [`ToInt32`](https://tc39.es/ecma262/#sec-toint32)
    pub fn to_int32<'l>(
        &self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        value: Value<'_, 'cell>,
    ) -> Result<i32, Value<'l, 'cell>> {
        let number = self.to_number(owner, arena, atoms, value)?;
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
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        value: Value<'_, 'cell>,
    ) -> Result<u32, Value<'l, 'cell>> {
        let number = self.to_number(owner, arena, atoms, value)?;
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
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        value: Value<'_, 'cell>,
        prefer_string: bool,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        const PREFER_STRING: &'static [atom::Atom] =
            &[atom::constant::toString, atom::constant::valueOf];
        const PREFER_VALUE: &'static [atom::Atom] =
            &[atom::constant::valueOf, atom::constant::toString];

        if let Some(obj) = value.into_object() {
            // Again not sure why this needs to be rooted but it doesn not work otherwise.
            root!(arena, obj);
            let keys = if prefer_string {
                PREFER_STRING
            } else {
                PREFER_VALUE
            };
            for &k in keys {
                let func = rebind_try!(arena, obj.index(owner, arena, atoms, self, k))
                    .empty_to_undefined();
                root_clone!(arena, func);

                #[allow(unused_unsafe)]
                if let Some(func) = func.into_object() {
                    if func.borrow(owner).is_function() {
                        let res =
                            rebind_try!(arena, self.method_call(owner, arena, atoms, func, obj));
                        if !res.is_object() {
                            return Ok(rebind!(arena, res));
                        }
                    }
                }
            }

            return Err(self.create_type_error(
                owner,
                arena,
                atoms,
                "Could not create a primitive from object",
            ));
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
            // Workaround for a weird lifetime conflict.
            return left.as_ptr() as usize == right.as_ptr() as usize;
        }
        if let (Some(left), Some(right)) = (left.into_float(), right.into_float()) {
            return left == right;
        }
        todo!("strict_equal left: {:?}, right: {:?}", left, right);
    }

    pub fn equal<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
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
            // Should not return error and always a number
            let right = unsafe { gc::rebind(self.to_number(owner, arena, atoms, right).unwrap()) };
            debug_assert!(right.is_number());
            return self.equal(owner, arena, atoms, left, right);
        }
        if right.is_number() && left.is_string() {
            // Should not return error and always a number
            let left = unsafe { gc::rebind(self.to_number(owner, arena, atoms, left).unwrap()) };
            debug_assert!(left.is_number());
            return self.equal(owner, arena, atoms, left, right);
        }
        if left.is_bool() {
            // Should not return error and always a number
            let left = unsafe { gc::rebind(self.to_number(owner, arena, atoms, left).unwrap()) };
            debug_assert!(left.is_number());
            return self.equal(owner, arena, atoms, left, right);
        }
        if right.is_bool() {
            // Should not return error and always a number
            let right = unsafe { gc::rebind(self.to_number(owner, arena, atoms, right).unwrap()) };
            debug_assert!(right.is_number());
            return self.equal(owner, arena, atoms, left, right);
        }
        if !left.is_object() && right.is_object() {
            let right = rebind_try!(arena, self.to_primitive(owner, arena, atoms, right, true));
            root_clone!(arena, right);
            return self.equal(owner, arena, atoms, left, right);
        }
        if !right.is_object() && left.is_object() {
            let left = rebind_try!(arena, self.to_primitive(owner, arena, atoms, left, true));
            root_clone!(arena, left);
            return self.equal(owner, arena, atoms, left, right);
        }
        Ok(false)
    }

    pub fn less_then<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        left: Value<'_, 'cell>,
        right: Value<'_, 'cell>,
        swap: bool,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let (left, right) = if swap {
            let r = rebind_try!(arena, self.to_primitive(owner, arena, atoms, left, false));
            root_clone!(arena, r);
            let l = rebind_try!(arena, self.to_primitive(owner, arena, atoms, right, false));
            (rebind!(arena, l), rebind!(arena, r))
        } else {
            let l = rebind_try!(arena, self.to_primitive(owner, arena, atoms, left, false));
            root_clone!(arena, l);
            let r = rebind_try!(arena, self.to_primitive(owner, arena, atoms, right, false));
            let r = rebind!(arena, r);
            (rebind!(arena, l), r)
        };
        if left.is_string() || right.is_string() {
            let left = self.to_string_primitive(arena, atoms, left).unwrap();
            let right = self.to_string_primitive(arena, atoms, right).unwrap();

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
        let left = self.to_number_primitive(owner, left).unwrap();
        let right = self.to_number_primitive(owner, right).unwrap();
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
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        left: Value<'_, 'cell>,
        right: Value<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let (left, right) = {
            let l = rebind_try!(arena, self.to_primitive(owner, arena, atoms, left, true));
            root_clone!(arena, l);
            let r = rebind_try!(arena, self.to_primitive(owner, arena, atoms, right, true));
            let r = rebind!(arena, r);
            let l = rebind!(arena, l);
            (l, r)
        };

        if left.is_string() || right.is_string() {
            let left = self.to_string_primitive(arena, atoms, left).unwrap();
            let right = self.to_string_primitive(arena, atoms, right).unwrap();
            let v = arena.add(left.borrow(owner).to_string() + right.borrow(owner));
            return Ok(v.into());
        }
        let left = self.to_number_primitive(owner, left).unwrap();
        let right = self.to_number_primitive(owner, right).unwrap();
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
        Ok((left + right).into())
    }

    #[inline]
    pub fn numeric_operator<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        left: Value<'_, 'cell>,
        right: Value<'_, 'cell>,
        op: NumericOperator,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let l = rebind_try!(arena, self.to_number(owner, arena, atoms, left));
        debug_assert!(l.is_number());
        let left = unsafe { gc::rebind(l) };
        let r = rebind_try!(arena, self.to_number(owner, arena, atoms, right));
        debug_assert!(right.is_number());
        let right = unsafe { gc::rebind(r) };
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
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        left: Value<'_, 'cell>,
        right: Value<'_, 'cell>,
    ) -> Result<bool, Value<'l, 'cell>> {
        let right = rebind_try!(
            arena,
            right.into_object().ok_or_else(|| self.create_type_error(
                owner,
                arena,
                atoms,
                "invalid `instanceof` operand"
            ))
        );

        let left = if let Some(x) = left.into_object() {
            x
        } else {
            return Ok(false);
        };

        // TODO implement @@hasInstance method
        if right.borrow(owner).is_function() {
            return Err(self.create_type_error(
                owner,
                arena,
                atoms,
                "right hand instanceof operand is not a function",
            ));
        }

        let tgt_proto = right.index(owner, arena, atoms, self, atom::constant::prototype);
        let tgt_proto = rebind_try!(arena, tgt_proto).empty_to_undefined();
        let tgt_proto = rebind!(arena, tgt_proto);
        let tgt_proto = tgt_proto.into_object().ok_or_else(|| {
            self.create_type_error(owner, arena, atoms, "object prototype is not an object")
        });

        let tgt_proto = rebind_try!(arena, tgt_proto);

        let mut cur = rebind!(arena, left);
        while let Some(proto) = cur.borrow(owner).prototype() {
            if proto.ptr_eq(tgt_proto) {
                return Ok(true);
            }
            cur = proto;
        }
        Ok(false)
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
        owner: &mut CellOwner<'cell>,
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
                Upvalue::Local(register) => (*self.get()).stack.create_upvalue(arena, register),
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

    pub fn method_call<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        function: GcObject<'_, 'cell>,
        obj: GcObject<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        // Life times function and object are independent of arena,
        // as such they have to be rooted so they are valid for the entire function and can be
        // rebind to the lifetime of the other

        let ctx = unsafe {
            ExecutionContext {
                function: gc::rebind(function),
                this: gc::rebind(obj).into(),
                new_target: Value::undefined(),
            }
        };

        unsafe { self.call(owner, arena, atoms, ctx) }
    }

    pub fn construct_call<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        function: GcObject<'_, 'cell>,
        target_obj: GcObject<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let proto = rebind!(
            arena,
            rebind_try!(
                arena,
                function.index(owner, arena, atoms, self, atom::constant::prototype)
            )
        )
        .into_object()
        .unwrap_or_else(|| rebind!(arena, unsafe { self.r(owner).builtin.object_proto }));
        let this = Object::new(Some(proto), ObjectFlags::ORDINARY, ObjectKind::Ordinary);
        let this = arena.add(this);
        root!(arena, this);

        // Life times function and target_obj are independent of arena,
        // as such they have to be rooted so they are valid for the entire function and can be
        // rebind to the lifetime of the other

        let ctx = unsafe {
            let new_target = gc::rebind(target_obj).into();
            let function = gc::rebind(function);

            ExecutionContext::<'_, 'cell> {
                this: this.into(),
                function,
                new_target,
            }
        };

        let res = rebind_try!(arena, unsafe { self.call(owner, arena, atoms, ctx) });
        let res = rebind!(arena, res);
        if let Some(obj) = res.into_object() {
            Ok(obj.into())
        } else {
            Ok(rebind!(arena, this).into())
        }
    }

    #[allow(unused_unsafe)]
    pub unsafe fn call<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        mut ctx: ExecutionContext<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        match ctx.function.borrow(owner).kind() {
            ObjectKind::VmFn(func) => {
                let bc = func.bc;
                let frame_size =
                    bc.borrow(owner).functions[func.function as usize].registers as u32;
                let mut instr = InstructionReader::new_unsafe(owner, bc, func.function);

                let guard = rebind_try!(
                    arena,
                    self.w(owner).stack.push_frame(frame_size).ok_or_else(|| {
                        self.create_runtime_error(
                            owner,
                            arena,
                            atoms,
                            Stack::DEPTH_EXCEEDED_MSG,
                            None,
                        )
                    })
                );
                let res = self.run(arena, owner, atoms, &mut instr, &mut ctx);
                let res = rebind!(arena, res);
                self.w(owner).stack.pop_frame(arena, guard);
                res
            }
            ObjectKind::StaticFn(x) => {
                let x = *x;
                let guard = rebind_try!(
                    arena,
                    self.w(owner).stack.push_frame(0).ok_or_else(|| {
                        self.create_runtime_error(
                            owner,
                            arena,
                            atoms,
                            Stack::DEPTH_EXCEEDED_MSG,
                            None,
                        )
                    })
                );
                let res = rebind!(arena, x(arena, owner, atoms, self, &ctx));
                self.w(owner).stack.pop_frame(arena, guard);
                res
            }
            _ => todo!(),
        }
    }

    #[cold]
    pub fn create_type_error<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        atoms: &Atoms,
        message: impl Into<String>,
    ) -> Value<'l, 'cell> {
        let message = arena.add(message.into());
        builtin::error::create(
            arena,
            owner,
            atoms,
            self.borrow(owner).builtin.type_error_proto,
            Some(message),
            None,
        )
        .into()
    }

    #[cold]
    pub fn create_syntax_error<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        atoms: &Atoms,
        message: impl Into<String>,
    ) -> Value<'l, 'cell> {
        let message = arena.add(message.into());
        builtin::error::create(
            arena,
            owner,
            atoms,
            self.borrow(owner).builtin.syntax_error_proto,
            Some(message),
            None,
        )
        .into()
    }

    #[cold]
    pub fn create_runtime_error<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l Arena<'_, 'cell>,
        atoms: &Atoms,
        message: impl Into<String>,
        context: Option<Value<'_, 'cell>>,
    ) -> Value<'l, 'cell> {
        let ctx = if let Some(x) = context {
            format!("{:?}", x)
        } else {
            String::new()
        };
        let message = arena.add(message.into() + &ctx);
        builtin::error::create(
            arena,
            owner,
            atoms,
            self.borrow(owner).builtin.runtime_error_proto,
            Some(message),
            None,
        )
        .into()
    }
}
