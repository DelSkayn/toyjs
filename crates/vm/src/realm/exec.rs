use common::atom::{self, Atoms};
use dreck::{Gc, Owner, Root, rebind, root};

use crate::{
    instructions::{GcByteCode, Instruction, Upvalue},
    object::{FunctionKind, ObjectFlags, ObjectKind, VmFunction},
    GcObject, Object, Realm, Value,
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
pub struct InstructionReader<'gc, 'own> {
    bc: GcByteCode<'gc, 'own>,
    cur: *const Instruction,
    #[cfg(debug_assertions)]
    first: *const Instruction,
    #[cfg(debug_assertions)]
    last: *const Instruction,
}

impl<'gc, 'own> InstructionReader<'gc, 'own> {
    /// # Safety
    ///
    /// `func` must be a smaller the amount of function present in the bytecode.
    /// `bc` must contain valid bytecode.
    ///
    pub unsafe fn new_unsafe(
        owner: &Owner<'own>,
        bc: GcByteCode<'gc, 'own>,
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

    pub fn bc(&self) -> GcByteCode<'gc, 'own> {
        self.bc
    }

    /// # Safety
    ///
    /// User must ensure that the reader does not read past the end of the instruction buffer
    pub unsafe fn next(&mut self, _owner: &Owner<'own>) -> Instruction {
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
    pub unsafe fn constant(&self, idx: u16, owner: &Owner<'own>) -> Value<'gc, 'own> {
        debug_assert!(self.bc.borrow(owner).constants.len() > idx as usize);
        *self.bc.borrow(owner).constants.get_unchecked(idx as usize)
    }
}

impl<'gc, 'own> Realm<'gc, 'own> {
    // Shorthand for Gc::borrow
    unsafe fn r<'a>(this: GcRealm<'gc,'own>, owner: &'a Owner<'own>) -> &'a Realm<'gc, 'own> {
        dreck::rebind(this.borrow(owner))
    }

    // Shorthand for Gc::unsafe_borrow_mut
    //
    // Should only be used as long as before each collection and return from the loop the a
    // write_barrier is done for the realm
    unsafe fn w<'a>(this: GcRealm<'gc,'own>, owner: &'a mut Owner<'own>) -> &'a mut Realm<'gc, 'own> {
        dreck::rebind(this.unsafe_borrow_mut(owner))
    }

    /// # Safety
    ///
    /// Instruction reader must read valid bytecode.
    #[allow(unused_unsafe)]
    pub unsafe fn run<'l>(
        this: GcRealm<'gc,'own>,
        arena: &'l mut Root< 'own>,
        owner: &mut Owner<'own>,
        atoms: &Atoms,
        upper_instr: &mut InstructionReader<'_, 'own>,
        ctx: &ExecutionContext<'_, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let mut instr = *upper_instr;
        loop {
            match instr.next(owner) {
                Instruction::LoadConst { dst, cons } => {
                    let con = instr.constant(cons, owner);
                    this.w(owner).stack.write(dst, con);
                }
                Instruction::LoadGlobal { dst } => {
                    let global = this.r(owner).builtin.global;
                    this.w(owner).stack.write(dst, global.into());
                }
                Instruction::LoadFunction { dst, func } => {
                    arena.write_barrier(this);
                    arena.collect(owner, atoms);

                    let fp = this.r(owner).builtin.function_proto;
                    let func = this.construct_function(owner, arena, func, &instr, ctx.function);
                    let func = Object::new_gc(
                        arena,
                        Some(fp),
                        ObjectFlags::ORDINARY,
                        ObjectKind::VmFn(func),
                    );
                    Self::w(this,owner).stack.write(dst, func.into());
                }

                Instruction::LoadConstructor { dst, func } => {
                    arena.write_barrier(this);
                    arena.collect(owner, atoms);

                    let fp = this.r(owner).builtin.function_proto;
                    let func = this.construct_function(owner, arena, func, &instr, ctx.function);
                    let func = Object::new_gc(
                        arena,
                        Some(fp),
                        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
                        ObjectKind::VmFn(func),
                    );
                    let op = this.r(owner).builtin.object_proto;
                    let proto = Object::new_gc(
                        arena,
                        Some(op),
                        ObjectFlags::ORDINARY,
                        ObjectKind::Ordinary,
                    );
                    func.raw_index_set(owner, arena, atoms, atom::constant::prototype, proto);
                    proto.raw_index_set(owner, arena, atoms, atom::constant::constructor, func);
                    this.w(owner).stack.write(dst, func.into());
                }

                Instruction::LoadThis { dst } => {
                    this.w(owner).stack.write(dst, ctx.this);
                }
                Instruction::LoadTarget { dst } => {
                    this.w(owner).stack.write(dst, ctx.new_target);
                }
                Instruction::Move { dst, src } => {
                    let src = this.r(owner).stack.read(src);
                    this.w(owner).stack.write(dst, src);
                }
                Instruction::CreateObject { dst } => {
                    // Always retrace the current realm to allow ring without a write barrier
                    // otherwise.
                    arena.write_barrier(this);
                    arena.collect(owner, atoms);
                    let op = this.r(owner).builtin.object_proto;
                    let object = arena.add(Object::new(
                        Some(op),
                        ObjectFlags::ORDINARY,
                        ObjectKind::Ordinary,
                    ));
                    this.w(owner).stack.write(dst, object.into());
                }
                Instruction::CreateArray { dst } => {
                    // Always retrace the current realm to allow ring without a write barrier
                    // otherwise.
                    arena.write_barrier(this);
                    arena.collect(owner, atoms);

                    let proto = this.borrow(owner).builtin.array_proto;
                    let res = Object::new_gc(
                        arena,
                        Some(proto),
                        ObjectFlags::ORDINARY,
                        ObjectKind::Array,
                    );
                    let fp = this.borrow(owner).builtin.function_proto;
                    builtin::array::define_length(owner, arena, atoms, fp, res);

                    this.w(owner).stack.write(dst, res.into());
                }
                Instruction::Index { dst, obj, key } => {
                    let key = this.r(owner).stack.read(key);
                    let obj = this.r(owner).stack.read(obj);
                    if let Some(obj) = obj.into_object() {
                        let res =
                            rebind_try!(arena, obj.index_value(owner, arena, atoms, this. key));
                        this.w(owner).stack.write(dst, res.empty_to_undefined());
                    } else {
                        let res = rebind_try!(
                            arena,
                            this.index_non_object(owner, arena, atoms, obj, key)
                        );
                        this.w(owner).stack.write(dst, res.empty_to_undefined());
                    }
                }
                Instruction::IndexAssign { obj, key, src } => {
                    let src = this.r(owner).stack.read(src);
                    let key = this.r(owner).stack.read(key);
                    let obj = this.r(owner).stack.read(obj);
                    if let Some(obj) = obj.into_object() {
                        rebind_try!(
                            arena,
                            obj.index_set_value(owner, arena, atoms, this. key, src)
                        );
                    } else if obj.is_undefined() {
                        return Err(this.create_type_error(
                            owner,
                            arena,
                            atoms,
                            "undefined has no properties",
                        ));
                    } else if obj.is_null() {
                        return Err(this.create_type_error(
                            owner,
                            arena,
                            atoms,
                            "null has no properties",
                        ));
                    } else {
                    }
                }
                Instruction::GlobalIndex { dst, key } => {
                    let key = this.r(owner).stack.read(key);
                    let obj = this.r(owner).builtin.global;

                    let res = rebind_try!(arena, obj.index_value(owner, arena, atoms, this. key));
                    this.w(owner).stack.write(dst, res.empty_to_undefined());
                }
                Instruction::GlobalAssign { key, src } => {
                    let src = this.r(owner).stack.read(src);
                    let key = this.r(owner).stack.read(key);
                    let obj = this.r(owner).builtin.global;
                    rebind_try!(
                        arena,
                        obj.index_set_value(owner, arena, atoms, this. key, src)
                    );
                }
                Instruction::Upvalue { dst, slot } => {
                    let upvalue =
                        ctx.function.borrow(owner).as_vm_function().upvalues[slot as usize];
                    let res = upvalue.borrow(owner).read();
                    this.w(owner).stack.write(dst, res);
                }

                Instruction::UpvalueAssign { src, slot } => {
                    let src = this.r(owner).stack.read(src);
                    let upvalue =
                        ctx.function.borrow(owner).as_vm_function().upvalues[slot as usize];
                    upvalue.borrow_mut(owner, arena).write(src);
                }

                Instruction::TypeOf { dst, src } => {
                    let src = this.r(owner).stack.read(src);
                    let res = this.type_of(owner, src);
                    let res = arena.add(String::from(res));
                    this.w(owner).stack.write(dst, res.into());
                }

                Instruction::InstanceOf { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);

                    let res =
                        rebind_try!(arena, this.instance_of(owner, arena, atoms, left, right));
                    this.w(owner).stack.write(dst, res.into());
                }

                Instruction::Add { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);

                    if let (Some(left), Some(right)) = (left.into_int(), right.into_int()) {
                        let res = Self::coerce_int(this,left as i64 + right as i64);
                        this.w(owner).stack.write(dst, res);
                    } else {
                        let res = rebind_try!(arena, this.add(owner, arena, atoms, left, right));
                        this.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::Sub { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    if let (Some(left), Some(right)) = (left.into_int(), right.into_int()) {
                        let res = Self::coerce_int(this,left as i64 - right as i64);
                        this.w(owner).stack.write(dst, res);
                    } else {
                        let res = rebind_try!(
                            arena,
                            this.numeric_operator(
                                owner,
                                arena,
                                atoms,
                                left,
                                right,
                                NumericOperator::Sub
                            )
                        );
                        this.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::Mul { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        this.numeric_operator(
                            owner,
                            arena,
                            atoms,
                            left,
                            right,
                            NumericOperator::Mul
                        )
                    );
                    this.w(owner).stack.write(dst, res);
                }
                Instruction::Div { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        this.numeric_operator(
                            owner,
                            arena,
                            atoms,
                            left,
                            right,
                            NumericOperator::Div
                        )
                    );
                    this.w(owner).stack.write(dst, res);
                }
                Instruction::Mod { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        this.numeric_operator(
                            owner,
                            arena,
                            atoms,
                            left,
                            right,
                            NumericOperator::Mod
                        )
                    );
                    this.w(owner).stack.write(dst, res);
                }
                Instruction::Pow { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        this.numeric_operator(
                            owner,
                            arena,
                            atoms,
                            left,
                            right,
                            NumericOperator::Pow
                        )
                    );
                    this.w(owner).stack.write(dst, res);
                }
                Instruction::BitwiseAnd { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, this.to_int32(owner, arena, atoms, left));
                    let right = rebind_try!(arena, this.to_int32(owner, arena, atoms, right));
                    let res = left & right;
                    this.w(owner).stack.write(dst, res.into());
                }
                Instruction::BitwiseOr { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, this.to_int32(owner, arena, atoms, left));
                    let right = rebind_try!(arena, this.to_int32(owner, arena, atoms, right));
                    let res = left | right;
                    this.w(owner).stack.write(dst, res.into());
                }
                Instruction::BitwiseXor { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, this.to_int32(owner, arena, atoms, left));
                    let right = rebind_try!(arena, this.to_int32(owner, arena, atoms, right));
                    let res = left ^ right;
                    this.w(owner).stack.write(dst, res.into());
                }
                Instruction::BitwiseNot { dst, src } => {
                    let src = this.r(owner).stack.read(src);
                    let src = rebind_try!(arena, this.to_int32(owner, arena, atoms, src));
                    this.w(owner).stack.write(dst, (!src).into());
                }
                Instruction::ShiftLeft { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, this.to_int32(owner, arena, atoms, left));
                    let right =
                        rebind_try!(arena, this.to_int32(owner, arena, atoms, right)) as u32 % 32;
                    this.w(owner).stack.write(dst, Value::from(left << right));
                }
                Instruction::ShiftRight { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, this.to_int32(owner, arena, atoms, left));
                    let right =
                        rebind_try!(arena, this.to_int32(owner, arena, atoms, right)) as u32 % 32;
                    this.w(owner).stack.write(dst, Value::from(left >> right));
                }
                Instruction::ShiftUnsigned { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let left = rebind_try!(arena, this.to_uint32(owner, arena, atoms, left));
                    let right =
                        rebind_try!(arena, this.to_int32(owner, arena, atoms, right)) as u32 % 32;
                    let v = left >> right;
                    if v > i32::MAX as u32 {
                        this.w(owner).stack.write(dst, Value::from(v as f64));
                    } else {
                        this.w(owner).stack.write(dst, Value::from(v as i32));
                    }
                }
                Instruction::Equal { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = rebind_try!(arena, this.equal(owner, arena, atoms, left, right));
                    this.w(owner).stack.write(dst, Value::from(res));
                }
                Instruction::NotEqual { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = !rebind_try!(arena, this.equal(owner, arena, atoms, left, right));
                    this.w(owner).stack.write(dst, Value::from(res));
                }

                Instruction::SEqual { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = this.strict_equal(owner, left, right);
                    this.w(owner).stack.write(dst, Value::from(res));
                }
                Instruction::SNotEqual { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = !this.strict_equal(owner, left, right);
                    this.w(owner).stack.write(dst, Value::from(res));
                }

                Instruction::Greater { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        this.less_then(owner, arena, atoms, left, right, true)
                    );
                    if res.is_undefined() {
                        this.w(owner).stack.write(dst, false.into());
                    } else {
                        this.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::GreaterEq { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        this.less_then(owner, arena, atoms, left, right, false)
                    );
                    let res = res.is_false() && !res.is_undefined();
                    this.w(owner).stack.write(dst, res.into());
                }

                Instruction::Less { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        this.less_then(owner, arena, atoms, left, right, false)
                    );
                    if res.is_undefined() {
                        this.w(owner).stack.write(dst, false.into());
                    } else {
                        this.w(owner).stack.write(dst, res);
                    }
                }
                Instruction::LessEq { dst, left, righ } => {
                    let left = this.r(owner).stack.read(left);
                    let right = this.r(owner).stack.read(righ);
                    let res = rebind_try!(
                        arena,
                        this.less_then(owner, arena, atoms, left, right, true)
                    );
                    let res = res.is_false() && !res.is_undefined();
                    this.w(owner).stack.write(dst, res.into());
                }

                Instruction::IsNullish { dst, op } => {
                    let src = this.r(owner).stack.read(op);
                    let nullish = this.is_nullish(src);
                    this.w(owner).stack.write(dst, Value::from(nullish));
                }
                Instruction::Not { dst, src } => {
                    let src = this.r(owner).stack.read(src);
                    let falsish = this.is_falsish(owner, src);
                    this.w(owner).stack.write(dst, Value::from(falsish));
                }
                Instruction::Negative { dst, op } => {
                    let src = this.r(owner).stack.read(op);
                    {
                        let number = rebind_try!(arena, this.to_number(owner, arena, atoms, src));
                        if let Some(number) = number.into_int() {
                            let number = -(number as i64);
                            if number as i32 as i64 == number {
                                this.w(owner).stack.write(dst, Value::from(number as i32))
                            } else {
                                this.w(owner).stack.write(dst, Value::from(number as f64))
                            }
                        } else if let Some(number) = number.into_float() {
                            let number = -number;
                            if number as i32 as f64 == number {
                                this.w(owner).stack.write(dst, Value::from(number as i32));
                            } else {
                                this.w(owner).stack.write(dst, Value::from(number));
                            }
                        } else {
                            unreachable!()
                        }
                    }
                }
                Instruction::Positive { dst, op } => {
                    let src = this.r(owner).stack.read(op);
                    let number = rebind_try!(arena, this.to_number(owner, arena, atoms, src));
                    this.w(owner).stack.write(dst, number);
                }

                Instruction::Jump { tgt } => instr.jump(tgt),
                Instruction::JumpFalse { cond, tgt } => {
                    let cond = this.r(owner).stack.read(cond);
                    if this.is_falsish(owner, cond) {
                        instr.jump(tgt)
                    }
                }
                Instruction::JumpTrue { cond, tgt } => {
                    let cond = this.r(owner).stack.read(cond);
                    if !this.is_falsish(owner, cond) {
                        instr.jump(tgt)
                    }
                }

                Instruction::Try { dst, tgt } => {
                    match this.run(arena, owner, atoms, &mut instr, ctx) {
                        Ok(_) => {
                            this.w(owner).stack.write(dst, Value::empty());
                        }
                        Err(e) => {
                            this.w(owner).stack.write(dst, e);
                            instr.jump(tgt);
                        }
                    }
                }

                Instruction::Untry { dst: _ } => {
                    *upper_instr = instr;
                    return Ok(Value::empty());
                }

                Instruction::Throw { src } => {
                    let src = this.r(owner).stack.read(src);
                    if !src.is_empty() {
                        return Err(rebind!(arena, src));
                    }
                }

                Instruction::Push { src } => {
                    let src = this.r(owner).stack.read(src);
                    this.w(owner).stack.push(src);
                }

                Instruction::Call { dst, func } => {
                    let func = this.r(owner).stack.read(func);
                    if let Some(obj) = func.into_object() {
                        let ctx = ExecutionContext {
                            function: obj,
                            this: obj.into(),
                            new_target: Value::undefined(),
                        };
                        let res = rebind_try!(arena, this.vm_call(owner, arena, atoms, ctx));
                        this.w(owner).stack.write(dst, res);
                    } else {
                        return Err(this.create_type_error(
                            owner,
                            arena,
                            atoms,
                            "tried to call non function value",
                        ));
                    }
                }
                Instruction::CallMethod { dst, key, obj } => {
                    let key = this.r(owner).stack.read(key);
                    let obj = this.r(owner).stack.read(obj);

                    if let Some(obj) = obj.into_object() {
                        let func =
                            rebind_try!(arena, obj.index_value(owner, arena, atoms, this. key));
                        if let Some(function) = func.into_object() {
                            root!(arena, function);
                            let res = rebind_try!(
                                arena,
                                this.method_call(owner, arena, atoms, function, obj.into())
                            );
                            this.w(owner).stack.write(dst, res);
                        } else {
                            return Err(this.create_type_error(
                                owner,
                                arena,
                                atoms,
                                "tried to call non function value",
                            ));
                        }
                    } else {
                        let func = rebind_try!(
                            arena,
                            this.index_non_object(owner, arena, atoms, obj, key)
                        );
                        if let Some(function) = func.into_object() {
                            root!(arena, function);
                            let res = rebind_try!(
                                arena,
                                this.method_call(owner, arena, atoms, function, obj)
                            );
                            this.w(owner).stack.write(dst, res);
                        } else {
                            return Err(this.create_type_error(
                                owner,
                                arena,
                                atoms,
                                "tried to call non function value",
                            ));
                        }
                    }
                }
                Instruction::CallConstruct { dst, func, obj } => {
                    let func = this.r(owner).stack.read(func);
                    let obj = this.r(owner).stack.read(obj);

                    if let Some(func) = func.into_object() {
                        if let Some(obj) = obj.into_object() {
                            let res = rebind_try!(
                                arena,
                                this.construct_call(owner, arena, atoms, func, obj)
                            );
                            this.w(owner).stack.write(dst, res);
                        }
                    } else {
                        return Err(this.create_type_error(
                            owner,
                            arena,
                            atoms,
                            "tried to call non function value",
                        ));
                    }
                }

                Instruction::Return { ret } => {
                    let res = this.r(owner).stack.read(ret);
                    arena.write_barrier(this);
                    return Ok(rebind!(arena, res));
                }
                Instruction::ReturnUndefined { _ignore } => {
                    arena.write_barrier(this);
                    return Ok(Value::undefined());
                }
                _ => {
                    return Err(this.create_runtime_error(
                        owner,
                        arena,
                        atoms,
                        "invalid instruction",
                        None,
                    ))
                }
            }
        }
    }

    /// Returns wether a value is falsish.
    pub fn is_falsish<'l>(this: GcRealm<'gc,'own>, owner: &Owner<'own>, value: Value<'l, 'own>) -> bool {
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

    /// Returns wether a values is like null, i.e. null or undefined
    pub fn is_nullish<'l>(this: GcRealm<'gc,'own>, value: Value<'l, 'own>) -> bool {
        value.is_null() || value.is_undefined()
    }

    /// Coerces a value to a string.
    pub fn to_string<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        value: Value<'_, 'own>,
    ) -> Result<Gc<'l, 'own, String>, Value<'l, 'own>> {
        let primitive = rebind_try!(arena, this.to_primitive(owner, arena, atoms, value, true));
        let primitive = rebind!(arena, primitive);
        let res = Self::to_string_primitive(this,arena, atoms, primitive)
            .expect("to_primitive returned an object");

        Ok(rebind!(arena, res))
    }

    /// Coerces a value which is already a primitive to a string.
    pub fn to_string_primitive<'l>(
        this: GcRealm<'gc,'own>,
        arena: &'l Root< 'own>,
        _atoms: &Atoms,
        value: Value<'_, 'own>,
    ) -> Option<Gc<'l, 'own, String>> {
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

    /// Coerces a value to a object.
    pub fn to_object<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l Root< 'own>,
        atoms: &Atoms,
        value: Value<'_, 'own>,
    ) -> Result<GcObject<'l, 'own>, Value<'l, 'own>> {
        if let Some(x) = value.into_object() {
            return Ok(rebind!(arena, x));
        }
        if let Some(x) = value.into_string() {
            let proto = this.borrow(owner).builtin.string_proto;
            let object = Object::new_gc(
                arena,
                Some(proto),
                ObjectFlags::ORDINARY,
                ObjectKind::String(x),
            );
            return Ok(rebind!(arena, object));
        }
        if value.is_number() {
            let value = if let Some(x) = value.into_int() {
                x as f64
            } else {
                value.into_float().unwrap()
            };
            let proto = this.borrow(owner).builtin.number_proto;
            let object = Object::new_gc(
                arena,
                Some(proto),
                ObjectFlags::ORDINARY,
                ObjectKind::Number(value),
            );
            return Ok(rebind!(arena, object));
        }
        if let Some(x) = value.into_bool() {
            let proto = this.borrow(owner).builtin.boolean_proto;
            let object = Object::new_gc(
                arena,
                Some(proto),
                ObjectFlags::ORDINARY,
                ObjectKind::Boolean(x),
            );
            return Ok(rebind!(arena, object));
        }
        if value.is_null() {
            return Err(this.create_type_error(
                owner,
                arena,
                atoms,
                "cannot make null into an object",
            ));
        }
        if value.is_undefined() {
            return Err(this.create_type_error(
                owner,
                arena,
                atoms,
                "cannot make null into an object",
            ));
        }
        todo!("toObject: {:?}", value);
    }

    /// Coerces a value to a number.
    pub fn to_number<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        value: Value<'_, 'own>,
    ) -> Result<Value<'static, 'own>, Value<'l, 'own>> {
        let prim = rebind_try!(arena, this.to_primitive(owner, arena, atoms, value, false));
        let res = Self::to_number_primitive(this,owner, prim)
            .expect("to primitive returned an object");
        Ok(res)
    }

    /// Coerces a value which is already a primitive to a number.
    pub fn to_number_primitive(
        this: GcRealm<'gc,'own>,
        owner: &Owner<'own>,
        value: Value<'_, 'own>,
    ) -> Option<Value<'static, 'own>> {
        if let Some(x) = value.into_int() {
            Some(x.into())
        } else if let Some(x) = value.into_float() {
            Some(Value::ensure_float(x))
        } else if value.is_undefined() {
            Some(Value::nan())
        } else if value.is_null() {
            Some(Value::from(0i32))
        } else if let Some(value) = value.into_string() {
            if let Ok(v) = value.borrow(owner).parse::<f64>() {
                Some(Value::from(v))
            } else {
                Some(Value::nan())
            }
        } else if let Some(b) = value.into_bool() {
            if b {
                Some(Value::from(1))
            } else {
                Some(Value::from(0))
            }
        } else {
            None
        }
    }

    /// Implements type conversion [`ToInt32`](https://tc39.es/ecma262/#sec-toint32)
    pub fn to_int32<'l>(
        &this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        value: Value<'_, 'own>,
    ) -> Result<i32, Value<'l, 'own>> {
        let number = this.to_number(owner, arena, atoms, value)?;
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
        &this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        value: Value<'_, 'own>,
    ) -> Result<u32, Value<'l, 'own>> {
        let number = this.to_number(owner, arena, atoms, value)?;
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

    /// Coerces a value to a primitive.
    pub fn to_primitive<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        value: Value<'_, 'own>,
        prefer_string: bool,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        const PREFER_STRING: &[atom::Atom] = &[atom::constant::toString, atom::constant::valueOf];
        const PREFER_VALUE: &[atom::Atom] = &[atom::constant::valueOf, atom::constant::toString];

        if let Some(obj) = value.into_object() {
            // Again not sure why this needs to be rooted but it doesn not work otherwise.
            root!(arena, obj);
            let keys = if prefer_string {
                PREFER_STRING
            } else {
                PREFER_VALUE
            };
            for &k in keys {
                let func = rebind_try!(arena, obj.index(owner, arena, atoms, this. k))
                    .empty_to_undefined();

                #[allow(unused_unsafe)]
                if let Some(func) = func.into_object() {
                    if func.borrow(owner).is_function() {
                        let res = rebind_try!(
                            arena,
                            this.method_call(owner, arena, atoms, func, obj.into())
                        );
                        if !res.is_object() {
                            return Ok(rebind!(arena, res));
                        }
                    }
                }
            }

            return Err(this.create_type_error(
                owner,
                arena,
                atoms,
                "Could not create a primitive from object",
            ));
        }
        return Ok(rebind!(arena, value));
    }

    /// Returns wether to values are strictly equal.
    pub fn strict_equal(
        this: GcRealm<'gc,'own>,
        owner: &Owner<'own>,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
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

    /// Returns wether to values are considered equal.
    pub fn equal<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
    ) -> Result<bool, Value<'l, 'own>> {
        if left.same_type(right) {
            return Ok(this.strict_equal(owner, left, right));
        }

        if left.is_undefined() && right.is_null() {
            return Ok(true);
        }
        if right.is_undefined() && left.is_null() {
            return Ok(true);
        }
        if left.is_number() && right.is_string() {
            // Should not return error and always a number
            let right = unsafe { dreck::rebind(this.to_number(owner, arena, atoms, right).unwrap()) };
            debug_assert!(right.is_number());
            return this.equal(owner, arena, atoms, left, right);
        }
        if right.is_number() && left.is_string() {
            // Should not return error and always a number
            let left = unsafe { dreck::rebind(this.to_number(owner, arena, atoms, left).unwrap()) };
            debug_assert!(left.is_number());
            return this.equal(owner, arena, atoms, left, right);
        }
        if left.is_bool() {
            // Should not return error and always a number
            let left = unsafe { dreck::rebind(this.to_number(owner, arena, atoms, left).unwrap()) };
            debug_assert!(left.is_number());
            return this.equal(owner, arena, atoms, left, right);
        }
        if right.is_bool() {
            // Should not return error and always a number
            let right = unsafe { dreck::rebind(this.to_number(owner, arena, atoms, right).unwrap()) };
            debug_assert!(right.is_number());
            return this.equal(owner, arena, atoms, left, right);
        }
        if !left.is_object() && right.is_object() {
            let right = rebind_try!(arena, this.to_primitive(owner, arena, atoms, right, true));
            return this.equal(owner, arena, atoms, left, right);
        }
        if !right.is_object() && left.is_object() {
            let left = rebind_try!(arena, this.to_primitive(owner, arena, atoms, left, true));
            return this.equal(owner, arena, atoms, left, right);
        }
        Ok(false)
    }

    /// Returns wether a value is considered less then an other value.
    ///
    /// Swap changes the order of evaluation.
    pub fn less_then<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
        swap: bool,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let (left, right) = if swap {
            let r = rebind_try!(arena, this.to_primitive(owner, arena, atoms, left, false));
            let l = rebind_try!(arena, this.to_primitive(owner, arena, atoms, right, false));
            (rebind!(arena, l), rebind!(arena, r))
        } else {
            let l = rebind_try!(arena, this.to_primitive(owner, arena, atoms, left, false));
            let r = rebind_try!(arena, this.to_primitive(owner, arena, atoms, right, false));
            let r = rebind!(arena, r);
            (rebind!(arena, l), r)
        };
        if left.is_string() || right.is_string() {
            let left = this.to_string_primitive(arena, atoms, left).unwrap();
            let right = this.to_string_primitive(arena, atoms, right).unwrap();

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
        let left = this.to_number_primitive(owner, left).unwrap();
        let right = this.to_number_primitive(owner, right).unwrap();
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

    /// Add to values together
    pub fn add<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let (left, right) = {
            let l = rebind_try!(arena, this.to_primitive(owner, arena, atoms, left, true));
            let r = rebind_try!(arena, this.to_primitive(owner, arena, atoms, right, true));
            let r = rebind!(arena, r);
            let l = rebind!(arena, l);
            (l, r)
        };

        if left.is_string() || right.is_string() {
            let left = this.to_string_primitive(arena, atoms, left).unwrap();
            let right = this.to_string_primitive(arena, atoms, right).unwrap();
            let v = arena.add(left.borrow(owner).to_string() + right.borrow(owner));
            return Ok(v.into());
        }
        let left = this.to_number_primitive(owner, left).unwrap();
        let right = this.to_number_primitive(owner, right).unwrap();
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
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
        op: NumericOperator,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let l = rebind_try!(arena, this.to_number(owner, arena, atoms, left));
        debug_assert!(l.is_number());
        let left = unsafe { dreck::rebind(l) };
        let r = rebind_try!(arena, this.to_number(owner, arena, atoms, right));
        debug_assert!(r.is_number());
        let right = unsafe { dreck::rebind(r) };
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

    pub fn type_of<'l>(this: GcRealm<'gc,'own>, owner: &Owner<'own>, v: Value<'l, 'own>) -> &'l str {
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
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
    ) -> Result<bool, Value<'l, 'own>> {
        let right = rebind_try!(
            arena,
            right.into_object().ok_or_else(|| this.create_type_error(
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
            return Err(this.create_type_error(
                owner,
                arena,
                atoms,
                "right hand instanceof operand is not a function",
            ));
        }

        let tgt_proto = right.index(owner, arena, atoms, this, atom::constant::prototype);
        let tgt_proto = rebind_try!(arena, tgt_proto).empty_to_undefined();
        let tgt_proto = rebind!(arena, tgt_proto);
        let tgt_proto = tgt_proto.into_object().ok_or_else(|| {
            this.create_type_error(owner, arena, atoms, "object prototype is not an object")
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
    fn coerce_int(int: i64) -> Value<'static, 'own> {
        if int as i32 as i64 == int {
            Value::from(int as i32)
        } else {
            Value::from(int as f64)
        }
    }

    #[allow(unused_unsafe)]
    pub unsafe fn construct_function<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l Root< 'own>,
        function_id: u16,
        read: &InstructionReader<'_, 'own>,
        function: GcObject<'_, 'own>,
    ) -> VmFunction<'l, 'own> {
        let bc = &read.bc.borrow(owner).functions[function_id as usize];
        let up_func = function.borrow(owner).as_vm_function();
        let upvalues = bc
            .upvalues
            .iter()
            .copied()
            .map(|x| match x {
                Upvalue::Local(register) => (*this.get()).stack.create_upvalue(arena, register),
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
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        function: GcObject<'_, 'own>,
        obj: Value<'_, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        // Life times function and object are independent of arena,
        // as such they have to be rooted so they are valid for the entire function and can be
        // rebind to the lifetime of the other

        let ctx = unsafe {
            ExecutionContext {
                function: dreck::rebind(function),
                this: dreck::rebind(obj),
                new_target: Value::undefined(),
            }
        };

        unsafe { this.vm_call(owner, arena, atoms, ctx) }
    }

    pub fn construct_call<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        function: GcObject<'_, 'own>,
        target_obj: GcObject<'_, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let proto = rebind!(
            arena,
            rebind_try!(
                arena,
                function.index(owner, arena, atoms, this, atom::constant::prototype)
            )
        )
        .into_object()
        .unwrap_or_else(|| rebind!(arena, unsafe { this.r(owner).builtin.object_proto }));
        let this = Object::new(Some(proto), ObjectFlags::ORDINARY, ObjectKind::Ordinary);
        let this = arena.add(this);
        root!(arena, this);

        // Life times function and target_obj are independent of arena,
        // as such they have to be rooted so they are valid for the entire function and can be
        // rebind to the lifetime of the other

        let ctx = unsafe {
            let new_target = dreck::rebind(target_obj).into();
            let function = dreck::rebind(function);

            ExecutionContext::<'_, 'own> {
                this: this.into(),
                function,
                new_target,
            }
        };

        let res = rebind_try!(arena, unsafe { this.vm_call(owner, arena, atoms, ctx) });
        let res = rebind!(arena, res);
        if let Some(obj) = res.into_object() {
            Ok(obj.into())
        } else {
            Ok(rebind!(arena, this).into())
        }
    }

    #[allow(unused_unsafe)]
    pub unsafe fn vm_call<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        ctx: ExecutionContext<'_, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        match ctx.function.as_function_kind(owner) {
            FunctionKind::VmFn(func) => {
                let bc = func.bc;
                let frame_size =
                    bc.borrow(owner).functions[func.function as usize].registers as u32;
                let mut instr = InstructionReader::new_unsafe(owner, bc, func.function);

                let guard = rebind_try!(
                    arena,
                    this.w(owner).stack.push_frame(frame_size).ok_or_else(|| {
                        this.create_runtime_error(
                            owner,
                            arena,
                            atoms,
                            Stack::DEPTH_EXCEEDED_MSG,
                            None,
                        )
                    })
                );
                let res = this.run(arena, owner, atoms, &mut instr, &ctx);
                let res = rebind!(arena, res);
                this.w(owner).stack.pop_frame(arena, guard);
                res
            }
            FunctionKind::StaticFn(x) => {
                let guard = rebind_try!(
                    arena,
                    this.w(owner).stack.push_frame(0).ok_or_else(|| {
                        this.create_runtime_error(
                            owner,
                            arena,
                            atoms,
                            Stack::DEPTH_EXCEEDED_MSG,
                            None,
                        )
                    })
                );
                let res = rebind!(arena, x(arena, owner, atoms, this, &ctx));
                this.w(owner).stack.pop_frame(arena, guard);
                res
            }
            FunctionKind::SharedFn(x) => {
                let guard = rebind_try!(
                    arena,
                    this.w(owner).stack.push_frame(0).ok_or_else(|| {
                        this.create_runtime_error(
                            owner,
                            arena,
                            atoms,
                            Stack::DEPTH_EXCEEDED_MSG,
                            None,
                        )
                    })
                );
                let res = rebind!(arena, (*x)(arena, owner, atoms, this, &ctx));
                this.w(owner).stack.pop_frame(arena, guard);
                res
            }
            _ => todo!(),
        }
    }

    pub fn index_non_object<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        value: Value<'_, 'own>,
        key: Value<'gc, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        assert!(!value.is_object());

        let object = if value.is_string() {
            this.borrow(owner).builtin.string_proto
        } else if value.is_number() {
            this.borrow(owner).builtin.number_proto
        } else if value.is_bool() {
            todo!("index bool")
        } else if value.is_undefined() {
            return Err(this.create_type_error(owner, arena, atoms, "undefined has no properties"));
        } else if value.is_nullish() {
            return Err(this.create_type_error(owner, arena, atoms, "null has no properties"));
        } else {
            panic!("invalid value")
        };

        let res = rebind_try!(arena, object.index_value(owner, arena, atoms, this. key));

        Ok(rebind!(arena, res))
    }

    pub fn to_integer_or_infinity<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root< 'own>,
        atoms: &Atoms,
        value: Value<'_, 'own>,
    ) -> Result<f64, Value<'l, 'own>> {
        let number = rebind_try!(arena, this.to_number(owner, arena, atoms, value));
        if let Some(x) = number.into_float() {
            if x.is_nan() {
                return Ok(0.0);
            }
            if x.is_infinite() {
                return Ok(x);
            }
            let res = x.abs().floor().copysign(x);
            return Ok(res);
        }
        Ok(number.into_int().unwrap() as f64)
    }

    pub fn to_string_radix(value: f64, radix: u8) -> String {
        assert!((2..=36).contains(&radix));

        if value.is_nan() {
            return "NaN".to_string();
        }
        if value == 0.0 {
            return "0".to_string();
        }
        if value.is_infinite() {
            if value > 0.0 {
                return "Infinity".to_string();
            }
            return "-Infinity".to_string();
        }

        let mut buffer = String::new();
        let value = if value < 0.0 {
            buffer.push('-');
            -value
        } else {
            value
        };

        let mut int = value.trunc() as u64;
        loop {
            let m = (int % radix as u64) as u32;
            int /= radix as u64;
            buffer.push(char::from_digit(m, radix.into()).unwrap());
            if int == 0 {
                break;
            }
        }

        let mut fract = value.fract();
        if fract == 0.0 {
            return buffer;
        }
        buffer.push('.');
        while fract > f64::EPSILON && buffer.len() < 21 {
            let num = fract * radix as f64;
            let int = num.trunc() as u32;
            fract = num.fract();
            buffer.push(char::from_digit(int, radix.into()).unwrap());
        }
        buffer
    }

    #[cold]
    pub fn create_type_error<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l Root< 'own>,
        atoms: &Atoms,
        message: impl Into<String>,
    ) -> Value<'l, 'own> {
        let message = arena.add(message.into());
        builtin::error::create(
            arena,
            owner,
            atoms,
            this.borrow(owner).builtin.type_error_proto,
            Some(message),
            None,
        )
        .into()
    }

    #[cold]
    pub fn create_syntax_error<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l Root< 'own>,
        atoms: &Atoms,
        message: impl Into<String>,
    ) -> Value<'l, 'own> {
        let message = arena.add(message.into());
        builtin::error::create(
            arena,
            owner,
            atoms,
            this.borrow(owner).builtin.syntax_error_proto,
            Some(message),
            None,
        )
        .into()
    }

    #[cold]
    pub fn create_runtime_error<'l>(
        this: GcRealm<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l Root< 'own>,
        atoms: &Atoms,
        message: impl Into<String>,
        context: Option<Value<'_, 'own>>,
    ) -> Value<'l, 'own> {
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
            this.borrow(owner).builtin.runtime_error_proto,
            Some(message),
            None,
        )
        .into()
    }
}
