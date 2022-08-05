use dreck::rebind;

use crate::{
    instructions::Instruction,
    object::{Object, ObjectFlags, ObjectKind},
    realm::InstructionReader,
    value::Value,
};

use super::{operator::NumericOperator, ExecutionContext};

// shorthand for writing to a register.
// used when ExecutionContext::w leads to lifetime conflict.
macro_rules! w {
    ($self:expr,$reg:expr,$value:expr) => {
        $self
            .stack
            .unsafe_borrow_mut($self.owner)
            .write($reg, $value.into())
    };
}

// dispatch try. will unwind the stack if an error is thrown.
macro_rules! dtry {
    ($self:expr,$value:expr) => {
        match $value {
            Ok(x) => x,
            Err(_) => todo!(),
        }
    };
}

impl<'l, 'gc, 'own> ExecutionContext<'l, 'gc, 'own> {
    #[inline]
    unsafe fn r(&'l self, reg: u8) -> Value<'gc, 'own> {
        self.stack.borrow(self.owner).read(reg)
    }

    // Shorthand for Gc::unsafe_borrow_mut
    //
    // Should only be used as long as before each collection and return from the loop the a
    // write_barrier is done for the realm
    #[inline]
    unsafe fn w<'a>(&mut self, reg: u8, value: impl Into<Value<'a, 'own>>) {
        self.stack
            .unsafe_borrow_mut(self.owner)
            .write(reg, value.into())
    }

    pub(super) unsafe fn dispatch(
        &mut self,
        mut reader: InstructionReader<'gc, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        loop {
            match reader.next(self.owner) {
                Instruction::LoadConst { dst, cons } => {
                    let cons = reader.constant(cons, self.owner);
                    self.w(dst, cons);
                }
                Instruction::LoadGlobal { dst } => {
                    let global = self.realm.borrow(self.owner).global;
                    self.w(dst, global);
                }
                Instruction::LoadThis { dst } => self.w(dst, self.this),
                Instruction::LoadTarget { dst } => self.w(dst, self.new_target),
                Instruction::Move { dst, src } => {
                    let v = self.r(src);
                    self.w(dst, v);
                }
                Instruction::CreateObject { dst } => {
                    self.root.write_barrier(self.stack);
                    self.root.collect(self.owner);
                    let obj = Object::new_gc(
                        self.root,
                        None,
                        ObjectFlags::ORDINARY,
                        ObjectKind::Ordinary,
                    );
                    w!(self, dst, obj);
                }

                Instruction::IndexAssign { obj, key, src } => {
                    let obj = self.r(obj);
                    let key = self.r(key);
                    let value = self.r(src);
                    if let Some(obj) = obj.into_object() {
                        dtry!(self, Object::index_set_value(obj, self, key, value))
                    } else {
                        todo!()
                    }
                }
                Instruction::Index { dst, obj, key } => {
                    let obj = self.r(obj);
                    let key = self.r(key);
                    if let Some(obj) = obj.into_object() {
                        let v = dtry!(self, Object::index_value(obj, self, key));
                        let v = rebind!(self.root, v);
                        w!(self, dst, v);
                    } else {
                        todo!()
                    }
                }
                Instruction::GlobalAssign { key, src } => {
                    let obj = self.realm.borrow(self.owner).global;
                    let key = self.r(key);
                    let value = self.r(src);
                    dtry!(self, Object::index_set_value(obj, self, key, value));
                }
                Instruction::GlobalIndex { dst, key } => {
                    let obj = self.realm.borrow(self.owner).global;
                    let key = self.r(key);
                    let v = dtry!(self, Object::index_value(obj, self, key));
                    let v = rebind!(self.root, v);
                    w!(self, dst, v);
                }
                Instruction::InstanceOf { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);
                    let res = dtry!(self, self.instance_of(left, right));
                    self.w(dst, res)
                }
                Instruction::TypeOf { dst, src } => {
                    let src = self.r(src);
                    let v = self.root.add(Self::type_of(self.owner, src).to_string());
                    w!(self, dst, v);
                }

                Instruction::Add { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    if let Some((left, right)) = left.into_int().zip(right.into_int()) {
                        if let Some(x) = left.checked_add(right) {
                            self.w(dst, x);
                            continue;
                        }
                    }

                    let res = dtry!(self, self.add(left, right));
                    let res = rebind!(self.root, res);
                    w!(self, dst, res);
                }
                Instruction::Sub { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    if let Some((left, right)) = left.into_int().zip(right.into_int()) {
                        if let Some(x) = left.checked_sub(right) {
                            self.w(dst, x);
                            continue;
                        }
                    }

                    let res = dtry!(
                        self,
                        self.numeric_operator(left, right, NumericOperator::Sub)
                    );
                    let res = rebind!(self.root, res);
                    w!(self, dst, res);
                }
                Instruction::Mul { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    if let Some((left, right)) = left.into_int().zip(right.into_int()) {
                        if let Some(x) = left.checked_mul(right) {
                            self.w(dst, x);
                            continue;
                        }
                    }

                    let res = dtry!(
                        self,
                        self.numeric_operator(left, right, NumericOperator::Mul)
                    );
                    let res = rebind!(self.root, res);
                    w!(self, dst, res);
                }
                Instruction::Div { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = dtry!(
                        self,
                        self.numeric_operator(left, right, NumericOperator::Div)
                    );
                    let res = rebind!(self.root, res);
                    w!(self, dst, res);
                }
                Instruction::Pow { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = dtry!(
                        self,
                        self.numeric_operator(left, right, NumericOperator::Pow)
                    );
                    let res = rebind!(self.root, res);
                    w!(self, dst, res);
                }
                Instruction::Mod { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = dtry!(
                        self,
                        self.numeric_operator(left, right, NumericOperator::Mod)
                    );
                    let res = rebind!(self.root, res);
                    w!(self, dst, res);
                }

                Instruction::BitwiseAnd { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let left = dtry!(self, self.to_int32(left));
                    let right = dtry!(self, self.to_int32(right));

                    let res = left & right;
                    self.w(dst, res)
                }
                Instruction::BitwiseOr { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let left = dtry!(self, self.to_int32(left));
                    let right = dtry!(self, self.to_int32(right));

                    let res = left | right;
                    self.w(dst, res)
                }
                Instruction::BitwiseXor { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let left = dtry!(self, self.to_int32(left));
                    let right = dtry!(self, self.to_int32(right));

                    let res = left ^ right;
                    self.w(dst, res);
                }
                Instruction::BitwiseNot { dst, src } => {
                    let src = self.r(src);
                    let src = dtry!(self, self.to_int32(src));

                    let res = !src;
                    self.w(dst, res);
                }

                Instruction::ShiftLeft { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let left = dtry!(self, self.to_int32(left));
                    let right = dtry!(self, self.to_int32(right));

                    let res = left << (right % 32);
                    self.w(dst, res);
                }
                Instruction::ShiftRight { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let left = dtry!(self, self.to_int32(left));
                    let right = dtry!(self, self.to_int32(right));

                    let res = left >> (right % 32);
                    self.w(dst, res);
                }
                Instruction::ShiftUnsigned { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let left = dtry!(self, self.to_uint32(left));
                    let right = dtry!(self, self.to_int32(right));

                    let res = left >> (right % 32) as u32;
                    self.w(dst, res as f64);
                }

                Instruction::Equal { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = dtry!(self, self.equal(left, right));
                    self.w(dst, res)
                }
                Instruction::NotEqual { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = !dtry!(self, self.equal(left, right));
                    self.w(dst, res)
                }
                Instruction::SEqual { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = Self::strict_equal(self.owner, left, right);
                    self.w(dst, res)
                }
                Instruction::SNotEqual { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = !Self::strict_equal(self.owner, left, right);
                    self.w(dst, res)
                }
                Instruction::Greater { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = dtry!(self, self.less_then(left, right, true));

                    if res.is_undefined() {
                        self.w(dst, false);
                    } else {
                        self.w(dst, res);
                    }
                }
                Instruction::GreaterEq { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = dtry!(self, self.less_then(left, right, false));

                    let res = res.is_false() || !res.is_undefined();
                    self.w(dst, res);
                }
                Instruction::Less { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = dtry!(self, self.less_then(left, right, false));

                    if res.is_undefined() {
                        self.w(dst, false);
                    } else {
                        self.w(dst, res);
                    }
                }
                Instruction::LessEq { dst, left, righ } => {
                    let left = self.r(left);
                    let right = self.r(righ);

                    let res = dtry!(self, self.less_then(left, right, true));

                    let res = res.is_false() || !res.is_undefined();
                    self.w(dst, res);
                }
                Instruction::IsNullish { dst, src } => {
                    let src = self.r(src);
                    self.w(dst, src.is_nullish());
                }
                Instruction::Not { dst, src } => {
                    let src = self.r(src);
                    self.w(dst, Self::is_falsish(self.owner, src));
                }

                Instruction::Negative { dst, src } => {
                    let src = self.r(src);
                    let number = dtry!(self, self.to_number(src));
                    let number = number
                        .into_float()
                        .or_else(|| number.into_int().map(|x| x as f64))
                        .unwrap();

                    self.w(dst, -number);
                }
                Instruction::Positive { dst, src } => {
                    let src = self.r(src);
                    let number = dtry!(self, self.to_number(src));
                    self.w(dst, number);
                }

                Instruction::Jump { tgt } => {
                    reader.jump(tgt);
                }
                Instruction::JumpFalse { cond, tgt } => {
                    let cond = self.r(cond);
                    if Self::is_falsish(self.owner, cond) {
                        reader.jump(tgt);
                    }
                }
                Instruction::JumpTrue { cond, tgt } => {
                    let cond = self.r(cond);
                    if !Self::is_falsish(self.owner, cond) {
                        reader.jump(tgt);
                    }
                }

                x => todo!("{x:?}"),
            }
        }
    }
}
