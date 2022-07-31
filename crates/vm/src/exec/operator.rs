use dreck::{rebind, rebind_try, Owner};

use crate::value::Value;

use super::ExecutionContext;

pub enum NumericOperator {
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

impl<'r, 'l: 'r, 'gc, 'own> ExecutionContext<'l, 'gc, 'own> {
    /// Add to values together
    pub fn add(
        &'r mut self,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
    ) -> Result<Value<'r, 'own>, Value<'r, 'own>> {
        let (left, right) = {
            let l = rebind_try!(self.root, self.to_primitive(left, true));
            root_value!(self.root, l);
            let r = rebind_try!(self.root, self.to_primitive(right, true));
            let l = rebind!(self.root, l);
            (l, r)
        };

        if left.is_string() || right.is_string() {
            let left = Self::to_string_primitive(self.root, left).unwrap();
            let right = Self::to_string_primitive(self.root, right).unwrap();
            let v = self
                .root
                .add(left.borrow(self.owner).to_string() + right.borrow(self.owner));
            return Ok(v.into());
        }
        let left = Self::to_number_primitive(self.owner, left).unwrap();
        let right = Self::to_number_primitive(self.owner, right).unwrap();
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
    pub fn numeric_operator(
        &'r mut self,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
        op: NumericOperator,
    ) -> Result<Value<'r, 'own>, Value<'r, 'own>> {
        let left = match self.to_number(left) {
            Ok(x) => x,
            Err(e) => return Err(rebind!(self.root, e).into()),
        };
        debug_assert!(left.is_number());
        let right = match self.to_number(right) {
            Ok(x) => x,
            Err(e) => return Err(rebind!(self.root, e).into()),
        };
        debug_assert!(right.is_number());
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

    pub fn type_of<'a>(owner: &Owner<'own>, v: Value<'a, 'own>) -> &'a str {
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

    pub fn instance_of(
        &'r mut self,
        _left: Value<'_, 'own>,
        _right: Value<'_, 'own>,
    ) -> Result<bool, Value<'l, 'own>> {
        todo!()
        /*
        let right = rebind_try!(
            self.root,
            right.into_object().ok_or_else(||
                this.create_type_error(
                self.owner,
                self.root,
                atoms,
                "invalid `instanceof` operand"
            )
                todo!())
        );

        let left = if let Some(x) = left.into_object() {
            x
        } else {
            return Ok(false);
        };

        // TODO implement @@hasInstance method
        if right.borrow(self.owner).is_function() {
            todo!()
            return Err(this.create_type_error(
                owner,
                self.root,
                atoms,
                "right hand instanceof operand is not a function",
            ));
        }

        todo!()
        let tgt_proto = right.index(owner, self.root, atoms, this, atom::constant::prototype);
        let tgt_proto = rebind_try!(self.root, tgt_proto).empty_to_undefined();
        let tgt_proto = rebind!(self.root, tgt_proto);
        let tgt_proto = tgt_proto.into_object().ok_or_else(|| {
            todo!()
            this.create_type_error(owner, self.root, atoms, "object prototype is not an object")
        });

        let tgt_proto = rebind_try!(self.root, tgt_proto);

        let mut cur = rebind!(self.root, left);
        while let Some(proto) = cur.borrow(self.owner).prototype() {
            if Gc::ptr_eq(proto,tgt_proto) {
                return Ok(true);
            }
            cur = proto;
        }
        Ok(false)
            */
    }
}
