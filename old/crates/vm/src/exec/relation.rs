use dreck::{rebind, rebind_try, Gc, Owner};

use crate::value::Value;

use super::ExecutionContext;

impl<'r, 'gc: 'r, 'own> ExecutionContext<'gc, 'own> {
    /// Returns wether to values are strictly equal.
    pub fn strict_equal(
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

            return Gc::ptr_eq(left, right);
        }
        if let (Some(left), Some(right)) = (left.into_float(), right.into_float()) {
            return left == right;
        }
        todo!("strict_equal left: {:?}, right: {:?}", left, right);
    }

    /// Returns wether to values are considered equal.
    pub fn equal(
        &'r mut self,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
    ) -> Result<bool, Value<'r, 'own>> {
        if left.same_type(right) {
            return Ok(Self::strict_equal(self.owner, left, right));
        }

        if left.is_undefined() && right.is_null() {
            return Ok(true);
        }
        if right.is_undefined() && left.is_null() {
            return Ok(true);
        }
        if left.is_number() && right.is_string() {
            // Should not return error and always a number
            let right = self.to_number(right).unwrap();
            debug_assert!(right.is_number());
            return self.equal(left, right);
        }
        if right.is_number() && left.is_string() {
            // Should not return error and always a number
            let left = self.to_number(left).unwrap();
            debug_assert!(left.is_number());
            return self.equal(left, right);
        }
        if left.is_bool() {
            // Should not return error and always a number
            let left = Self::to_number_primitive(self.owner, left).unwrap();
            debug_assert!(left.is_number());
            return self.equal(left, right);
        }
        if right.is_bool() {
            // Should not return error and always a number
            let right = Self::to_number_primitive(self.owner, right).unwrap();
            debug_assert!(left.is_number());
            return self.equal(left, right);
        }
        if !left.is_object() && right.is_object() {
            let right = rebind_try!(self.root, self.to_primitive(right, true));
            root_value!(self.root, right);
            return self.equal(left, right);
        }
        if !right.is_object() && left.is_object() {
            let left = rebind_try!(self.root, self.to_primitive(left, true));
            root_value!(self.root, left);
            return self.equal(left, right);
        }
        Ok(false)
    }

    /// Returns wether a value is considered less then an other value.
    ///
    /// Swap changes the order of evaluation.
    pub fn less_then(
        &'r mut self,
        left: Value<'_, 'own>,
        right: Value<'_, 'own>,
        swap: bool,
    ) -> Result<Value<'static, 'own>, Value<'r, 'own>> {
        let (left, right) = if swap {
            let r = rebind_try!(self.root, self.to_primitive(left, false));
            root_value!(self.root, r);
            let l = rebind_try!(self.root, self.to_primitive(right, false));
            (rebind!(self.root, l), rebind!(self.root, r))
        } else {
            let l = rebind_try!(self.root, self.to_primitive(left, false));
            root_value!(self.root, l);
            let r = rebind_try!(self.root, self.to_primitive(right, false));
            let r = rebind!(self.root, r);
            (rebind!(self.root, l), r)
        };
        if left.is_string() || right.is_string() {
            let left = Self::to_string_primitive(self.root, left).unwrap();
            let right = Self::to_string_primitive(self.root, right).unwrap();

            if left
                .borrow(self.owner)
                .as_str()
                .starts_with(right.borrow(self.owner).as_str())
            {
                return Ok(false.into());
            }
            if right
                .borrow(self.owner)
                .as_str()
                .starts_with(left.borrow(self.owner).as_str())
            {
                return Ok(true.into());
            }
            let mut left = left.borrow(self.owner).as_str().chars();
            let mut right = right.borrow(self.owner).as_str().chars();
            loop {
                let left = left.next().unwrap();
                let right = right.next().unwrap();
                if left != right {
                    return Ok((left < right).into());
                }
            }
        }

        // Left and right are already primitives so they cannot return an error
        let left = Self::to_number_primitive(self.owner, left).unwrap();
        let right = Self::to_number_primitive(self.owner, right).unwrap();
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
}
