use dreck::{rebind, rebind_try, Gc, Owner, Root};

use crate::{
    atom::{self, Atom, Atoms},
    value::Value,
};

use super::ExecutionContext;

impl<'r, 'l: 'r, 'gc, 'own> ExecutionContext<'l, 'gc, 'own> {
    pub fn is_falshish(owner: &Owner<'own>, value: Value<'gc, 'own>) -> bool {
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

    pub fn to_string(
        &'r mut self,
        value: Value<'_, 'own>,
    ) -> Result<Gc<'r, 'own, String>, Value<'r, 'own>> {
        let primitive = rebind_try!(self.root, self.to_primitive(value, true));
        let res = rebind!(
            self.root,
            Self::to_string_primitive(self.root, primitive)
                .expect("to_primitive returned an object")
        );
        Ok(res)
    }

    pub fn to_string_primitive(
        root: &'r Root<'own>,
        value: Value<'_, 'own>,
    ) -> Option<Gc<'r, 'own, String>> {
        if let Some(value) = value.into_string() {
            Some(rebind!(root, value))
        } else if let Some(value) = value.into_int() {
            Some(root.add(value.to_string()))
        } else if let Some(value) = value.into_float() {
            Some(root.add(value.to_string()))
        } else if value.is_null() {
            Some(root.add("null".to_string()))
        } else if value.is_undefined() {
            Some(root.add("undefined".to_string()))
        } else if value.is_true() {
            Some(root.add("true".to_string()))
        } else if value.is_false() {
            Some(root.add("false".to_string()))
        } else if value.is_object() {
            None
        } else {
            todo!("toString: {:?}", value);
        }
    }

    /// Coerces a value to a number.
    pub fn to_number(
        &'r mut self,
        value: Value<'_, 'own>,
    ) -> Result<Value<'static, 'own>, Value<'r, 'own>> {
        let prim = rebind_try!(self.root, self.to_primitive(value, false));
        let res =
            Self::to_number_primitive(self.owner, prim).expect("to primitive returned an object");
        Ok(res)
    }

    /// Coerces a value which is already a primitive to a number.
    pub fn to_number_primitive(
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
    pub fn to_int32(&'r mut self, value: Value<'_, 'own>) -> Result<i32, Value<'r, 'own>> {
        let number = self.to_number(value)?;
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
    pub fn to_uint32(&'r mut self, value: Value<'_, 'own>) -> Result<u32, Value<'r, 'own>> {
        let number = self.to_number(value)?;
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

    pub fn to_primitive<'a>(
        &'r mut self,
        _value: Value<'_, 'own>,
        _prefer_string: bool,
    ) -> Result<Value<'r, 'own>, Value<'r, 'own>> {
        todo!()
    }

    pub fn atomize_primitive(
        owner: &'r Owner<'own>,
        root: &'r Root<'own>,
        atoms: &mut Atoms<'gc, 'own>,
        value: Value<'r, 'own>,
    ) -> Atom<'r, 'own> {
        if let Some(x) = value.into_int() {
            atoms.atomize_integer(root, x)
        } else if let Some(x) = value.into_string() {
            atoms.atomize_string(root, &x.borrow(owner))
        } else if let Some(x) = value.into_bool() {
            if x {
                atom::constants::r#true()
            } else {
                atom::constants::r#false()
            }
        } else if let Some(x) = value.into_atom() {
            unsafe { dreck::rebind(x) }
        } else if let Some(x) = value.into_float() {
            atoms.atomize_string(root, &format!("{x}"))
        } else {
            panic!("atomize primitive was called on a non primitive: {value:?}")
        }
    }
}
