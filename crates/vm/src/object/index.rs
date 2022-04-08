use crate::{atom::Atom, Gc, Realm, Value, VmInner};

use super::{
    property::{Accessor, PropertyValue, SetResult},
    Object, Property, PropertyFlags,
};

impl Gc<Object> {
    #[inline]
    pub unsafe fn index_value<V: Into<Value>>(self, realm: &Realm, key: V) -> Result<Value, Value> {
        let v = key.into();
        if let Some(atom) = v.into_atom() {
            self.index(realm, atom)
        } else {
            let atom = realm.to_atom(v)?;
            let res = self.index(realm, atom);
            realm.vm().decrement(atom);
            res
        }
    }

    pub unsafe fn index(self, realm: &Realm, key: Atom) -> Result<Value, Value> {
        if let Some(idx) = key.into_idx() {
            if let Some(x) = self.elements.get(idx as usize) {
                return Ok(x);
            } else {
                return self
                    .prototype
                    .map(|x| x.index(realm, key))
                    .unwrap_or(Ok(Value::undefined()));
            }
        }

        if let Some(prop) = self.properties.get(key) {
            match prop.into_value() {
                PropertyValue::Value(x) => return Ok(x),
                PropertyValue::Accessor(Accessor { get: Some(get), .. }) => {
                    return realm.enter_method_call(get, self.into())
                }
                PropertyValue::Accessor(Accessor { get: None, .. }) => {
                    return Ok(Value::undefined())
                }
            }
        }

        self.prototype
            .map(|p| p.index(realm, key))
            .unwrap_or_else(|| Ok(Value::undefined()))
    }

    #[inline]
    pub unsafe fn index_set_value<K: Into<Value>, V: Into<Value>>(
        self,
        realm: &Realm,
        key: K,
        value: V,
    ) -> Result<(), Value> {
        let v = key.into();
        if let Some(a) = v.into_atom() {
            self.index_set(realm, a, value.into())?;
        } else {
            let a = realm.to_atom(v)?;
            self.index_set(realm, a, value.into())?;
            realm.vm().decrement(a);
        }
        Ok(())
    }

    pub unsafe fn index_set(self, realm: &Realm, key: Atom, value: Value) -> Result<(), Value> {
        if let Some(idx) = key.into_idx() {
            self.elements.set(idx as usize, value);
        }

        match self.properties.set_value(key, value) {
            SetResult::Setter(x) => {
                realm.stack.push(value);
                realm.enter_method_call(x, self.into())?;
                return Ok(());
            }
            SetResult::Vacant => {
                realm.vm().increment(key);
            }
            SetResult::Occupied => {}
        }
        Ok(())
    }

    #[inline]
    pub fn raw_index_set<V: Into<Value>>(self, vm: &VmInner, key: Atom, value: V) {
        self.raw_index_set_prop(
            vm,
            Property::value(value.into(), PropertyFlags::ORDINARY, key),
        );
    }

    #[inline]
    pub fn raw_index_set_flags(self, vm: &VmInner, key: Atom, value: Value, flags: PropertyFlags) {
        self.raw_index_set_prop(vm, Property::value(value, flags, key));
    }

    fn raw_index_set_prop(self, vm: &VmInner, value: Property) {
        if self.properties.set(value) {
            vm.increment(value.key);
        }
    }
}
