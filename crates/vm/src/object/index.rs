use std::collections::hash_map::Entry;

use crate::{atom::Atom, Gc, Realm, Value, VmInner};

use super::{Object, Property, PropertyFlags};

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

        if let Some(idx) = (*self.map.get()).get(&key).copied() {
            debug_assert!(self.properties.len() > idx as usize);
            // safe because all the indecies stored in the should be valid.
            let p = self.properties.get_unchecked(idx);
            if let Some(accessor) = p.get_accessor() {
                if let Some(get) = accessor.get {
                    if !get.is_function() {
                        return Err(realm.create_type_error("getter is not a function"));
                    }
                    return realm.enter_method_call(get, self.into());
                } else {
                    return self
                        .prototype
                        .map(|p| p.index(realm, key))
                        .unwrap_or_else(|| Ok(Value::undefined()));
                }
            } else {
                return Ok(p.value.value);
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

        match (*self.map.get()).entry(key) {
            Entry::Occupied(x) => {
                debug_assert!(self.properties.len() >= *x.get());
                // safe because all the indecies stored in the should be valid.
                let prop = self.properties.get_unchecked(*x.get());
                if !prop.flags.contains(PropertyFlags::WRITABLE) {
                    //TODO strict
                    return Ok(());
                }
                if let Some(accessor) = prop.get_accessor() {
                    if let Some(set) = accessor.set {
                        if !set.is_function() {
                            return Err(realm.create_type_error("setter is not a function"));
                        }
                        realm.stack.push(value);
                        realm.enter_method_call(set, self.into())?;
                    }
                    //TODO strict
                    return Ok(());
                }

                // safe because all the indecies stored in the should be valid.
                self.properties
                    .set_unchecked(*x.get(), Property::new_value(value, prop.flags))
            }
            Entry::Vacant(x) => {
                x.insert(self.properties.len());
                self.properties.push(Property::ordinary(value));
            }
        }
        Ok(())
    }

    #[inline]
    pub unsafe fn raw_index_set(self, vm: &VmInner, key: Atom, value: Value) {
        self.raw_index_set_prop(vm, key, Property::ordinary(value));
    }

    #[inline]
    pub unsafe fn raw_index_set_flags(
        self,
        vm: &VmInner,
        key: Atom,
        value: Value,
        flags: PropertyFlags,
    ) {
        self.raw_index_set_prop(vm, key, Property::new_value(value, flags));
    }

    pub unsafe fn raw_index_set_prop(self, vm: &VmInner, key: Atom, value: Property) {
        match (*self.map.get()).entry(key) {
            Entry::Occupied(x) => {
                debug_assert!(self.properties.len() >= *x.get());
                // safe because all the indecies stored in the should be valid.
                self.properties.set_unchecked(*x.get(), value);
            }
            Entry::Vacant(x) => {
                x.insert(self.properties.len());
                vm.increment(key);
                self.properties.push(value);
            }
        }
    }
}
