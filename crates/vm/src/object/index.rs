use common::atom::{Atom, Atoms};

use crate::{cell::CellOwner, gc::Arena, realm::GcRealm, rebind, root, GcObject, Value};

use super::properties::{Accessor, Property, PropertyEntry, PropertyFlag, PropertyValue};

impl<'gc, 'cell> GcObject<'gc, 'cell> {
    #[inline]
    pub fn index_value<'l, V: Into<Value<'gc, 'cell>>>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        realm: GcRealm<'gc, 'cell>,
        key: V,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let v = key.into();
        if let Some(atom) = v.into_atom() {
            self.index(owner, arena, atoms, realm, atom)
        } else {
            todo!()
            /*
            let atom = realm.to_atom(v)?;
            let res = self.index(realm, atom);
            realm.vm().decrement(atom);
            res
            */
        }
    }

    pub fn index<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        realm: GcRealm<'_, 'cell>,
        key: Atom,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let borrow = self.borrow_mut(owner, arena);
        if let Some(idx) = key.into_idx() {
            if let Some(x) = borrow.elements.get(idx as usize) {
                return Ok(rebind!(arena, x));
            } else if let Some(prototype) = borrow.prototype {
                root!(arena, prototype);
                return prototype.index(owner, arena, atoms, realm, key);
            } else {
                return Ok(Value::empty());
            }
        }

        let mut cur = self;
        loop {
            if let Some(prop) = cur.borrow(owner).properties.get(key) {
                match prop.as_value() {
                    PropertyValue::Value(x) => return Ok(rebind!(arena, x)),
                    PropertyValue::Accessor(Accessor { get: Some(get), .. }) => {
                        return realm.method_call(owner, arena, atoms, get, self)
                    }
                    PropertyValue::Accessor(Accessor { get: None, .. }) => {
                        return Ok(Value::undefined())
                    }
                }
            }
            if let Some(x) = cur.borrow(owner).prototype {
                cur = x
            } else {
                return Ok(Value::empty());
            }
        }
    }

    #[inline]
    pub fn index_set_value<'k, 'v, 'l, K, V>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        realm: GcRealm<'_, 'cell>,
        key: K,
        value: V,
    ) -> Result<(), Value<'l, 'cell>>
    where
        K: Into<Value<'k, 'cell>>,
        V: Into<Value<'v, 'cell>>,
    {
        let v = key.into();
        if let Some(a) = v.into_atom() {
            self.index_set(owner, arena, atoms, realm, a, value.into())?;
        } else {
            /*
            let a = realm.to_atom(v)?;
            self.index_set(realm, a, value.into())?;
            atoms.decrement(a);
            */
            todo!()
        }
        Ok(())
    }

    pub fn index_set<'l>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &'l mut Arena<'_, 'cell>,
        atoms: &Atoms,
        realm: GcRealm<'_, 'cell>,
        key: Atom,
        value: Value<'_, 'cell>,
    ) -> Result<(), Value<'l, 'cell>> {
        let borrow = self.borrow_mut(owner, arena);
        if let Some(idx) = key.into_idx() {
            borrow.elements.set(idx as usize, value);
        }

        match borrow.properties.entry(key) {
            PropertyEntry::Occupied(mut x) => x.set(value),
            PropertyEntry::Vacant(x) => {
                atoms.increment(key);
                x.insert(value)
            }
            PropertyEntry::Accessor(set) => {
                root!(arena, set);
                unsafe {
                    realm.borrow_mut(owner, arena).stack.push(value);
                }
                if let Err(e) = realm.method_call(owner, arena, atoms, set, self) {
                    return Err(rebind!(arena, e));
                }
            }
            PropertyEntry::Unwritable => {}
        }
        Ok(())
    }

    #[inline]
    pub fn raw_index_set<'a, V>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &Arena<'_, 'cell>,
        atoms: &Atoms,
        key: Atom,
        value: V,
    ) where
        V: Into<Value<'a, 'cell>>,
    {
        self.raw_index_set_prop(
            owner,
            arena,
            atoms,
            Property::value(value.into(), PropertyFlag::ordinary(), key),
        );
    }

    #[inline]
    pub fn raw_index_set_flags<'a, V>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &Arena<'_, 'cell>,
        atoms: &Atoms,
        key: Atom,
        value: V,
        flags: PropertyFlag,
    ) where
        V: Into<Value<'a, 'cell>>,
    {
        self.raw_index_set_prop(
            owner,
            arena,
            atoms,
            Property::value(value.into(), flags, key),
        );
    }

    pub fn raw_index_set_prop(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &Arena<'_, 'cell>,
        atoms: &Atoms,
        value: Property<'_, 'cell>,
    ) {
        let borrow = self.borrow_mut(owner, arena);
        if let Some(x) = borrow.properties.set(value) {
            atoms.decrement(x.atom());
        }
    }
}
