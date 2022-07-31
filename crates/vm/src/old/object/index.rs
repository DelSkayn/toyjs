use common::atom::{Atom, Atoms};

use dreck::{Owner, Root, rebind, root};

use crate::{GcObject, Object, Realm, Value, realm::GcRealm};

use super::properties::{Accessor, Property, PropertyEntry, PropertyFlags, PropertyValue};

impl<'gc, 'own> Object<'gc, 'own> {
    #[inline]
    pub fn index_value<'l, V: Into<Value<'gc, 'own>>>(
        this: GcObject<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root<'own>,
        atoms: &Atoms,
        realm: GcRealm<'gc, 'own>,
        key: V,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let v = key.into();
        if let Some(atom) = v.into_atom() {
            Object::index(this,owner, arena, atoms, realm, atom)
        } else {
            let v = rebind_try!(arena, Realm::to_primitive(realm,owner, arena, atoms, v, true));
            let atom = Realm::atomize_primitive(owner, atoms, v);
            let res = Object::index(this,owner, arena, atoms, realm, atom);
            atoms.decrement(atom);
            res
        }
    }

    pub fn index<'l>(
        this: GcObject<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root<'own>,
        atoms: &Atoms,
        realm: GcRealm<'_, 'own>,
        key: Atom,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let borrow = this.borrow_mut(owner, arena);
        if let Some(idx) = key.into_idx() {
            if let Some(x) = borrow.elements.get(idx as usize) {
                return Ok(rebind!(arena, x));
            } else if let Some(prototype) = borrow.prototype {
                root!(arena, prototype);
                return Object::index(prototype,owner, arena, atoms, realm, key);
            } else {
                return Ok(Value::empty());
            }
        }

        let mut cur = this;
        loop {
            if let Some(prop) = cur.borrow(owner).properties.get(key) {
                match prop.as_value() {
                    PropertyValue::Value(x) => return Ok(rebind!(arena, x)),
                    PropertyValue::Accessor(Accessor { get: Some(get), .. }) => {
                        return Realm::method_call(realm,owner, arena, atoms, get, this.into())
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
        this: GcObject<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root<'own>,
        atoms: &Atoms,
        realm: GcRealm<'_, 'own>,
        key: K,
        value: V,
    ) -> Result<(), Value<'l, 'own>>
    where
        K: Into<Value<'k, 'own>>,
        V: Into<Value<'v, 'own>>,
    {
        let v = key.into();
        if let Some(a) = v.into_atom() {
            Object::index_set(this,owner, arena, atoms, realm, a, value.into())?;
        } else {
            let v = rebind_try!(arena, Realm::to_primitive(realm,owner, arena, atoms, v, true));
            let atom = Realm::atomize_primitive(owner, atoms, v);
            Object::index_set(this,owner, arena, atoms, realm, atom, value.into())?;
            atoms.decrement(atom);
        }
        Ok(())
    }

    pub fn index_set<'l>(
        this: GcObject<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &'l mut Root<'own>,
        atoms: &Atoms,
        realm: GcRealm<'_, 'own>,
        key: Atom,
        value: Value<'_, 'own>,
    ) -> Result<(), Value<'l, 'own>> {
        let borrow = this.borrow_mut(owner, arena);
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
                if let Err(e) = Realm::method_call(realm,owner, arena, atoms, set, this.into()) {
                    return Err(rebind!(arena, e));
                }
            }
            PropertyEntry::Unwritable => {}
        }
        Ok(())
    }

    #[inline]
    pub fn raw_index_set<'a, V>(
        this: GcObject<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &Root<'own>,
        atoms: &Atoms,
        key: Atom,
        value: V,
    ) where
        V: Into<Value<'a, 'own>>,
    {
        Object::raw_index_set_prop(
            this,
            owner,
            arena,
            atoms,
            Property::value(value.into(), PropertyFlags::ordinary(), key),
        );
    }

    #[inline]
    pub fn raw_index_set_flags<'a, V>(
        this: GcObject<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &Root<'own>,
        atoms: &Atoms,
        key: Atom,
        value: V,
        flags: PropertyFlags,
    ) where
        V: Into<Value<'a, 'own>>,
    {
        Object::raw_index_set_prop(
            this,
            owner,
            arena,
            atoms,
            Property::value(value.into(), flags, key),
        );
    }

    pub fn raw_index_set_prop(
        this: GcObject<'gc,'own>,
        owner: &mut Owner<'own>,
        arena: &Root<'own>,
        atoms: &Atoms,
        value: Property<'_, 'own>,
    ) {
        let borrow = this.borrow_mut(owner, arena);
        let atom = value.atom();
        if borrow.properties.set(value).is_none() {
            atoms.increment(atom);
        }
    }
}
