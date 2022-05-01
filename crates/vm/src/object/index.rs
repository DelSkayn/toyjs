use crate::{
    atom::{Atom, Atoms},
    cell::CellOwner,
    gc::Arena,
    realm::GcRealm,
    GcObject, Value,
};

use super::properties::{Accessor, Property, PropertyEntry, PropertyFlag, PropertyValue};

impl<'gc, 'cell> GcObject<'gc, 'cell> {
    #[inline]
    pub fn index_value<V: Into<Value<'gc, 'cell>>>(
        self,
        owner: &mut CellOwner<'cell>,
        realm: GcRealm<'gc, 'cell>,
        key: V,
    ) -> Result<Value<'gc, 'cell>, Value<'gc, 'cell>> {
        let v = key.into();
        if let Some(atom) = v.into_atom() {
            self.index(owner, realm, atom)
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

    pub fn index(
        self,
        owner: &mut CellOwner<'cell>,
        realm: GcRealm<'gc, 'cell>,
        key: Atom,
    ) -> Result<Value<'gc, 'cell>, Value<'gc, 'cell>> {
        let borrow = self.borrow_mut_untraced(owner);
        if let Some(idx) = key.into_idx() {
            if let Some(x) = borrow.elements.get(idx as usize) {
                return Ok(x);
            } else {
                return borrow
                    .prototype
                    .map(|x| x.index(owner, realm, key))
                    .unwrap_or(Ok(Value::undefined()));
            }
        }

        let mut cur = self;
        loop {
            if let Some(prop) = cur.borrow(owner).properties.get(key) {
                match prop.as_value() {
                    PropertyValue::Value(x) => return Ok(x),
                    PropertyValue::Accessor(Accessor {
                        get: Some(_get), ..
                    }) => {
                        todo!()
                        //return realm.enter_method_call(get, self.into())
                    }
                    PropertyValue::Accessor(Accessor { get: None, .. }) => {
                        return Ok(Value::undefined())
                    }
                }
            }
            if let Some(x) = cur.borrow(owner).prototype {
                cur = x
            } else {
                return Ok(Value::undefined());
            }
        }
    }

    #[inline]
    pub fn index_set_value<'k, 'v, K, V>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &Arena<'_, 'cell>,
        realm: GcRealm<'gc, 'cell>,
        atoms: &Atoms,
        key: K,
        value: V,
    ) -> Result<(), Value<'gc, 'cell>>
    where
        K: Into<Value<'k, 'cell>>,
        V: Into<Value<'v, 'cell>>,
    {
        let v = key.into();
        if let Some(a) = v.into_atom() {
            self.index_set(owner, arena, realm, atoms, a, value.into())?;
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

    pub fn index_set(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &Arena<'_, 'cell>,
        _realm: GcRealm<'gc, 'cell>,
        _atoms: &Atoms,
        key: Atom,
        value: Value<'_, 'cell>,
    ) -> Result<(), Value<'gc, 'cell>> {
        let borrow = self.borrow_mut(owner, arena);
        if let Some(idx) = key.into_idx() {
            borrow.elements.set(idx as usize, value);
        }

        match borrow.properties.entry(key) {
            PropertyEntry::Occupied(mut x) => x.set(value),
            PropertyEntry::Vacant(x) => x.insert(value),
            PropertyEntry::Accessor(_x) => todo!(),
            PropertyEntry::Unwritable => todo!(),
        }
        Ok(())
    }

    #[inline]
    pub fn raw_index_set<V>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &Arena<'_, 'cell>,
        atoms: &Atoms,
        key: Atom,
        value: V,
    ) where
        V: Into<Value<'gc, 'cell>>,
    {
        self.raw_index_set_prop(
            owner,
            arena,
            atoms,
            Property::value(value.into(), PropertyFlag::ordinary(), key),
        );
    }

    #[inline]
    pub fn raw_index_set_flags<V>(
        self,
        owner: &mut CellOwner<'cell>,
        arena: &Arena<'_, 'cell>,
        atoms: &Atoms,
        key: Atom,
        value: V,
        flags: PropertyFlag,
    ) where
        V: Into<Value<'gc, 'cell>>,
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
        value: Property<'gc, 'cell>,
    ) {
        let borrow = self.borrow_mut(owner, arena);
        if let Some(x) = borrow.properties.set(value) {
            atoms.decrement(x.atom());
        }
    }
}
