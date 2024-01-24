use dreck::{rebind, rebind_try, root};

use crate::{
    atom::Atom,
    exec::ExecutionContext,
    object::{
        properties::{Accessor, PropertyValue},
        Object,
    },
    value::Value,
};

use super::{properties::PropertyEntry, GcObject};

impl<'gc, 'own> Object<'gc, 'own> {
    #[inline]
    pub fn index_value<'r, V: Into<Value<'gc, 'own>>>(
        this: GcObject<'_, 'own>,
        exec: &'r mut ExecutionContext<'gc, 'own>,
        key: V,
    ) -> Result<Value<'r, 'own>, Value<'r, 'own>> {
        let v = key.into();
        if let Some(atom) = v.into_atom() {
            Object::index(this, exec, atom)
        } else {
            let v = rebind_try!(exec.root, exec.to_primitive(v, true));
            let atom = ExecutionContext::atomize_primitive(exec.owner, exec.root, exec.atoms, v);
            root_atom!(exec.root, atom);
            let res = Object::index(this, exec, atom);
            res
        }
    }

    pub fn index<'l>(
        this: GcObject<'_, 'own>,
        exec: &'l mut ExecutionContext<'gc, 'own>,
        key: Atom<'_, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        let borrow = this.borrow_mut(exec.owner, exec.root);
        if let Some(idx) = key.into_integer() {
            if let Some(x) = borrow.elements.get(idx as usize) {
                return Ok(rebind!(exec.root, x));
            } else if let Some(prototype) = borrow.prototype {
                root!(exec.root, prototype);
                return Object::index(prototype, exec, key);
            } else {
                return Ok(Value::empty());
            }
        }

        let mut cur = this;
        loop {
            if let Some(prop) = cur.borrow(exec.owner).properties.get(key) {
                match prop.as_value() {
                    PropertyValue::Value(x) => return Ok(rebind!(exec.root, x)),
                    PropertyValue::Accessor(Accessor { get: Some(get), .. }) => {
                        todo!()
                        //return Realm::method_call(realm, owner, arena, atoms, get, this.into())
                    }
                    PropertyValue::Accessor(Accessor { get: None, .. }) => {
                        return Ok(Value::undefined())
                    }
                }
            }
            if let Some(x) = cur.borrow(exec.owner).prototype {
                cur = x
            } else {
                return Ok(Value::empty());
            }
        }
    }

    #[inline]
    pub fn index_set_value<'k, 'v, 'r, 'l, K, V>(
        this: GcObject<'_, 'own>,
        exec: &'r mut ExecutionContext<'gc, 'own>,
        key: K,
        value: V,
    ) -> Result<(), Value<'r, 'own>>
    where
        K: Into<Value<'k, 'own>>,
        V: Into<Value<'v, 'own>>,
    {
        let v = key.into();
        if let Some(a) = v.into_atom() {
            Object::index_set(this, exec, a, value.into())?;
        } else {
            let v = rebind_try!(exec.root, exec.to_primitive(v, true));
            let atom = ExecutionContext::atomize_primitive(exec.owner, exec.root, exec.atoms, v);
            root_atom!(exec.root, atom);
            Object::index_set(this, exec, atom, value.into())?;
        }
        Ok(())
    }

    pub fn index_set<'l>(
        this: GcObject<'_, 'own>,
        exec: &mut ExecutionContext<'gc, 'own>,
        key: Atom<'_, 'own>,
        value: Value<'_, 'own>,
    ) -> Result<(), Value<'l, 'own>> {
        let borrow = this.borrow_mut(exec.owner, exec.root);
        if let Some(idx) = key.into_integer() {
            borrow.elements.set(idx as usize, value);
        }

        match borrow.properties.entry(key) {
            PropertyEntry::Occupied(mut x) => x.set(value),
            PropertyEntry::Vacant(x) => x.insert(value),
            PropertyEntry::Accessor(set) => {
                root!(exec.root, set);
                todo!()
                /*
                unsafe {
                    //exec.stack.borrow_mut(exec.owner, exec.root).push(value);
                }
                if let Err(e) = Realm::method_call(realm, owner, arena, atoms, set, this.into()) {
                    return Err(rebind!(arena, e));
                }
                        */
            }
            PropertyEntry::Unwritable => {}
        }
        Ok(())
    }
}
