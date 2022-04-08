use crate::{
    atom,
    object::{ObjectFlags, ObjectKind, Property, PropertyFlags},
    realm::ExecutionContext,
    Gc, Object, Realm, Value, VmInner,
};

use super::new_func;

unsafe fn construct(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    if exec.new_target.is_undefined()
        || exec
            .new_target
            .into_object()
            .map_or(false, |x| x.ptr_eq(exec.function))
    {
        if realm.stack.frame_size() == 0 {
            return Ok(Object::new_gc(
                realm.vm(),
                Some(realm.builtin.object_proto),
                ObjectFlags::empty(),
                ObjectKind::Ordinary,
            )
            .into());
        }
        let value = realm.stack.read(0);
        if value.is_undefined() || value.is_null() {
            return Ok(Object::new_gc(
                realm.vm(),
                Some(realm.builtin.object_proto),
                ObjectFlags::empty(),
                ObjectKind::Ordinary,
            )
            .into());
        }
        return Ok(realm.to_object(value)?.into());
    }

    let proto = if let Some(object) = exec.new_target.into_object() {
        object.index(realm, atom::constant::prototype)?
    } else {
        Value::undefined()
    };
    let proto = proto.into_object().unwrap_or(realm.builtin.error_proto);

    let object = Object::new_gc(
        realm.vm(),
        Some(proto),
        ObjectFlags::ORDINARY,
        ObjectKind::Ordinary,
    );
    Ok(object.into())
}

unsafe fn get_prototype_of(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let value = if realm.stack.frame_size() > 0 {
        realm.stack.read(0)
    } else {
        Value::undefined()
    };
    let object = realm.to_object(value)?;
    Ok(object
        .prototype
        .map(Value::from)
        .unwrap_or(Value::undefined()))
}

unsafe fn is(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    if realm.stack.frame_size() > 0 {
        let a = realm.stack.read(0);
        if realm.stack.frame_size() > 1 {
            let b = realm.stack.read(0);
            Ok(realm.strict_equal(a, b).into())
        } else {
            Ok(a.is_undefined().into())
        }
    } else {
        Ok(true.into())
    }
}

unsafe fn is_extensible(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    if realm.stack.frame_size() > 0 {
        if let Some(obj) = realm.stack.read(0).into_object() {
            Ok(obj.flags.contains(ObjectFlags::EXTENABLE).into())
        } else {
            Ok(false.into())
        }
    } else {
        Ok(false.into())
    }
}

unsafe fn assign(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let args = realm.stack.frame_size();
    if args == 0 {
        return Ok(Value::undefined());
    }
    let to = realm.to_object(realm.stack.read(0))?;
    if args == 1 {
        return Ok(to.into());
    }

    // Keep `to` alive during for the duration of the function
    realm.stack.push_temp(to.into());

    for i in 1..realm.stack.frame_size() {
        let from = realm.to_object(realm.stack.read(i as u8))?;
        from.elements.for_each(|idx, value| {
            // These are all elements so we can use raw as no getter can be triggered
            to.elements.set(idx, value);
        });
        // Need to just straight up clone since getters can add or remove object keys.
        let properties = from.properties.clone_properties();
        for p in properties {
            let prop = if let Some(prop) = from.properties.get(p.key) {
                prop
            } else {
                continue;
            };
            if !prop.flags.contains(PropertyFlags::ENUMERABLE) {
                continue;
            }
            let v = if let Some(x) = prop.into_value().get(realm, from)? {
                x
            } else {
                continue;
            };

            if to
                .properties
                .set(Property::value(v, PropertyFlags::ORDINARY, p.key))
            {
                realm.vm().increment(p.key);
            }
        }
    }
    Ok(to.into())
}

unsafe fn create(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    if realm.stack.frame_size() == 0 {
        return Err(realm.create_type_error("Object.create requires atleast a single argument"));
    }
    let arg = realm.stack.read(0);
    if !arg.is_object() && !arg.is_null() {
        return Err(realm.create_type_error("argument is not a object or null"));
    }
    let obj = Object::new_gc(
        realm.vm(),
        arg.into_object(),
        ObjectFlags::ORDINARY,
        ObjectKind::Ordinary,
    );
    if realm.stack.frame_size() == 1 {
        return Ok(obj.into());
    }

    // Keep to alive
    realm.stack.push_temp(obj.into());

    let from = realm.stack.read(1);
    let from = realm.to_object(from)?;
    let properties = from.properties.clone_properties();
    for p in properties {
        let prop = if let Some(prop) = from.properties.get(p.key) {
            prop
        } else {
            continue;
        };
        if !prop.flags.contains(PropertyFlags::ENUMERABLE) {
            continue;
        }
        let v = if let Some(x) = prop.into_value().get(realm, from)? {
            x
        } else {
            continue;
        };
        let prop = if let Some(prop) = v.into_object() {
            prop
        } else {
            return Err(realm.create_type_error("properties entry value is not a object"));
        };
        let prop = Property::from_object(realm, prop, p.key)?;
        obj.properties.set(prop);
    }
    Ok(obj.into())
}

pub unsafe fn init(vm: &VmInner, op: Gc<Object>, fp: Gc<Object>, global: Gc<Object>) {
    let construct = Object::new_gc(
        vm,
        Some(fp),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct),
    );
    construct.raw_index_set(vm, atom::constant::prototype, op);

    let get_prototype_of = new_func(vm, fp, get_prototype_of, 1);
    construct.raw_index_set(vm, atom::constant::getPrototypeOf, get_prototype_of);

    let is = new_func(vm, fp, is, 2);
    construct.raw_index_set(vm, atom::constant::is, is);

    let is_extensible = new_func(vm, fp, is_extensible, 1);
    construct.raw_index_set(vm, atom::constant::isExtensible, is_extensible);

    let assign = new_func(vm, fp, assign, 2);
    construct.raw_index_set(vm, atom::constant::assign, assign);

    let create = new_func(vm, fp, create, 2);
    construct.raw_index_set(vm, atom::constant::create, create);

    construct.raw_index_set(vm, atom::constant::length, 1);
    op.raw_index_set(vm, atom::constant::constructor, construct);

    global.raw_index_set(vm, atom::constant::Object, construct)
}
