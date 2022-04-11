use crate::{
    atom,
    object::{ObjectFlags, ObjectKind, Property, PropertyFlags},
    realm::ExecutionContext,
    Gc, Object, Realm, Value, VmInner,
};

use super::new_func;

fn object_to_string(realm: &Realm, _: &mut ExecutionContext) -> Result<Value, Value> {
    Ok(realm.vm().allocate("[object Object]".to_string()).into())
}

fn construct(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    if exec.new_target.is_undefined()
        || exec
            .new_target
            .into_object()
            .map_or(false, |x| x.ptr_eq(exec.function))
    {
        let value = if let Some(x) = realm.stack.read_arg(0) {
            x
        } else {
            return Ok(Object::new_gc(
                realm.vm(),
                Some(realm.builtin.object_proto),
                ObjectFlags::empty(),
                ObjectKind::Ordinary,
            )
            .into());
        };
        if value.is_undefined() || value.is_null() {
            return Ok(Object::new_gc(
                realm.vm(),
                Some(realm.builtin.object_proto),
                ObjectFlags::empty(),
                ObjectKind::Ordinary,
            )
            .into());
        }
        unsafe {
            return Ok(realm.to_object(value)?.into());
        }
    }

    let proto = if let Some(object) = exec.new_target.into_object() {
        unsafe { object.index(realm, atom::constant::prototype)? }
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

fn get_prototype_of(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let value = realm.stack.read_arg(0).unwrap_or_else(Value::undefined);
    let object = unsafe { realm.to_object(value)? };
    Ok(object
        .prototype
        .map(Value::from)
        .unwrap_or(Value::undefined()))
}

fn is(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    if let Some(a) = realm.stack.read_arg(0) {
        if let Some(b) = realm.stack.read_arg(1) {
            Ok(realm.strict_equal(a, b).into())
        } else {
            Ok(a.is_undefined().into())
        }
    } else {
        Ok(true.into())
    }
}

fn is_extensible(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let res = realm
        .stack
        .read_arg(0)
        .and_then(|x| x.into_object())
        .map_or(false, |obj| obj.flags.contains(ObjectFlags::EXTENABLE));
    Ok(res.into())
}

fn assign(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let to = if let Some(arg) = realm.stack.read_arg(0) {
        arg
    } else {
        return Ok(Value::undefined());
    };
    let to = unsafe { realm.to_object(to)? };
    if realm.stack.frame_size() == 1 {
        return Ok(to.into());
    }

    // Keep `to` alive during for the duration of the function
    realm.stack.push_temp(to.into());

    for i in 1..realm.stack.frame_size() {
        let from = unsafe { realm.stack.read(i as u8) };
        let from = if let Some(from) = from.into_object() {
            from
        } else {
            unsafe {
                let f = realm.to_object(realm.stack.read(i as u8))?;
                realm.stack.push_temp(f.into());
                f
            }
        };
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
            if prop.is_accessor() {
                realm.vm().write_barrier(to);
            }
            let v = if let Some(x) = unsafe { prop.into_value().get(realm, from)? } {
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

unsafe fn define_prop(to: Gc<Object>, from: Gc<Object>, realm: &Realm) -> Result<(), Value> {
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
        if prop.is_accessor() {
            realm.vm().write_barrier(to);
        }
        let v = if let Some(x) = prop.into_value().get(realm, from)? {
            x
        } else {
            continue;
        };
        let prop = Property::from_value(realm, v, p.key)?;
        if to.properties.set(prop) {
            realm.vm().increment(prop.key);
        }
    }
    Ok(())
}

fn create(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let arg = realm.stack.read_arg(0).ok_or_else(|| {
        realm.create_type_error("Object.create requires atleast a single argument")
    })?;
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

    let from = unsafe { realm.stack.read(1) };
    let from = if let Some(from) = from.into_object() {
        from
    } else {
        unsafe {
            let f = realm.to_object(from)?;
            realm.stack.push_temp(f.into());
            f
        }
    };
    unsafe {
        define_prop(obj, from, realm)?;
    }
    Ok(obj.into())
}

fn define_properties(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    if realm.stack.frame_size() < 2 {
        return Err(
            realm.create_type_error("Object.defineProperties requires atleast a two arguments")
        );
    }
    let obj = realm.stack.read_arg(0).unwrap_or_else(|| {
        realm.create_type_error("Object.defineProperties requires atleast a two arguments")
    });
    let prop = realm.stack.read_arg(1).unwrap_or_else(|| {
        realm.create_type_error("Object.defineProperties requires atleast a two arguments")
    });
    let object = if let Some(obj) = obj.into_object() {
        obj
    } else {
        return Err(realm
            .create_type_error("Object.defineProperties requires a object as its first argument"));
    };

    let prop = if let Some(prop) = prop.into_object() {
        prop
    } else {
        unsafe {
            let p = realm.to_object(prop)?;
            realm.stack.push_temp(p.into());
            p
        }
    };
    unsafe {
        define_prop(object, prop, realm)?;
    }
    Ok(obj.into())
}

unsafe fn define_property(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let obj = realm.stack.read_arg(0).unwrap_or_else(Value::undefined);
    let obj = obj
        .into_object()
        .ok_or_else(|| realm.create_type_error("can't define property on non object value"))?;

    let p = realm.stack.read_arg(1).unwrap_or_else(Value::undefined);
    let p = realm.to_atom(p)?;

    let prop = realm.stack.read_arg(2).unwrap_or_else(Value::undefined);
    let prop = Property::from_value(realm, prop, p)?;
    realm.vm().write_barrier(obj);
    if obj.properties.set(prop) {
        realm.vm().increment(p);
    }
    Ok(obj.into())
}

fn freeze(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let v = realm.stack.read_arg(0).unwrap_or_else(Value::undefined);
    if let Some(obj) = v.into_object() {
        if obj.flags.contains(ObjectFlags::EXTENABLE) {
            obj.properties.freeze();
        } else {
            return Err(realm.create_type_error("can't freeze unextenable object"));
        }
    }
    Ok(v)
}

fn seal(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let v = realm.stack.read_arg(0).unwrap_or_else(Value::undefined);
    if let Some(obj) = v.into_object() {
        if obj.flags.contains(ObjectFlags::EXTENABLE) {
            obj.properties.seal();
        } else {
            return Err(realm.create_type_error("can't seal unextenable object"));
        }
    }
    Ok(v)
}

fn is_frozen(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let o = realm.stack.read_arg(0).unwrap_or_else(Value::undefined);
    if let Some(obj) = o.into_object() {
        if !obj.flags.contains(ObjectFlags::EXTENABLE) {
            return Ok(false.into());
        }
        for i in 0..obj.properties.len() {
            let prop = obj.properties.get_idx(i).unwrap();
            if prop.flags.contains(PropertyFlags::CONFIGURABLE)
                || !prop.is_accessor() && prop.flags.contains(PropertyFlags::WRITABLE)
            {
                return Ok(false.into());
            }
        }
    }
    Ok(true.into())
}

fn is_sealed(realm: &Realm, _exec: &mut ExecutionContext) -> Result<Value, Value> {
    let o = realm.stack.read_arg(0).unwrap_or_else(Value::undefined);
    if let Some(obj) = o.into_object() {
        if !obj.flags.contains(ObjectFlags::EXTENABLE) {
            return Ok(false.into());
        }
        for i in 0..obj.properties.len() {
            let prop = obj.properties.get_idx(i).unwrap();
            if prop.flags.contains(PropertyFlags::CONFIGURABLE) {
                return Ok(false.into());
            }
        }
    }
    Ok(true.into())
}

pub fn init(vm: &VmInner, op: Gc<Object>, fp: Gc<Object>, global: Gc<Object>) {
    let construct = Object::new_gc(
        vm,
        Some(fp),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct),
    );
    construct.raw_index_set_flags(vm, atom::constant::prototype, op, PropertyFlags::BUILTIN);

    let get_prototype_of = new_func(vm, fp, get_prototype_of, 1);
    construct.raw_index_set_flags(
        vm,
        atom::constant::getPrototypeOf,
        get_prototype_of,
        PropertyFlags::BUILTIN,
    );

    let is = new_func(vm, fp, is, 2);
    construct.raw_index_set_flags(vm, atom::constant::is, is, PropertyFlags::BUILTIN);

    let is_extensible = new_func(vm, fp, is_extensible, 1);
    construct.raw_index_set_flags(
        vm,
        atom::constant::isExtensible,
        is_extensible,
        PropertyFlags::BUILTIN,
    );

    let assign = new_func(vm, fp, assign, 2);
    construct.raw_index_set_flags(vm, atom::constant::assign, assign, PropertyFlags::BUILTIN);

    let create = new_func(vm, fp, create, 2);
    construct.raw_index_set_flags(vm, atom::constant::create, create, PropertyFlags::BUILTIN);

    let define_properties = new_func(vm, fp, define_properties, 2);
    construct.raw_index_set_flags(
        vm,
        atom::constant::defineProperties,
        define_properties,
        PropertyFlags::BUILTIN,
    );

    let define_property = new_func(vm, fp, define_property, 3);
    construct.raw_index_set_flags(
        vm,
        atom::constant::defineProperty,
        define_property,
        PropertyFlags::BUILTIN,
    );

    let freeze = new_func(vm, fp, freeze, 1);
    construct.raw_index_set_flags(vm, atom::constant::freeze, freeze, PropertyFlags::BUILTIN);

    let seal = new_func(vm, fp, seal, 1);
    construct.raw_index_set_flags(vm, atom::constant::seal, seal, PropertyFlags::BUILTIN);

    let is_frozen = new_func(vm, fp, is_frozen, 1);
    construct.raw_index_set_flags(
        vm,
        atom::constant::isFrozen,
        is_frozen,
        PropertyFlags::BUILTIN,
    );

    let is_sealed = new_func(vm, fp, is_sealed, 1);
    construct.raw_index_set_flags(
        vm,
        atom::constant::isSealed,
        is_sealed,
        PropertyFlags::BUILTIN,
    );

    construct.raw_index_set(vm, atom::constant::length, 1);
    op.raw_index_set_flags(
        vm,
        atom::constant::constructor,
        construct,
        PropertyFlags::BUILTIN,
    );

    let to_string = new_func(vm, fp, object_to_string, 0);
    op.raw_index_set_flags(
        vm,
        atom::constant::toString,
        to_string,
        PropertyFlags::BUILTIN,
    );

    global.raw_index_set(vm, atom::constant::Object, construct)
}
