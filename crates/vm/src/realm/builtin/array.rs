use crate::{
    atom,
    object::{Accessor, ObjectFlags, ObjectKind, Property, PropertyFlags},
    realm::ExecutionContext,
    Gc, Object, Realm, Value, VmInner,
};

use super::new_func;

pub fn construct(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    let proto = if let Some(object) = exec.new_target.into_object() {
        unsafe { object.index(realm, atom::constant::prototype)? }
    } else {
        Value::undefined()
    };
    let proto = proto.into_object().unwrap_or(realm.builtin.array_proto);

    if realm.stack.frame_size() == 0 {
        let res = Object::new_gc(
            realm.vm(),
            Some(proto),
            ObjectFlags::ORDINARY,
            ObjectKind::Array,
        );
        define_length(realm.vm(), realm.builtin.array_proto, res);
        return Ok(res.into());
    }

    let obj = Object::new_gc(
        realm.vm(),
        Some(proto),
        ObjectFlags::ORDINARY,
        ObjectKind::Array,
    );
    define_length(realm.vm(), realm.builtin.array_proto, obj);
    if realm.stack.frame_size() == 1 {
        let len = realm.stack.read_arg(0).unwrap();
        if len.is_number() {
            realm.stack.push_temp(obj.into());
            let len = unsafe { realm.to_uint32(len)? };
            // TODO SaveValueZero
            obj.elements.set_len(len as usize);
        } else {
            obj.elements.set(0, len);
        }
        return Ok(obj.into());
    }

    obj.elements.set_len(realm.stack.frame_size());

    let mut i = 0;
    while let Some(value) = realm.stack.read_arg(i) {
        obj.elements.set(i.into(), value);
        i = i.checked_add(1).unwrap();
    }
    Ok(obj.into())
}

fn get_length(_: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    if let Some(this) = exec.this.into_object() {
        Ok((this.elements.len().min(u32::MAX as usize) as i32).into())
    } else {
        Ok(0.into())
    }
}

fn set_length(realm: &Realm, exec: &mut ExecutionContext) -> Result<Value, Value> {
    if let Some(this) = exec.this.into_object() {
        let len = realm.stack.read_arg(0).unwrap_or_else(Value::undefined);
        let len = unsafe { realm.to_uint32(len)? };
        this.elements.set_len(len as usize);
    }
    Ok(Value::undefined())
}

pub fn define_length(vm: &VmInner, fp: Gc<Object>, object: Gc<Object>) {
    let get_length = new_func(vm, fp, get_length, 0);
    let set_length = new_func(vm, fp, set_length, 1);
    let prop = Property::accessor(
        Accessor {
            get: Some(get_length),
            set: Some(set_length),
        },
        PropertyFlags::empty(),
        atom::constant::length,
    );
    object.raw_index_set_prop(vm, prop);
}

pub fn init(vm: &VmInner, op: Gc<Object>, fp: Gc<Object>, global: Gc<Object>) -> Gc<Object> {
    let array_proto = Object::new_gc(vm, Some(op), ObjectFlags::empty(), ObjectKind::Array);
    array_proto.raw_index_set_flags(vm, atom::constant::length, 0, PropertyFlags::BUILTIN);

    define_length(vm, fp, array_proto);

    let array_constructor = Object::new_gc(
        vm,
        Some(fp),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct),
    );
    array_constructor.raw_index_set_flags(
        vm,
        atom::constant::prototype,
        array_proto,
        PropertyFlags::BUILTIN,
    );

    array_proto.raw_index_set_flags(
        vm,
        atom::constant::constructor,
        array_constructor,
        PropertyFlags::BUILTIN,
    );

    global.raw_index_set_flags(
        vm,
        atom::constant::Array,
        array_constructor,
        PropertyFlags::BUILTIN,
    );

    array_proto
}
