use crate::{
    atom,
    object::{ObjectFlags, ObjectKind},
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
            return Ok(realm.strict_equal(a, b).into());
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

pub unsafe fn init(vm: &VmInner, op: Gc<Object>, fp: Gc<Object>, global: Gc<Object>) {
    let construct = Object::new_gc(
        vm,
        Some(fp),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct),
    );
    construct.raw_index_set(vm, atom::constant::prototype, op.into());

    let get_prototype_of = new_func(vm, fp, get_prototype_of);
    construct.raw_index_set(vm, atom::constant::getPrototypeOf, get_prototype_of.into());

    let is = new_func(vm, fp, is);
    construct.raw_index_set(vm, atom::constant::is, is.into());

    let is_extensible = new_func(vm, fp, is_extensible);
    construct.raw_index_set(vm, atom::constant::isExtensible, is_extensible.into());

    construct.raw_index_set(vm, atom::constant::length, 1.into());
    op.raw_index_set(vm, atom::constant::constructor, construct.into());

    global.raw_index_set(vm, atom::constant::Object, construct.into())
}
