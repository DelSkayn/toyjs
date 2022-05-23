use common::atom::{self, Atoms};

use crate::{
    cell::CellOwner,
    gc::Arena,
    object::{Accessor, ObjectFlags, ObjectKind, Property, PropertyFlags},
    realm::{ExecutionContext, GcRealm},
    rebind, rebind_try, root, GcObject, Object, Value,
};

use super::new_func;

fn construct<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let proto = if let Some(object) = ctx.new_target.into_object() {
        rebind_try!(
            arena,
            object.index(owner, arena, atoms, realm, atom::constant::prototype)
        )
    } else {
        Value::undefined()
    };
    let proto = rebind!(arena, proto);
    let proto = proto
        .into_object()
        .unwrap_or(rebind!(arena, realm.borrow(owner).builtin.array_proto));

    if realm.argc(owner) > 0 {
        let obj = Object::new_gc(arena, Some(proto), ObjectFlags::ORDINARY, ObjectKind::Array);
        define_length(
            owner,
            arena,
            atoms,
            realm.borrow(owner).builtin.array_proto,
            obj,
        );
        if realm.argc(owner) == 1 {
            root!(arena, obj);
            let len = realm.arg(owner, 0).unwrap();
            if len.is_number() {
                let len = rebind_try!(arena, realm.to_uint32(owner, arena, atoms, len));
                // TODO SaveValueZero
                unsafe {
                    // Safe because no new pointers are added into elements
                    obj.unsafe_borrow_mut(owner).elements.set_len(len as usize);
                }
            } else {
                obj.borrow_mut(owner, arena).elements.set(0, len);
            }
            return Ok(rebind!(arena, obj).into());
        }

        unsafe {
            let len = realm.argc(owner);
            // Safe because no new pointers are added into elements
            obj.unsafe_borrow_mut(owner)
                .elements
                .set_len(len.try_into().unwrap());
        }

        let mut i = 0;
        while let Some(value) = realm.arg(owner, i) {
            obj.borrow_mut(owner, arena)
                .elements
                .set(i.try_into().unwrap(), value);
            i = i.checked_add(1).unwrap();
        }
        Ok(rebind!(arena, obj).into())
    } else {
        let res = Object::new_gc(arena, Some(proto), ObjectFlags::ORDINARY, ObjectKind::Array);
        let fp = realm.borrow(owner).builtin.function_proto;
        define_length(owner, arena, atoms, fp, res);
        Ok(res.into())
    }
}

fn get_length<'l, 'cell>(
    _arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    _atoms: &Atoms,
    _realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    if let Some(this) = ctx.this.into_object() {
        Ok((this.borrow(owner).elements.len().min(u32::MAX as usize) as i32).into())
    } else {
        Ok(0.into())
    }
}

fn set_length<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    if let Some(this) = ctx.this.into_object() {
        let len = realm.arg(owner, 0).unwrap_or_else(Value::undefined);
        let len = realm.to_uint32(owner, arena, atoms, len)?;
        unsafe {
            // Safe because no new pointers are inserted into the object
            this.unsafe_borrow_mut(owner).elements.set_len(len as usize);
        }
    }
    Ok(Value::undefined())
}

pub fn define_length<'l, 'cell>(
    owner: &mut CellOwner<'cell>,
    arena: &'l Arena<'_, 'cell>,
    atoms: &Atoms,
    fp: GcObject<'_, 'cell>,
    object: GcObject<'_, 'cell>,
) {
    let get_length = new_func(arena, owner, atoms, fp, get_length, 0);
    let set_length = new_func(arena, owner, atoms, fp, set_length, 1);
    let prop = Property::accessor(
        Accessor {
            get: Some(get_length),
            set: Some(set_length),
        },
        PropertyFlags::WRITABLE,
        atom::constant::length,
    );
    object.raw_index_set_prop(owner, arena, atoms, prop);
}

pub fn init<'l, 'cell>(
    owner: &mut CellOwner<'cell>,
    arena: &'l Arena<'_, 'cell>,
    atoms: &Atoms,
    op: GcObject<'_, 'cell>,
    fp: GcObject<'_, 'cell>,
    global: GcObject<'_, 'cell>,
) -> GcObject<'l, 'cell> {
    let array_proto = Object::new_gc(arena, Some(op), ObjectFlags::empty(), ObjectKind::Array);
    array_proto.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::length,
        0,
        PropertyFlags::BUILTIN,
    );

    define_length(owner, arena, atoms, fp, array_proto);

    let array_constructor = Object::new_gc(
        arena,
        Some(fp),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct),
    );
    array_constructor.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::prototype,
        array_proto,
        PropertyFlags::BUILTIN,
    );

    array_proto.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::constructor,
        array_constructor,
        PropertyFlags::BUILTIN,
    );

    global.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::Array,
        array_constructor,
        PropertyFlags::BUILTIN,
    );

    array_proto
}
