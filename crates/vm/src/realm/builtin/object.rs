use crate::{
    atom::{self, Atoms},
    cell::CellOwner,
    gc::{Arena, Gc},
    object::{GcObject, Object, ObjectFlags, ObjectKind, Property, PropertyFlag, PropertyValue},
    realm::{ExecutionContext, GcRealm},
    rebind, rebind_try, Value,
};

use super::new_func;

fn construct<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    if ctx.new_target.is_undefined()
        || ctx
            .new_target
            .into_object()
            .map_or(false, |x| x.ptr_eq(ctx.function))
    {
        let value = if let Some(x) = realm.arg(owner, 0) {
            rebind!(arena, x)
        } else {
            let obj = Object::new_gc(
                arena,
                Some(realm.borrow(owner).builtin.object_proto),
                ObjectFlags::empty(),
                ObjectKind::Ordinary,
            );
            return Ok(obj.into());
        };
        if value.is_undefined() || value.is_null() {
            let obj = Object::new_gc(
                arena,
                Some(realm.borrow(owner).builtin.object_proto),
                ObjectFlags::empty(),
                ObjectKind::Ordinary,
            );

            return Ok(obj.into());
        }
        return Ok(realm.to_object(value)?.into());
    }

    let proto = if let Some(object) = ctx.new_target.into_object() {
        rebind!(
            arena,
            object.index(owner, arena, atoms, realm, atom::constant::prototype)
        )?
    } else {
        Value::undefined()
    };
    let proto = proto
        .into_object()
        .unwrap_or(rebind!(arena, realm.borrow(owner).builtin.object_proto));

    let object = Object::new_gc(
        arena,
        Some(proto),
        ObjectFlags::ORDINARY,
        ObjectKind::Ordinary,
    );
    Ok(object.into())
}

fn get_prototype_of<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    _atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let value = realm.arg(owner, 0).unwrap_or_else(Value::undefined);
    let object = rebind_try!(arena, realm.to_object(value));
    let res = object
        .borrow(owner)
        .prototype()
        .map(Value::from)
        .unwrap_or_else(Value::undefined);
    Ok(rebind!(arena, res))
}

fn is<'l, 'cell>(
    _arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    _atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    if let Some(a) = realm.arg(owner, 0) {
        if let Some(b) = realm.arg(owner, 1) {
            Ok(realm.strict_equal(owner, a, b).into())
        } else {
            Ok(a.is_undefined().into())
        }
    } else {
        Ok(true.into())
    }
}

fn is_extensible<'l, 'cell>(
    _arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    _atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let res = realm
        .arg(owner, 0)
        .and_then(|x| x.into_object())
        .map_or(false, |obj| {
            obj.borrow(owner).flags().contains(ObjectFlags::EXTENDABLE)
        });
    Ok(res.into())
}

fn assign<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let to = if let Some(arg) = realm.arg(owner, 0) {
        arg
    } else {
        return Ok(Value::undefined());
    };

    let to = rebind_try!(arena, realm.to_object(to));
    if realm.argc(owner) == 1 {
        return Ok(rebind!(arena, to).into());
    }

    let i = 1;
    while let Some(from) = realm.arg(owner, i) {
        let from = rebind_try!(arena, realm.to_object(from));

        let (b_from, b_to) = Gc::borrow_mut_2(owner, arena, from, to);
        b_from.elements.for_each(|idx, value| {
            // These are all elements so we can use raw as no getter can be triggered
            b_to.elements.set(idx, value);
        });
        // Need to just straight up clone since getters can add or remove object keys.
        let properties = from.borrow(owner).properties.clone_properties();
        for p in properties {
            let prop = if let Some(prop) = from.borrow(owner).properties.get(p.atom()) {
                prop
            } else {
                continue;
            };
            if !prop.is_enumerable() {
                continue;
            }

            let value = match prop.as_value() {
                PropertyValue::Accessor(_accessor) => {
                    todo!()
                }
                PropertyValue::Value(value) => value,
            };

            if let Some(p) = to.borrow_mut(owner, arena).properties.set(Property::value(
                value,
                PropertyFlag::ordinary(),
                p.atom(),
            )) {
                atoms.increment(p.atom());
            }
        }
    }
    Ok(rebind!(arena, to).into())
}

pub fn init<'l, 'cell>(
    owner: &mut CellOwner<'cell>,
    arena: &'l Arena<'_, 'cell>,
    atoms: &Atoms,
    op: GcObject<'_, 'cell>,
    fp: GcObject<'_, 'cell>,
    global: GcObject<'_, 'cell>,
) {
    let construct = Object::new_gc(
        arena,
        Some(fp),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct),
    );
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::prototype,
        op,
        PropertyFlag::BUILTIN,
    );
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::length,
        1,
        PropertyFlag::BUILTIN,
    );

    global.raw_index_set(owner, arena, atoms, atom::constant::Object, construct);

    let get_prototype_of = new_func(arena, owner, atoms, fp, get_prototype_of, 1);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::getPrototypeOf,
        get_prototype_of,
        PropertyFlag::BUILTIN,
    );

    let is = new_func(arena, owner, atoms, fp, is, 2);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::is,
        is,
        PropertyFlag::BUILTIN,
    );

    let is_extensible = new_func(arena, owner, atoms, fp, is_extensible, 1);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::isExtensible,
        is_extensible,
        PropertyFlag::BUILTIN,
    );

    let assign = new_func(arena, owner, atoms, fp, assign, 2);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::assign,
        assign,
        PropertyFlag::BUILTIN,
    );

    op.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::constructor,
        construct,
        PropertyFlag::BUILTIN,
    );
}
