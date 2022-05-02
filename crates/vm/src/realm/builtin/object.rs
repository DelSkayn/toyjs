use crate::{
    atom::{self, Atoms},
    cell::CellOwner,
    gc::{Arena, Gc},
    object::{GcObject, Object, ObjectFlags, ObjectKind, Property, PropertyFlag, PropertyValue},
    realm::{ExecutionContext, GcRealm},
    rebind, rebind_try, root, Value,
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
                PropertyValue::Accessor(accessor) => {
                    if let Some(get) = accessor.get {
                        let v =
                            rebind_try!(arena, realm.method_call(owner, arena, atoms, get, from));
                        rebind!(arena, v)
                    } else {
                        continue;
                    }
                }
                PropertyValue::Value(value) => {
                    rebind!(arena, value)
                }
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

fn define_prop<'l, 'cell>(
    owner: &mut CellOwner<'cell>,
    arena: &'l mut Arena<'_, 'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    from: GcObject<'_, 'cell>,
    to: GcObject<'_, 'cell>,
) -> Result<(), Value<'l, 'cell>> {
    let prorties = from.borrow(owner).properties.clone_properties();
    for p in prorties {
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
        let prop = Property::from_value(owner, arena, atoms, realm, value, p.atom());
        let prop = rebind!(arena, rebind_try!(arena, prop));
        to.raw_index_set_prop(owner, arena, atoms, prop);
    }
    Ok(())
}

fn create<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let arg = realm.arg(owner, 0).ok_or_else(|| {
        realm.create_type_error(arena, "Object.create requires atleast a single argument")
    });
    let arg = rebind_try!(arena, arg);
    if !arg.is_object() && !arg.is_null() {
        return Err(realm.create_type_error(arena, "argument is not a object or null"));
    }

    let obj = Object::new_gc(
        arena,
        arg.into_object(),
        ObjectFlags::ORDINARY,
        ObjectKind::Ordinary,
    );
    if realm.argc(owner) == 1 {
        return Ok(rebind!(arena, obj).into());
    }

    let from = realm.arg(owner, 1).unwrap();
    let from = rebind_try!(arena, realm.to_object(from));

    root!(arena, obj);

    rebind_try!(arena, define_prop(owner, arena, atoms, realm, from, obj));

    let obj = rebind!(arena, obj);
    Ok(obj.into())
}

fn define_properties<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    if realm.argc(&owner) < 2 {
        return Err(realm.create_type_error(
            arena,
            "Object.defineProperties requires atleast a two arguments",
        ));
    }

    let obj = realm.arg(owner, 0).unwrap();
    let prop = realm.arg(owner, 1).unwrap();

    let obj = rebind_try!(arena, realm.to_object(obj));
    let prop = rebind_try!(arena, realm.to_object(prop));

    rebind_try!(arena, define_prop(owner, arena, atoms, realm, prop, obj));

    Ok(rebind!(arena, obj).into())
}

fn define_property<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let obj = realm.arg(owner, 0).unwrap_or_else(Value::undefined);
    let obj = obj
        .into_object()
        .ok_or_else(|| realm.create_type_error(arena, "can't define property on non object value"));
    let obj = rebind_try!(arena, obj);
    root!(arena, obj);

    let p = realm.arg(owner, 1).unwrap_or_else(Value::undefined);
    let p = rebind_try!(arena, realm.to_primitive(owner, arena, atoms, p, true));
    let p = atoms.atomize_primitive(owner, p).unwrap();

    let prop = realm.arg(owner, 2).unwrap_or_else(Value::undefined);
    let prop = rebind_try!(
        arena,
        Property::from_value(owner, arena, atoms, realm, prop, p)
    );
    let prop = rebind!(arena, prop);
    if let Some(prop) = obj.borrow_mut(owner, &arena).properties.set(prop) {
        atoms.increment(prop.atom())
    }
    Ok(rebind!(arena, obj).into())
}

fn freeze<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    _atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let v = realm.arg(owner, 0).unwrap_or_else(Value::undefined);
    if let Some(obj) = v.into_object() {
        if obj.borrow(owner).flags().contains(ObjectFlags::EXTENDABLE) {
            unsafe { obj.unsafe_borrow_mut(owner).properties.freeze() }
        } else {
            return Ok(realm.create_type_error(arena, "Cannot freeze unextendable object"));
        }
    }
    Ok(rebind!(arena, v))
}

fn seal<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    _atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let v = realm.arg(owner, 0).unwrap_or_else(Value::undefined);
    if let Some(obj) = v.into_object() {
        if obj.borrow(owner).flags().contains(ObjectFlags::EXTENDABLE) {
            unsafe { obj.unsafe_borrow_mut(owner).properties.seal() }
        } else {
            return Ok(realm.create_type_error(arena, "Cannot seal unextendable object"));
        }
    }
    Ok(rebind!(arena, v))
}

fn is_frozen<'l, 'cell>(
    _arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    _atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let o = realm.arg(owner, 0).unwrap_or_else(Value::undefined);
    if let Some(o) = o.into_object() {
        if !o.borrow(owner).flags().contains(ObjectFlags::EXTENDABLE) {
            return Ok(false.into());
        }
        return Ok(o.borrow(owner).properties.is_frozen().into());
    }
    Ok(true.into())
}

fn is_sealed<'l, 'cell>(
    _arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    _atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    _ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let o = realm.arg(owner, 0).unwrap_or_else(Value::undefined);
    if let Some(o) = o.into_object() {
        if !o.borrow(owner).flags().contains(ObjectFlags::EXTENDABLE) {
            return Ok(false.into());
        }
        return Ok(o.borrow(owner).properties.is_sealed().into());
    }
    Ok(true.into())
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

    let create = new_func(arena, owner, atoms, fp, create, 2);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::create,
        create,
        PropertyFlag::BUILTIN,
    );

    let define_properties = new_func(arena, owner, atoms, fp, define_properties, 2);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::defineProperties,
        define_properties,
        PropertyFlag::BUILTIN,
    );

    let define_property = new_func(arena, owner, atoms, fp, define_property, 3);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::defineProperty,
        define_property,
        PropertyFlag::BUILTIN,
    );

    let freeze = new_func(arena, owner, atoms, fp, freeze, 1);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::freeze,
        freeze,
        PropertyFlag::BUILTIN,
    );

    let seal = new_func(arena, owner, atoms, fp, seal, 1);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::seal,
        seal,
        PropertyFlag::BUILTIN,
    );

    let is_sealed = new_func(arena, owner, atoms, fp, is_sealed, 1);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::isSealed,
        is_sealed,
        PropertyFlag::BUILTIN,
    );

    let is_frozen = new_func(arena, owner, atoms, fp, is_frozen, 1);
    construct.raw_index_set_flags(
        owner,
        arena,
        atoms,
        atom::constant::isFrozen,
        is_frozen,
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
