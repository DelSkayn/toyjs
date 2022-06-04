use common::atom::{self, Atoms};

use crate::{
    cell::CellOwner,
    gc::Arena,
    object::{ObjectFlags, ObjectKind, Property, PropertyFlags},
    realm::{ExecutionContext, GcRealm},
    rebind, rebind_try, GcObject, Object, Value,
};

use super::new_func;

fn construct<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let value = if let Some(x) = realm.arg(owner, 0) {
        rebind_try!(arena, realm.to_number(owner, arena, atoms, x))
    } else {
        Value::from(0i32)
    };

    if ctx.new_target.is_undefined() {
        Ok(rebind!(arena, value))
    } else {
        let proto = if let Some(object) = ctx.new_target.into_object() {
            let res = rebind_try!(
                arena,
                object.index(owner, arena, atoms, realm, atom::constant::prototype)
            );
            if let Some(res) = res.into_object() {
                rebind!(arena, res)
            } else {
                rebind!(arena, realm.borrow(owner).builtin.number_proto)
            }
        } else {
            rebind!(arena, realm.borrow(owner).builtin.number_proto)
        };

        let value = if let Some(x) = value.into_int() {
            x as f64
        } else {
            value.into_float().unwrap()
        };
        let obj = Object::new_gc(
            arena,
            Some(proto),
            ObjectFlags::ORDINARY,
            ObjectKind::Number(value),
        );
        Ok(rebind!(arena, obj).into())
    }
}

fn number_value<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    this: Value<'_, 'cell>,
) -> Result<Value<'static, 'cell>, Value<'l, 'cell>> {
    if let Some(x) = this.into_int() {
        return Ok(x.into());
    }
    if let Some(x) = this.into_float() {
        return Ok(x.into());
    }
    if let Some(obj) = this.into_object() {
        if let ObjectKind::Number(x) = unsafe { obj.borrow(owner).kind() } {
            return Ok((*x).into());
        }
    }
    return Err(realm.create_type_error(
        owner,
        arena,
        atoms,
        "tried to retrieve number value of an object which is neither number nor a Number object",
    ));
}

fn value_of<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    rebind!(arena, number_value(arena, owner, atoms, realm, ctx.this))
}

fn to_string<'l, 'cell>(
    arena: &'l mut Arena<'_, 'cell>,
    owner: &mut CellOwner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let radix = if let Some(x) = realm.arg(owner, 0) {
        let radix = rebind_try!(arena, realm.to_integer_or_infinity(owner, arena, atoms, x));
        if !(2.0..=36.0).contains(&radix) {
            //TODO: switch to range error
            return Err(realm.create_type_error(
                owner,
                arena,
                atoms,
                format!("{} is not a valid radix", radix),
            ));
        }
        radix as i32
    } else {
        10
    };

    let value = rebind_try!(arena, number_value(arena, owner, atoms, realm, ctx.this));
    if radix == 10 {
        realm
            .to_string(owner, arena, atoms, value)
            .map(|x| x.into())
    } else {
        let value = if let Some(x) = value.into_int() {
            x as f64
        } else {
            value.into_float().unwrap()
        };
        let str = GcRealm::to_string_radix(value, radix as u8);
        Ok(arena.add(str).into())
    }
}

pub fn init<'l, 'cell>(
    owner: &mut CellOwner<'cell>,
    arena: &'l Arena<'_, 'cell>,
    atoms: &Atoms,
    op: GcObject<'_, 'cell>,
    fp: GcObject<'_, 'cell>,
    global: GcObject<'_, 'cell>,
) -> GcObject<'l, 'cell> {
    let prototype = Object::new_gc(
        arena,
        Some(op),
        ObjectFlags::ORDINARY,
        ObjectKind::Number(0.0),
    );
    let construct = Object::new_gc(
        arena,
        Some(fp),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct),
    );
    construct.raw_index_set(owner, arena, atoms, atom::constant::prototype, prototype);
    prototype.raw_index_set(owner, arena, atoms, atom::constant::constructor, construct);
    construct.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(
            f64::EPSILON.into(),
            PropertyFlags::empty(),
            atom::constant::EPSILON,
        ),
    );
    construct.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(
            f64::MIN.into(),
            PropertyFlags::empty(),
            atom::constant::MIN_VALUE,
        ),
    );
    construct.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(
            f64::MAX.into(),
            PropertyFlags::empty(),
            atom::constant::MAX_VALUE,
        ),
    );
    construct.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(
            (((1u64 << 52) - 1) as f64).into(),
            PropertyFlags::empty(),
            atom::constant::MAX_SAFE_INTEGER,
        ),
    );
    construct.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(
            (-((1i64 << 52) - 1) as f64).into(),
            PropertyFlags::empty(),
            atom::constant::MIN_SAFE_INTEGER,
        ),
    );
    construct.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(f64::NAN.into(), PropertyFlags::empty(), atom::constant::NaN),
    );
    construct.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(
            f64::NEG_INFINITY.into(),
            PropertyFlags::empty(),
            atom::constant::NEGATIVE_INFINITY,
        ),
    );
    construct.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(
            f64::INFINITY.into(),
            PropertyFlags::empty(),
            atom::constant::POSITIVE_INFINITY,
        ),
    );
    global.raw_index_set(owner, arena, atoms, atom::constant::Number, construct);
    let value_of = new_func(arena, owner, atoms, fp, value_of, 0);
    prototype.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(
            value_of.into(),
            PropertyFlags::BUILTIN,
            atom::constant::valueOf,
        ),
    );
    let to_string = new_func(arena, owner, atoms, fp, to_string, 0);
    prototype.raw_index_set_prop(
        owner,
        arena,
        atoms,
        Property::value(
            to_string.into(),
            PropertyFlags::BUILTIN,
            atom::constant::toString,
        ),
    );

    prototype
}
