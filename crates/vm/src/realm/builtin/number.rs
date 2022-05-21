use common::atom::{self, Atoms};

use crate::{
    cell::CellOwner,
    gc::Arena,
    object::{ObjectFlags, ObjectKind},
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
    let value = if let Some(x) = realm.arg(owner, 0) {
        rebind_try!(arena, realm.to_number(owner, arena, atoms, x))
    } else {
        Value::from(0i32)
    };

    if ctx.new_target.is_undefined() {
        Ok(value)
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
    construct.raw_index_set(owner, arena, atoms, atom::constant::EPSILON, f64::EPSILON);
    construct.raw_index_set(owner, arena, atoms, atom::constant::MIN_VALUE, f64::MIN);
    construct.raw_index_set(owner, arena, atoms, atom::constant::MAX_VALUE, f64::MAX);
    construct.raw_index_set(owner, arena, atoms, atom::constant::NaN, f64::NAN);
    construct.raw_index_set(
        owner,
        arena,
        atoms,
        atom::constant::NEGATIVE_INFINITY,
        f64::NEG_INFINITY,
    );
    construct.raw_index_set(
        owner,
        arena,
        atoms,
        atom::constant::POSITIVE_INFINITY,
        f64::INFINITY,
    );
    global.raw_index_set(owner, arena, atoms, atom::constant::Number, construct);

    prototype
}
