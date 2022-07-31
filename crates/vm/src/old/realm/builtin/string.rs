use common::atom::{self, Atoms};
use dreck::{Owner, Root, rebind, root};

use crate::{
    object::{ObjectFlags, ObjectKind},
    realm::{ExecutionContext, GcRealm},
     GcObject, Object, Value,
};

use super::new_func;

fn construct<'l, 'own>(
    arena: &'l mut Root< 'own>,
    owner: &mut Owner<'own>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'own>,
    ctx: &ExecutionContext<'_, 'own>,
) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
    if ctx.new_target.is_undefined() {
        if let Some(x) = realm.arg(owner, 0) {
            let res = rebind_try!(arena, realm.to_string(owner, arena, atoms, x));
            Ok(rebind!(arena, res).into())
        } else {
            return Ok(arena.add(String::new()).into());
        }
    } else {
        let proto = if let Some(object) = ctx.new_target.into_object() {
            let res = rebind_try!(
                arena,
                object.index(owner, arena, atoms, realm, atom::constant::prototype)
            );
            rebind!(arena, res)
        } else {
            Value::undefined()
        };
        let proto = proto
            .into_object()
            .unwrap_or(rebind!(arena, realm.borrow(owner).builtin.string_proto));

        root!(arena, proto);
        let str = if let Some(x) = realm.arg(owner, 0) {
            let str = rebind_try!(arena, realm.to_string(owner, arena, atoms, x));
            rebind!(arena, str)
        } else {
            arena.add(String::new())
        };

        let obj = Object::new_gc(
            arena,
            Some(proto),
            ObjectFlags::ORDINARY,
            ObjectKind::String(str),
        );
        Ok(rebind!(arena, obj).into())
    }
}

fn to_string<'l, 'own>(
    arena: &'l mut Root< 'own>,
    owner: &mut Owner<'own>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'own>,
    ctx: &ExecutionContext<'_, 'own>,
) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
    if ctx.this.is_string() {
        return Ok(rebind!(arena, ctx.this));
    }
    if let Some(obj) = ctx.this.into_object() {
        if let ObjectKind::String(x) = unsafe { obj.borrow(owner).kind() } {
            return Ok(rebind!(arena, *x).into());
        }
    }

    return Err(realm.create_type_error(
        owner,
        arena,
        atoms,
        "String.prototype.toString called on non string object",
    ));
}

pub fn init<'l, 'own>(
    owner: &mut Owner<'own>,
    arena: &'l Root< 'own>,
    atoms: &Atoms,
    op: GcObject<'_, 'own>,
    fp: GcObject<'_, 'own>,
    global: GcObject<'_, 'own>,
) -> GcObject<'l, 'own> {
    let str = arena.add(String::new());
    let prototype = Object::new_gc(
        arena,
        Some(op),
        ObjectFlags::ORDINARY,
        ObjectKind::String(str),
    );
    let construct = Object::new_gc(
        arena,
        Some(fp),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct),
    );
    construct.raw_index_set(owner, arena, atoms, atom::constant::prototype, prototype);
    global.raw_index_set(owner, arena, atoms, atom::constant::String, construct);

    let to_string = new_func(arena, owner, atoms, fp, to_string, 0);
    prototype.raw_index_set(owner, arena, atoms, atom::constant::toString, to_string);
    prototype.raw_index_set(owner, arena, atoms, atom::constant::valueOf, to_string);

    prototype
}
