use common::atom::{self, Atoms};
use dreck::{rebind, Owner, Root};

use crate::{
    object::{ObjectFlags, ObjectKind, Property, PropertyFlags},
    realm::{ExecutionContext, GcRealm},
    GcObject, Object, Realm, Value,
};

use super::new_func;

fn construct<'l, 'cell>(
    arena: &'l mut Root<'cell>,
    owner: &mut Owner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    let value = if let Some(x) = Realm::arg(realm, owner, 0) {
        Realm::is_falsish(realm, owner, x)
    } else {
        false
    };

    if ctx.new_target.is_undefined() {
        Ok(value.into())
    } else {
        let proto = if let Some(object) = ctx.new_target.into_object() {
            let res = rebind_try!(
                arena,
                Object::index(
                    object,
                    owner,
                    arena,
                    atoms,
                    realm,
                    atom::constant::prototype
                )
            );
            if let Some(res) = res.into_object() {
                rebind!(arena, res)
            } else {
                rebind!(arena, realm.borrow(owner).builtin.boolean_proto)
            }
        } else {
            rebind!(arena, realm.borrow(owner).builtin.boolean_proto)
        };

        let obj = Object::new_gc(
            arena,
            Some(proto),
            ObjectFlags::ORDINARY,
            ObjectKind::Boolean(value),
        );
        Ok(rebind!(arena, obj).into())
    }
}

fn bool_value<'l, 'cell>(
    arena: &'l Root<'cell>,
    owner: &mut Owner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    this: Value<'_, 'cell>,
) -> Result<bool, Value<'l, 'cell>> {
    if let Some(x) = this.into_bool() {
        return Ok(x);
    } else if let Some(obj) = this.into_object() {
        if let ObjectKind::Boolean(x) = unsafe { obj.borrow(owner).kind() } {
            return Ok(*x);
        }
    }
    return Err(Realm::create_type_error(
        realm,
        owner,
        arena,
        atoms,
        "tried to retrieve boolean value of an object which is neither bool nor a Boolean object",
    ));
}

fn value_of<'l, 'cell>(
    arena: &'l mut Root<'cell>,
    owner: &mut Owner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    bool_value(arena, owner, atoms, realm, ctx.this).map(|x| x.into())
}

fn to_string<'l, 'cell>(
    arena: &'l mut Root<'cell>,
    owner: &mut Owner<'cell>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'cell>,
    ctx: &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
    if rebind_try!(arena, bool_value(arena, owner, atoms, realm, ctx.this)) {
        Ok(arena.add("true".to_string()).into())
    } else {
        Ok(arena.add("false".to_string()).into())
    }
}

pub fn init<'l, 'cell>(
    owner: &mut Owner<'cell>,
    arena: &'l Root<'cell>,
    atoms: &Atoms,
    op: GcObject<'_, 'cell>,
    fp: GcObject<'_, 'cell>,
    global: GcObject<'_, 'cell>,
) -> GcObject<'l, 'cell> {
    let prototype = Object::new_gc(
        arena,
        Some(op),
        ObjectFlags::ORDINARY,
        ObjectKind::Boolean(false),
    );
    let construct = Object::new_gc(
        arena,
        Some(fp),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct),
    );
    Object::raw_index_set_prop(
        construct,
        owner,
        arena,
        atoms,
        Property::value(
            prototype.into(),
            PropertyFlags::empty(),
            atom::constant::prototype,
        ),
    );
    Object::raw_index_set(
        prototype,
        owner,
        arena,
        atoms,
        atom::constant::constructor,
        construct,
    );
    Object::raw_index_set(
        global,
        owner,
        arena,
        atoms,
        atom::constant::Boolean,
        construct,
    );

    let value_of = new_func(arena, owner, atoms, fp, value_of, 0);
    Object::raw_index_set(
        prototype,
        owner,
        arena,
        atoms,
        atom::constant::valueOf,
        value_of,
    );
    let to_string = new_func(arena, owner, atoms, fp, to_string, 0);
    Object::raw_index_set(
        prototype,
        owner,
        arena,
        atoms,
        atom::constant::toString,
        to_string,
    );

    prototype
}
