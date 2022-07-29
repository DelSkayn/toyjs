use common::atom::{self, Atoms};
use dreck::{Gc, Owner, Root, rebind, root};

use crate::{
    object::{ObjectFlags, ObjectKind},
    realm::{ExecutionContext, GcRealm},
    GcObject, Object, Value,
};

#[repr(u8)]
pub enum ErrorType {
    Base,
    Syntax,
    Type,
}

pub fn construct<'l, 'own, const KIND: u8>(
    arena: &'l mut Root< 'own>,
    owner: &mut Owner<'own>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'own>,
    ctx: &ExecutionContext<'_, 'own>,
) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
    let new_target = if ctx.new_target.is_undefined() {
        ctx.function.into()
    } else {
        ctx.new_target
    };

    let proto = if let Some(obj) = new_target.into_object() {
        rebind_try!(
            arena,
            obj.index(owner, arena, atoms, realm, atom::constant::prototype)
        )
        .empty_to_undefined()
    } else {
        Value::undefined()
    };

    let proto = if let Some(proto) = proto.into_object() {
        proto
    } else {
        let error = if KIND == ErrorType::Base as u8 {
            realm.borrow(owner).builtin.error_proto
        } else if KIND == ErrorType::Syntax as u8 {
            realm.borrow(owner).builtin.syntax_error_proto
        } else if KIND == ErrorType::Type as u8 {
            realm.borrow(owner).builtin.type_error_proto
        } else {
            panic!("invalid error kind")
        };
        rebind!(arena, error)
    };
    root!(arena, proto);

    let (message, cause) = if let Some(message) = realm.arg(owner, 0) {
        let message = rebind_try!(arena, realm.to_string(owner, arena, atoms, message));
        let message = rebind!(arena, message);
        if let Some(options) = realm.arg(owner, 1).and_then(|x| x.into_object()) {
            root!(arena, message);
            let cause = rebind_try!(
                arena,
                options.index(owner, arena, atoms, realm, atom::constant::cause)
            );
            if cause.is_empty() {
                (Some(rebind!(arena, message)), None)
            } else {
                let cause = rebind!(arena, cause);
                let message = rebind!(arena, message);
                (Some(message), Some(cause))
            }
        } else {
            (Some(message), None)
        }
    } else {
        (None, None)
    };

    let res = create(arena, owner, atoms, proto, message, cause);
    Ok(res.into())
}

pub fn create<'l, 'own>(
    arena: &'l Root< 'own>,
    owner: &mut Owner<'own>,
    atoms: &Atoms,
    prototype: GcObject<'_, 'own>,
    message: Option<Gc<'_, 'own, String>>,
    cause: Option<Value<'_, 'own>>,
) -> GcObject<'l, 'own> {
    let res = Object::new_gc(
        arena,
        Some(prototype),
        ObjectFlags::ORDINARY,
        ObjectKind::Error,
    );
    if let Some(message) = message {
        res.raw_index_set(owner, arena, atoms, atom::constant::message, message)
    }
    if let Some(cause) = cause {
        res.raw_index_set(owner, arena, atoms, atom::constant::cause, cause)
    }
    res
}

pub fn to_string<'l, 'own>(
    arena: &'l mut Root< 'own>,
    owner: &mut Owner<'own>,
    atoms: &Atoms,
    realm: GcRealm<'_, 'own>,
    ctx: &ExecutionContext<'_, 'own>,
) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
    let this = ctx.this;
    let this = this
        .into_object()
        .ok_or_else(|| realm.create_type_error(owner, arena, atoms, "this is not an object"));
    let this = rebind_try!(arena, this);

    let name = rebind_try!(
        arena,
        this.index(owner, arena, atoms, realm, atom::constant::name)
    );
    let name = rebind!(arena, name);
    let name = if let Some(obj) = name.into_object() {
        root!(arena, obj);
        let name = rebind_try!(arena, realm.to_string(owner, arena, atoms, obj.into()));
        rebind!(arena, name)
    } else {
        realm.to_string_primitive(arena, atoms, name).unwrap()
    };
    root!(arena, name);

    let message = rebind_try!(
        arena,
        this.index(owner, arena, atoms, realm, atom::constant::message)
    );
    let message = rebind!(arena, message);
    let message = if let Some(obj) = message.into_object() {
        root!(arena, obj);
        let message = rebind_try!(arena, realm.to_string(owner, arena, atoms, obj.into()));
        rebind!(arena, message)
    } else {
        realm.to_string_primitive(arena, atoms, message).unwrap()
    };

    let res = name.borrow(owner).to_string() + ": " + message.borrow(owner).as_str();
    let res = arena.add(res);
    Ok(res.into())
}

pub fn init<'gc, 'own, const KIND: u8>(
    owner: &mut Owner<'own>,
    arena: &'gc Root< 'own>,
    atoms: &Atoms,
    name: Gc<'_, 'own, String>,
    construct_proto: GcObject<'_, 'own>,
    proto_proto: GcObject<'_, 'own>,
) -> (GcObject<'gc, 'own>, GcObject<'gc, 'own>) {
    let proto = Object::new_gc(
        arena,
        Some(proto_proto),
        ObjectFlags::empty(),
        ObjectKind::Ordinary,
    );

    let construct = Object::new_gc(
        arena,
        Some(construct_proto),
        ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR,
        ObjectKind::StaticFn(construct::<KIND>),
    );

    construct.raw_index_set(owner, arena, atoms, atom::constant::prototype, proto);
    construct.raw_index_set(owner, arena, atoms, atom::constant::constructor, construct);

    let message = arena.add(String::new());
    proto.raw_index_set(owner, arena, atoms, atom::constant::message, message);
    proto.raw_index_set(owner, arena, atoms, atom::constant::name, name);

    (construct, proto)
}
