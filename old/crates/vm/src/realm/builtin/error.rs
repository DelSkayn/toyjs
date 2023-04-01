use dreck::{rebind, rebind_try, root, Gc, Owner, Root};

use crate::{
    atom,
    exec::ExecutionContext,
    object::{GcObject, Object, ObjectFlags, ObjectKind, Property, PropertyFlags},
    value::Value,
};

use super::BuiltinBuilder;

#[repr(u8)]
pub enum ErrorType {
    Base,
    Syntax,
    Type,
}

pub fn construct<'r, 'l, 'own, const KIND: u8>(
    exec: &'r mut ExecutionContext<'_, 'own>,
) -> Result<Value<'r, 'own>, Value<'r, 'own>> {
    let new_target = if exec.new_target.is_undefined() {
        exec.function.into()
    } else {
        exec.new_target
    };

    let proto = if let Some(obj) = new_target.into_object() {
        rebind_try!(
            exec.root,
            Object::index(obj, exec, atom::constants::prototype())
        )
        .empty_to_undefined()
    } else {
        Value::undefined()
    };

    let proto = if let Some(proto) = proto.into_object() {
        proto
    } else {
        let error = if KIND == ErrorType::Base as u8 {
            exec.realm.borrow(exec.owner).builtin.error_proto
        } else if KIND == ErrorType::Syntax as u8 {
            exec.realm.borrow(exec.owner).builtin.syntax_error_proto
        } else if KIND == ErrorType::Type as u8 {
            exec.realm.borrow(exec.owner).builtin.type_error_proto
        } else {
            panic!("invalid error kind")
        };
        rebind!(exec.root, error)
    };
    root!(exec.root, proto);

    let (message, cause) = if let Some(message) = exec.arg(0) {
        let message = rebind_try!(exec.root, exec.to_string(message));
        let message = rebind!(exec.root, message);
        if let Some(options) = exec.arg(1).and_then(|x| x.into_object()) {
            root!(exec.root, message);
            let cause = rebind_try!(
                exec.root,
                Object::index(options, exec, atom::constants::cause())
            );
            if cause.is_empty() {
                (Some(rebind!(exec.root, message)), None)
            } else {
                let cause = rebind!(exec.root, cause);
                let message = rebind!(exec.root, message);
                (Some(message), Some(cause))
            }
        } else {
            (Some(message), None)
        }
    } else {
        (None, None)
    };

    let res = create(exec.owner, exec.root, proto, message, cause);
    Ok(res.into())
}

pub fn create<'l, 'own>(
    owner: &mut Owner<'own>,
    root: &'l Root<'own>,
    prototype: GcObject<'_, 'own>,
    message: Option<Gc<'_, 'own, String>>,
    cause: Option<Value<'_, 'own>>,
) -> GcObject<'l, 'own> {
    let res = Object::new_gc(
        root,
        Some(prototype),
        ObjectFlags::ORDINARY,
        ObjectKind::Error,
    );
    if let Some(message) = message {
        let prop = Property::value(
            message.into(),
            PropertyFlags::BUILTIN,
            atom::constants::message(),
        );
        res.borrow_mut(owner, root).properties.set(prop);
    }
    if let Some(cause) = cause {
        let prop = Property::value(
            cause.into(),
            PropertyFlags::BUILTIN,
            atom::constants::cause(),
        );
        res.borrow_mut(owner, root).properties.set(prop);
    }
    res
}

pub fn to_string<'r, 'l, 'own>(
    exec: &'r mut ExecutionContext<'_, 'own>,
) -> Result<Value<'r, 'own>, Value<'r, 'own>> {
    let this = exec.this;
    let this = if let Some(this) = this.into_object() {
        this
    } else {
        return Err(exec.type_error("this is not an object"));
    };

    let name = rebind_try!(
        exec.root,
        Object::index(this, exec, atom::constants::name())
    );
    let name = rebind!(exec.root, name);
    let name = if let Some(obj) = name.into_object() {
        root!(exec.root, obj);
        let name = rebind_try!(exec.root, exec.to_string(obj.into()));
        rebind!(exec.root, name)
    } else {
        ExecutionContext::to_string_primitive(exec.root, name).unwrap()
    };
    root!(exec.root, name);

    let message = rebind_try!(
        exec.root,
        Object::index(this, exec, atom::constants::message())
    );
    let message = rebind!(exec.root, message);
    let message = if let Some(obj) = message.into_object() {
        root!(exec.root, obj);
        let message = rebind_try!(exec.root, exec.to_string(obj.into()));
        rebind!(exec.root, message)
    } else {
        ExecutionContext::to_string_primitive(exec.root, message).unwrap()
    };

    let res = name.borrow(exec.owner).to_string() + ": " + message.borrow(exec.owner).as_str();
    let res = exec.root.add(res);
    Ok(res.into())
}

pub fn init<'gc, 'own, const KIND: u8>(
    obj_proto: GcObject<'gc, 'own>,
    construct_proto: GcObject<'gc, 'own>,
    name: Gc<'gc, 'own, String>,
    builder: &mut BuiltinBuilder<'gc, 'own>,
) -> (GcObject<'gc, 'own>, GcObject<'gc, 'own>) {
    let message = builder.string(String::new());

    let proto = builder
        .object()
        .prototype(obj_proto)
        .props()
        .set(atom::constants::message, message)
        .set(atom::constants::name, name)
        .build();

    let construct = builder
        .object()
        .prototype(construct_proto)
        .flags(ObjectFlags::ORDINARY | ObjectFlags::CONSTRUCTOR)
        .kind(ObjectKind::StaticFn(construct::<KIND>))
        .props()
        .set(atom::constants::prototype, proto)
        .build();

    builder
        .prop(proto)
        .set(atom::constants::constructor, construct);

    (construct, proto)
}
