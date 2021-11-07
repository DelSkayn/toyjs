use crate::{function::Function, object::Object, Realm, Value};

pub unsafe fn log(realm: &mut Realm) -> Value {
    let arg = realm.stack.read_arg(0);
    if arg.is_string() {
        println!("{}", *arg.unsafe_cast_string())
    } else if arg.is_int() {
        println!("{}", arg.cast_int())
    } else if arg.is_float() {
        println!("{}", arg.cast_float())
    }

    Value::undefined()
}

pub unsafe fn defer(realm: &mut Realm) -> Value {
    let arg = realm.stack.read_arg(0);
    if arg.is_function() {
        realm.push_task(arg.unsafe_cast_function())
    }
    Value::undefined()
}

pub unsafe fn initialize(realm: &mut Realm) {
    let console = realm.gc.allocate(Object::new());

    let log = realm.gc.allocate(Function::from_native(|realm| log(realm)));
    console.unsafe_index_set(
        Value::from(realm.gc.allocate("log".to_string())),
        Value::from(log),
        realm,
    );
    let global = realm.global;
    global.unsafe_index_set(
        Value::from(realm.gc.allocate("console".to_string())),
        Value::from(console),
        realm,
    );
    let defer = realm
        .gc
        .allocate(Function::from_native(|realm| defer(realm)));
    global.unsafe_index_set(
        Value::from(realm.gc.allocate("defer".to_string())),
        Value::from(defer),
        realm,
    );

    let name = realm.interner.intern("console");
    let defer = realm.interner.intern("defer");
    realm.symbol_table.define_global(name);
    realm.symbol_table.define_global(defer);
}
