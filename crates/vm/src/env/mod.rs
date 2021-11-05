use crate::{function::Function, object::Object, JSValue, Realm};

pub unsafe fn log(realm: &mut Realm) -> JSValue {
    let arg = realm.stack.read_arg(0);
    if arg.is_string() {
        println!("{}", *arg.into_string())
    } else if arg.is_int() {
        println!("{}", arg.into_int())
    } else if arg.is_float() {
        println!("{}", arg.into_float())
    }

    JSValue::undefined()
}

pub unsafe fn initialize(realm: &mut Realm) {
    let console = realm.gc.allocate(Object::new());

    let log = realm.gc.allocate(Function::from_native(|realm| log(realm)));
    console.index_set(
        JSValue::from(realm.gc.allocate("log".to_string())),
        JSValue::from(log),
        realm,
    );
    let global = realm.global;
    global.index_set(
        JSValue::from(realm.gc.allocate("console".to_string())),
        JSValue::from(console),
        realm,
    );

    let name = realm.interner.intern("console");
    realm.symbol_table.define_global(name);
}
