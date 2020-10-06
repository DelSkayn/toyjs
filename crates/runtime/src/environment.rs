pub struct Environment {
    parent: Gc<Environment>,
    slots: Box<[JSValue]>,
}
