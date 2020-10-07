pub struct Environment {
    parent: Option<Gc<Environment>>,
    slots: Box<[JSValue]>,
}
