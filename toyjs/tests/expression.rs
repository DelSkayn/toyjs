use toyjs::{ToyJs, Value};

#[test]
fn expression() {
    let mut toyjs = ToyJs::new();
    assert_eq!(toyjs.exec("1 + 1").unwrap(), Value::Int(2))
}

#[test]
fn arithmentic_operations() {
    const EXPRESSION: &str = "((2 + 2) ** 3 / 100 - 5 ** 3 * -1000) ** 2 + 100 - 8;";
    let mut toyjs = ToyJs::new();
    assert_eq!(
        toyjs.exec(EXPRESSION).unwrap(),
        Value::Float(15625160092.4096)
    )
}
