use common::id;
use token::{NumberId, StringId};

id!(pub struct ConstantId(u32));

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum Constant {
    Number(NumberId),
    String(StringId),
    Boolean(bool),
    Null,
    Undefined,
}
