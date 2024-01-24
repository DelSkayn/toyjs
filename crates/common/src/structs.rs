use crate::{
    interner::Interner,
    number::{Number, NumberId},
    string::{String, StringId},
};

#[derive(Clone, Default)]
pub struct Interners {
    pub numbers: Interner<Number, NumberId>,
    pub strings: Interner<String, StringId>,
}
