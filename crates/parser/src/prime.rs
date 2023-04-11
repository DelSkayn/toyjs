use ast::{NodeId, PrimeExpr};

use crate::Parser;

impl<'a> Parser<'a> {
    pub fn parse_prime(&mut self) -> Result<NodeId<PrimeExpr>, ()> {
        todo!()
    }
}
