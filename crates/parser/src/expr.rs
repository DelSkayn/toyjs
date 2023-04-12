use ast::{Expr, NodeId};

use crate::{Parser, Result};

impl<'a> Parser<'a> {
    pub(crate) fn parse_expr(&mut self) -> Result<NodeId<Expr>> {
        todo!()
    }

    pub(crate) fn parse_assignment_expr(&mut self) -> Result<NodeId<Expr>> {
        todo!()
    }
}
