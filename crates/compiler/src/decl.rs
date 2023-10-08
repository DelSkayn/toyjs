use ast::NodeId;
use bc::Reg;

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn binding_placement(
        &mut self,
        symbol: NodeId<ast::IdentOrPattern>,
    ) -> Result<Option<Reg>> {
        match self.ast[symbol] {
            ast::IdentOrPattern::Ident(x) => match self.registers.store(self.ast, x) {
                Ok(_) => todo!(),
                Err(_) => todo!(),
            },
            ast::IdentOrPattern::Pattern(_) => Ok(None),
        }
    }
}
