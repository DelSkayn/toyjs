use ast::NodeId;
use bc::Reg;

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn binding_placement(
        &mut self,
        symbol: NodeId<ast::IdentOrPattern>,
    ) -> Result<Option<Reg>> {
        match self.ast[symbol] {
            ast::IdentOrPattern::Ident(x) => {
                let symbol_reg = self.registers.alloc_symbol(x);
                to_do!()
            }
            ast::IdentOrPattern::Pattern(_) => Ok(None),
        }
    }
}
