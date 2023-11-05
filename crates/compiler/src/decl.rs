use ast::NodeId;
use bc::Reg;

use crate::{Compiler, Result};

impl<'a> Compiler<'a> {
    pub fn compile_variable_decl(
        &mut self,
        kind: ast::VariableKind,
        decl: NodeId<ast::VariableDecl>,
    ) -> Result<()> {
        let initializer = self.ast[decl].initializer;
        let decl = self.ast[decl].decl;

        let placement = match self.ast[decl] {
            ast::IdentOrPattern::Ident(x) => {}
            ast::IdentOrPattern::Pattern(_) => todo!(),
        };

        todo!()
    }

    pub fn binding_placement(
        &mut self,
        symbol: NodeId<ast::IdentOrPattern>,
    ) -> Result<Option<Reg>> {
        match self.ast[symbol] {
            ast::IdentOrPattern::Ident(x) => {
                to_do!()
            }
            ast::IdentOrPattern::Pattern(_) => Ok(None),
        }
    }
}
