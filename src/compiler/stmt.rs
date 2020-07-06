use crate::{
    ast::{Stmt, StmtKind},
    compiler::{Compiler, Result},
};

impl Compiler {
    pub fn compile_statement(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt.kind {
            StmtKind::Block(ref x) => {
                for s in x.stmts.iter() {
                    self.compile_statement(s)?
                }
                Ok(())
            }
            StmtKind::Empty => Ok(()),
            StmtKind::Expr { ref expr } => {
                self.parse_expression(expr)?;
                Ok(())
            }
            _ => to_do!(stmt.span),
        }
    }
}
