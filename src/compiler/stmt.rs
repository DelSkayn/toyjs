use crate::{
    ast::{Binding, Decl, DeclKind, Stmt, StmtKind},
    compiler::{Compiler, Result},
    runtime::bc::{DataValue, Op},
    source::Span,
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
            StmtKind::Declaration { ref kind } => self.compile_declaration(stmt.span, kind),
            _ => to_do!(stmt.span),
        }
    }

    pub fn compile_declaration(&mut self, span: Span, decl: &DeclKind) -> Result<()> {
        match decl {
            DeclKind::Lexical(ref x) => self.compile_lexical_decl(x),
            _ => to_do!(span),
        }
    }

    pub fn compile_lexical_decl(&mut self, decl: &Decl) -> Result<()> {
        for decl in decl.decl.iter() {
            let value = match decl.binding {
                Binding::Ident(ref x) => &x.0,
                _ => todo!(),
            };

            if let Some(ref expr) = decl.initializer {
                if let Some(reg) = self.compile_assignment_expr(expr)? {
                    let k_reg = self.regs.alloc().unwrap();
                    self.load_data_value(k_reg, DataValue::String(value.clone()));
                    self.type_a(Op::OSET, 0xff, k_reg, reg);
                    self.regs.free(k_reg);
                    self.regs.free(reg);
                }
            };
            //TODO eventually we should prob do something that
            //when we do not initialize the variable
        }
        Ok(())
    }
}
