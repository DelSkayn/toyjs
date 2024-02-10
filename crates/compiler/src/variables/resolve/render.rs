use std::fmt;

use ast::{visitor::Visitor as _, Ast, ListId};
use common::{source::Source, structs::Interners};

use crate::{variables::ScopeKind, Result};

use super::{
    driver::{VariableVisitor, VisitorDriver},
    ScopeId, Variables,
};

#[derive(Clone, Copy)]
pub struct RenderVariables<'a> {
    root: ListId<ast::Stmt>,
    root_scope: ScopeId,
    variables: &'a Variables,
    ast: &'a Ast,
    interners: &'a Interners,
    source: &'a Source,
}

impl fmt::Display for RenderVariables<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut inner = RenderVariablesWriter {
            writer: f,
            inner: *self,
            last: self.root_scope,
            current: self.root_scope,
        };
        inner.write_decls();

        VisitorDriver::new(inner)
            .visit_stmt_list(self.root)
            .unwrap();
        Ok(())
    }
}

pub struct RenderVariablesWriter<'a, 'b, 'c> {
    writer: &'b mut fmt::Formatter<'c>,
    inner: RenderVariables<'a>,
    last: ScopeId,
    current: ScopeId,
}

impl RenderVariablesWriter<'_, '_, '_> {
    fn write_spacing(&mut self) {
        let mut current = self.current;
        while let Some(p) = self.inner.variables.scopes[current].parent {
            write!(self.writer, "    ").unwrap();
            current = p;
        }
    }

    fn write_decls(&mut self) {
        for sym_id in self.inner.variables.declared_vars(self.current) {
            let sym = &self.inner.variables.symbols[*sym_id];
            self.write_spacing();
            let last_use = self.inner.variables.last_use_of(*sym_id);
            writeln!(
                self.writer,
                "DECL #{} {:?} {} ${}..${}",
                sym_id.to_u32(),
                sym.kind,
                self.inner.interners.strings.get(sym.ident).unwrap(),
                sym.defined.map(|x| x.to_u32() as i64).unwrap_or(-1),
                last_use.map(|x| x.to_u32() as i64).unwrap_or(-1),
            )
            .unwrap();
        }
    }
}

impl VariableVisitor for RenderVariablesWriter<'_, '_, '_> {
    fn ast(&self) -> &Ast {
        self.inner.ast
    }

    fn push_scope(&mut self, kind: ScopeKind) -> Result<()> {
        self.write_spacing();
        writeln!(self.writer, "BLOCK {{").unwrap();

        self.last = self.last.next();
        self.current = self.last;

        self.write_decls();

        Ok(())
    }

    fn pop_scope(&mut self) -> Result<()> {
        self.current = self.inner.variables.scopes[self.current]
            .parent
            .expect("tried to pop root scope");

        self.write_spacing();
        writeln!(self.writer, "}}").unwrap();

        Ok(())
    }

    fn use_symbol(&mut self, ast_node: ast::NodeId<ast::Symbol>) -> Result<()> {
        let use_info = &self.inner.variables.ast_to_symbol[ast_node];

        self.write_spacing();
        writeln!(
            self.writer,
            "USE #{} @ ${} {}",
            use_info.id.unwrap().to_u32(),
            use_info.use_order.to_u32(),
            self.inner
                .source
                .render_span(self.inner.ast[ast_node].span, None)
                .unwrap()
                .as_highlight()
        )
        .unwrap();

        Ok(())
    }
}

impl Variables {
    pub fn render<'a>(
        &'a self,
        root: ListId<ast::Stmt>,
        root_scope: ScopeId,
        ast: &'a Ast,
        interners: &'a Interners,
        source: &'a Source,
    ) -> RenderVariables {
        RenderVariables {
            source,
            root_scope,
            root,
            variables: self,
            ast,
            interners,
        }
    }
}
