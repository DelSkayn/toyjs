use std::fmt::{self, Write as _};

use ast::{visitor::Visitor as _, Ast, NodeListId};
use common::{format::IndentFormatter, source::Source};

use super::{
    driver::{VariableVisitor, VisitorDriver},
    ScopeId, Variables,
};
use crate::{variables::ScopeKind, Result};

#[derive(Clone, Copy)]
pub struct RenderVariables<'a> {
    root: NodeListId<ast::Stmt>,
    root_scope: ScopeId,
    variables: &'a Variables,
    ast: &'a Ast,
    source: &'a Source,
}

impl fmt::Display for RenderVariables<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut inner = RenderVariablesWriter {
            fmt: IndentFormatter::new(f, 2),
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

pub struct RenderVariablesWriter<'a, W> {
    fmt: IndentFormatter<W>,
    inner: RenderVariables<'a>,
    last: ScopeId,
    current: ScopeId,
}

impl<W: fmt::Write> RenderVariablesWriter<'_, W> {
    fn write_decls(&mut self) {
        for sym_id in self.inner.variables.declared_vars(self.current) {
            let sym = &self.inner.variables.symbols[*sym_id];
            let last_use = self.inner.variables.last_use_of(*sym_id);
            writeln!(
                self.fmt,
                "DECL #{} {:?} {} ${}..${}",
                sym_id.into_u32(),
                sym.kind,
                self.inner.ast[sym.ident],
                sym.defined.map(|x| x.to_u32() as i64).unwrap_or(-1),
                last_use.map(|x| x.to_u32() as i64).unwrap_or(-1),
            )
            .unwrap();
        }
    }
}

impl<W: fmt::Write> VariableVisitor for RenderVariablesWriter<'_, W> {
    fn ast(&self) -> &Ast {
        self.inner.ast
    }

    fn push_scope(&mut self, kind: ScopeKind) -> Result<()> {
        self.fmt.increase_depth();
        writeln!(self.fmt, "BLOCK {{").unwrap();

        self.last = self.last.next().unwrap();
        self.current = self.last;

        self.write_decls();

        Ok(())
    }

    fn pop_scope(&mut self) -> Result<()> {
        self.fmt.decrease_depth();
        self.current = self.inner.variables.scopes[self.current]
            .parent
            .expect("tried to pop root scope");

        writeln!(self.fmt, "}}").unwrap();

        Ok(())
    }

    fn use_symbol(&mut self, ast_node: ast::NodeId<ast::Symbol>) -> Result<()> {
        let use_info = &self.inner.variables.ast_to_symbol[ast_node];

        writeln!(
            self.fmt,
            "USE #{} @ ${} {}",
            use_info.id.unwrap().into_u32(),
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
        root: NodeListId<ast::Stmt>,
        root_scope: ScopeId,
        ast: &'a Ast,
        source: &'a Source,
    ) -> RenderVariables<'a> {
        RenderVariables {
            source,
            root_scope,
            root,
            variables: self,
            ast,
        }
    }
}
