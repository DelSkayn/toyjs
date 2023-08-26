use ast::{BindingElement, BindingPattern, BindingProperty, ListHead, NodeId};

use crate::{Compiler, Result};

use super::Kind;

impl<'a> Compiler<'a> {
    pub(super) fn resolve_stmts(&mut self, stmt: ListHead<ast::Stmt>) -> Result<()> {
        let ListHead::Present(mut head) = stmt else {
            return Ok(());
        };
        loop {
            self.resolve_stmt(self.ast[head].item)?;
            if let Some(next) = self.ast[head].next {
                head = next;
            } else {
                break;
            }
        }
        Ok(())
    }

    pub(super) fn resolve_stmt(&mut self, stmt: NodeId<ast::Stmt>) -> Result<()> {
        match self.ast[stmt] {
            ast::Stmt::Empty
            | ast::Stmt::Break { .. }
            | ast::Stmt::Continue { .. }
            | ast::Stmt::Debugger => {}
            ast::Stmt::Block { list } => {
                self.resolve_stmts(list)?;
            }
            ast::Stmt::VariableDecl { kind, mut decl } => {
                let kind = match kind {
                    ast::VariableKind::Const => Kind::Const,
                    ast::VariableKind::Var => Kind::Function,
                    ast::VariableKind::Let => Kind::Let,
                };
                loop {
                    let item = self.ast[decl].item;
                    self.resolve_decl(kind, self.ast[item].decl, self.ast[item].initializer)?;

                    if let Some(x) = self.ast[decl].next {
                        decl = x;
                    } else {
                        break;
                    }
                }
            }
            ast::Stmt::Expr { expr } => {
                self.resolve_exprs(expr)?;
            }
            ast::Stmt::DoWhile { body, cond } => {
                self.resolve_stmt(body)?;
                self.resolve_exprs(cond)?;
            }
            ast::Stmt::If { cond, body, r#else } => {
                self.resolve_exprs(cond)?;
                self.resolve_stmt(body)?;
                if let Some(e) = r#else {
                    self.resolve_stmt(e)?;
                }
            }
            ast::Stmt::While { cond, body } => {
                self.resolve_exprs(cond)?;
                self.resolve_stmt(body)?;
            }
            ast::Stmt::For { head, body } => {
                //TODO
                self.resolve_stmt(body)?;
            }
            ast::Stmt::Switch {
                cond,
                cases,
                default,
            } => to_do!(),
            ast::Stmt::Throw { expr } => {
                self.resolve_exprs(expr)?;
            }
            ast::Stmt::Try {
                block,
                catch,
                finally,
            } => {
                self.resolve_stmts(block)?;

                if let Some(catch) = catch {
                    self.resolve_stmts(self.ast[catch].block)?;
                }

                if let Some(finally) = finally {
                    self.resolve_stmts(finally)?;
                }
            }
            ast::Stmt::With { expr, stmt } => to_do!(),
            ast::Stmt::Return { expr } => {
                if let Some(expr) = expr {
                    self.resolve_exprs(expr)?;
                }
            }
            ast::Stmt::Labeled { label, stmt } => {
                self.resolve_stmt(stmt)?;
            }
            ast::Stmt::Function { func } => self.resolve_func(func)?,
            ast::Stmt::Class { class } => self.resolve_class(class)?,
        }
        Ok(())
    }

    pub fn resolve_decl(
        &mut self,
        kind: Kind,
        decl: NodeId<ast::IdentOrPattern>,
        initializer: Option<NodeId<ast::Expr>>,
    ) -> Result<()> {
        if let Kind::Const = kind {
            if initializer.is_none() {
                to_do!()
            }
        }

        match self.ast[decl] {
            ast::IdentOrPattern::Ident(x) => {
                self.variables.declare(x, kind, decl)?;
            }
            ast::IdentOrPattern::Pattern(pattern) => {
                self.resolve_binding_pattern(kind, decl, pattern, initializer)?;
            }
        }
        Ok(())
    }

    pub fn resolve_binding_pattern(
        &mut self,
        kind: Kind,
        decl: NodeId<ast::IdentOrPattern>,
        pattern: NodeId<BindingPattern>,
        initializer: Option<NodeId<ast::Expr>>,
    ) -> Result<()> {
        to_do!()
        /*
            if let ListHead::Present(mut head) = properties {
                loop {
                    self.resolve_binding_property(
                        kind,
                        decl,
                        self.ast[head].item,
                        initializer,
                    )?;
                    if let Some(x) = self.ast[head].next {
                        head = x;
                    } else {
                        break;
                    }
                }
            }

            if let Some(rest) = rest {
                self.variables.declare(rest, kind, decl)?;
            }
        */
    }

    pub fn resolve_binding_property(
        &mut self,
        kind: Kind,
        decl: NodeId<ast::IdentOrPattern>,
        element: NodeId<BindingProperty>,
        initializer: Option<NodeId<ast::Expr>>,
    ) -> Result<()> {
        match self.ast[element] {
            BindingProperty::Binding { name, initializer } => {
                self.variables.declare(name, kind, decl)?;
            }
            BindingProperty::Property { element, .. } => {}
        }
        to_do!()
    }

    pub fn resolve_binding_element(
        &mut self,
        kind: Kind,
        decl: NodeId<ast::IdentOrPattern>,
        element: NodeId<Option<BindingElement>>,
        initializer: Option<NodeId<ast::Expr>>,
    ) -> Result<()> {
        to_do!()
    }

    pub fn resolve_func(&mut self, func: NodeId<ast::Function>) -> Result<()> {
        to_do!()
    }

    pub fn resolve_class(&mut self, func: NodeId<ast::Class>) -> Result<()> {
        to_do!()
    }
}
