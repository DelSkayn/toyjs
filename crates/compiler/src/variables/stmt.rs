use ast::{
    BindingElement, BindingPattern, BindingProperty, ForLoopHead, Function, ListHead, ListId,
    NodeId,
};

use crate::{variables::ScopeKind, Error, Result};

use super::{Kind, VariablesBuilder};

impl<'a> VariablesBuilder<'a> {
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
                self.push_scope(super::ScopeKind::Block(stmt));
                self.resolve_stmts(list)?;
                self.pop_scope()?;
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
                self.resolve_for_head(head)?;
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
                    if let Some(binding) = self.ast[catch].binding {
                        self.resolve_decl(Kind::Let, binding, None)?;
                    }
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

    pub fn resolve_for_head(&mut self, head: NodeId<ForLoopHead>) -> Result<()> {
        match self.ast[head] {
            ForLoopHead::CStyle { decl, cond, post } => {
                match decl {
                    ast::CstyleDecl::Expr(x) => {
                        self.resolve_exprs(x)?;
                    }
                    ast::CstyleDecl::Decl { kind, decl } => {
                        let mut head = Some(decl);
                        while let Some(item) = head {
                            let id = self.ast[item].item;
                            self.resolve_decl(
                                kind.into(),
                                self.ast[id].decl,
                                self.ast[id].initializer,
                            )?;
                            head = self.ast[item].next;
                        }
                    }
                    ast::CstyleDecl::Empty => {}
                }

                if let Some(cond) = cond {
                    self.resolve_exprs(cond)?;
                }
                if let Some(post) = post {
                    self.resolve_exprs(post)?;
                }
            }
            ForLoopHead::In { decl, mut expr } => {
                match decl {
                    ast::InOfDecl::Expr(x) => self.resolve_expr(x)?,
                    ast::InOfDecl::Decl { kind, binding } => {
                        // find the last expression.
                        let mut head = expr;
                        while let Some(next) = self.ast[head].next {
                            head = next;
                        }
                        self.resolve_decl(kind.into(), binding, Some(self.ast[head].item))?;
                    }
                }
                loop {
                    self.resolve_expr(self.ast[expr].item)?;
                    if let Some(next) = self.ast[expr].next {
                        expr = next;
                    } else {
                        break;
                    }
                }
            }
            ForLoopHead::Of { decl, expr } => {
                match decl {
                    ast::InOfDecl::Expr(x) => self.resolve_expr(x)?,
                    ast::InOfDecl::Decl { kind, binding } => {
                        self.resolve_decl(kind.into(), binding, Some(expr))?;
                    }
                }
                self.resolve_expr(expr)?;
            }
        }
        Ok(())
    }

    pub fn resolve_decl(
        &mut self,
        kind: Kind,
        decl: NodeId<ast::IdentOrPattern>,
        initializer: Option<NodeId<ast::Expr>>,
    ) -> Result<()> {
        match self.ast[decl] {
            ast::IdentOrPattern::Ident(x) => {
                let symbol = self.declare(x, kind, Some(decl))?;
                if let Some(init) = initializer {
                    self.store_symbol(symbol, init);
                    self.resolve_expr(init)?;
                }
            }
            ast::IdentOrPattern::Pattern(pattern) => {
                self.resolve_binding_pattern(kind, Some(decl), pattern, initializer)?;
            }
        }
        Ok(())
    }

    pub fn resolve_binding_pattern(
        &mut self,
        kind: Kind,
        decl: Option<NodeId<ast::IdentOrPattern>>,
        pattern: NodeId<BindingPattern>,
        initializer: Option<NodeId<ast::Expr>>,
    ) -> Result<()> {
        match self.ast[pattern] {
            BindingPattern::Object { properties, rest } => {
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
                    self.declare(rest, kind, decl)?;
                }
            }
            BindingPattern::Array { elements, rest } => {
                if let Some(mut head) = elements {
                    loop {
                        if let Some(elem) = self.ast[head].data {
                            self.resolve_binding_element(kind, decl, elem, initializer)?;
                        }
                        if let Some(x) = self.ast[head].next {
                            head = x;
                        } else {
                            break;
                        }
                    }
                }

                if let Some(rest) = rest {
                    self.resolve_decl(kind, rest, initializer)?;
                }
            }
        }
        Ok(())
    }

    pub fn resolve_binding_property(
        &mut self,
        kind: Kind,
        decl: Option<NodeId<ast::IdentOrPattern>>,
        property: NodeId<BindingProperty>,
        initializer: Option<NodeId<ast::Expr>>,
    ) -> Result<()> {
        match self.ast[property] {
            BindingProperty::Binding {
                symbol,
                initializer,
            } => {
                self.declare(symbol, kind, decl)?;
            }
            BindingProperty::Property { element, .. } => {
                self.resolve_binding_element(kind, decl, element, initializer)?;
            }
        }
        Ok(())
    }

    pub fn resolve_binding_element(
        &mut self,
        kind: Kind,
        decl: Option<NodeId<ast::IdentOrPattern>>,
        element: NodeId<BindingElement>,
        initializer: Option<NodeId<ast::Expr>>,
    ) -> Result<()> {
        match self.ast[element] {
            BindingElement::SingleName {
                symbol: name,
                initializer,
            } => {
                self.declare(name, kind, decl)?;
            }
            BindingElement::Pattern {
                pattern,
                initializer,
            } => self.resolve_binding_pattern(kind, decl, pattern, initializer)?,
        }
        Ok(())
    }

    pub fn resolve_func(&mut self, func: NodeId<Function>) -> Result<()> {
        let function = &self.ast[func];
        match *function {
            Function::Arrow {
                params,
                rest_param,
                body,
                ..
            } => {
                self.push_scope(ScopeKind::Function(func));
                let mut head: Option<ListId<ast::BindingElement>> = params.into();
                while let Some(item) = head {
                    let elem = self.ast[item].item;
                    head = self.ast[item].next;
                    self.resolve_binding_element(Kind::Arg, None, elem, None)?;
                }
                if let Some(rest) = rest_param {
                    self.resolve_decl(Kind::Arg, rest, None)?;
                }
                match body {
                    ast::ArrowFunctionBody::Expr(x) => self.resolve_expr(x)?,
                    ast::ArrowFunctionBody::Stmt(body) => self.resolve_stmts(body)?,
                }
                self.pop_scope()?;
                Ok(())
            }
            Function::Base {
                name,
                params,
                rest_param,
                body,
                ..
            } => {
                if let Some(name) = name {
                    self.declare(name, Kind::Function, None)?;
                }
                self.push_scope(ScopeKind::Function(func));
                let mut head: Option<ListId<ast::BindingElement>> = params.into();
                while let Some(item) = head {
                    let elem = self.ast[item].item;
                    head = self.ast[item].next;
                    self.resolve_binding_element(Kind::Arg, None, elem, None)?;
                }
                if let Some(rest) = rest_param {
                    self.resolve_decl(Kind::Arg, rest, None)?;
                }
                self.resolve_stmts(body)?;
                self.pop_scope()?;
                Ok(())
            }
        }
    }

    pub fn resolve_class(&mut self, func: NodeId<ast::Class>) -> Result<()> {
        to_do!()
    }
}
