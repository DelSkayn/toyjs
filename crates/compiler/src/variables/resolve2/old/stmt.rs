use ast::{
    BindingElement, BindingPattern, BindingProperty, ForLoopHead, Function, ListHead, ListId,
    NodeId,
};

use crate::{
    variables::{Kind, ScopeKind},
    Result,
};

use super::{loop_stack::LoopPoint, VariablesBuilder};

#[derive(Clone, Copy)]
pub enum BindingKind {
    Decl { kind: Kind, is_initialized: bool },
    Destructure { expr: NodeId<ast::Expr> },
}

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
                self.push_scope(ScopeKind::Block)?;
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

                    if let Some(initializer) = self.ast[item].initializer {
                        self.resolve_expr(initializer)?;
                    }

                    self.resolve_decl(
                        BindingKind::Decl {
                            kind,
                            is_initialized: self.ast[item].initializer.is_some(),
                        },
                        self.ast[item].decl,
                    )?;

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
                self.push_scope(ScopeKind::Block)?;
                let loop_point = self.mark_loop();
                self.resolve_stmt(body)?;
                self.resolve_exprs(cond)?;
                self.finish_loop(loop_point);
                self.pop_scope()?;
            }
            ast::Stmt::If { cond, body, r#else } => {
                self.resolve_exprs(cond)?;
                self.resolve_stmt(body)?;
                if let Some(e) = r#else {
                    self.resolve_stmt(e)?;
                }
            }
            ast::Stmt::While { cond, body } => {
                self.push_scope(ScopeKind::Block)?;
                let loop_point = self.mark_loop();
                self.resolve_exprs(cond)?;
                self.resolve_stmt(body)?;
                self.finish_loop(loop_point);
                self.pop_scope()?;
            }
            ast::Stmt::For { head, body } => {
                self.push_scope(ScopeKind::Block)?;
                let loop_point = self.resolve_for_head_pre(head)?;
                self.resolve_stmt(body)?;
                self.resolve_for_head_post(head, loop_point)?;
                self.pop_scope()?;
            }
            ast::Stmt::Switch {
                cond,
                cases,
                default,
            } => {
                self.push_scope(ScopeKind::Block)?;
                self.resolve_exprs(cond)?;
                let mut head: Option<ListId<ast::CaseItem>> = Option::from(cases);
                while let Some(list) = head {
                    let item = self.ast[list].item;
                    self.resolve_exprs(self.ast[item].expr)?;
                    self.resolve_stmts(self.ast[item].stmts)?;
                    head = self.ast[list].next;
                }

                if let Some(default) = default {
                    self.resolve_stmts(default)?;
                }
                self.pop_scope()?;
            }
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
                    self.push_scope(ScopeKind::Block)?;
                    if let Some(binding) = self.ast[catch].binding {
                        self.resolve_decl(
                            BindingKind::Decl {
                                kind: Kind::Let,
                                is_initialized: true,
                            },
                            binding,
                        )?;
                    }
                    self.resolve_stmts(self.ast[catch].block)?;
                    self.pop_scope()?;
                }
                if let Some(finally) = finally {
                    self.push_scope(ScopeKind::Block)?;
                    self.resolve_stmts(finally)?;
                    self.pop_scope()?;
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

    pub fn resolve_for_head_pre(&mut self, head: NodeId<ForLoopHead>) -> Result<LoopPoint> {
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

                            if let Some(init) = self.ast[id].initializer {
                                self.resolve_expr(init)?;
                            }

                            self.resolve_decl(
                                BindingKind::Decl {
                                    kind: kind.into(),
                                    is_initialized: self.ast[id].initializer.is_some(),
                                },
                                self.ast[id].decl,
                            )?;

                            head = self.ast[item].next;
                        }
                    }
                    ast::CstyleDecl::Empty => {}
                }

                self.push_scope(ScopeKind::Block)?;
                let loop_point = self.mark_loop();
                if let Some(cond) = cond {
                    self.resolve_exprs(cond)?;
                }
                Ok(loop_point)
            }
            ForLoopHead::In { decl, expr } => match decl {
                ast::InOfDecl::Expr(x) => {
                    self.resolve_expr(x)?;
                    self.resolve_exprs(expr)?;
                    self.push_scope(ScopeKind::Block)?;
                    Ok(self.mark_loop())
                }
                ast::InOfDecl::Decl { kind, binding } => {
                    self.resolve_exprs(expr)?;

                    self.push_scope(ScopeKind::Block)?;
                    let loop_point = self.mark_loop();

                    self.resolve_decl(
                        BindingKind::Decl {
                            kind: kind.into(),
                            is_initialized: true,
                        },
                        binding,
                    )?;

                    Ok(loop_point)
                }
            },
            ForLoopHead::Of { decl, expr } => match decl {
                ast::InOfDecl::Expr(x) => {
                    self.resolve_expr(x)?;
                    self.resolve_expr(expr)?;
                    self.push_scope(ScopeKind::Block)?;
                    Ok(self.mark_loop())
                }
                ast::InOfDecl::Decl { kind, binding } => {
                    self.resolve_expr(expr)?;

                    self.push_scope(ScopeKind::Block)?;
                    let loop_point = self.mark_loop();
                    self.resolve_decl(
                        BindingKind::Decl {
                            kind: kind.into(),
                            is_initialized: true,
                        },
                        binding,
                    )?;

                    Ok(loop_point)
                }
            },
        }
    }

    pub fn resolve_for_head_post(
        &mut self,
        head: NodeId<ForLoopHead>,
        loop_point: LoopPoint,
    ) -> Result<()> {
        // TODO: make sure that condition variables are still accessable after loop.
        match self.ast[head] {
            ForLoopHead::CStyle { post, .. } => {
                if let Some(post) = post {
                    self.resolve_exprs(post)?;
                }
            }

            ForLoopHead::In { .. } | ForLoopHead::Of { .. } => {}
        }
        self.finish_loop(loop_point);
        self.pop_scope()?;
        Ok(())
    }

    pub fn resolve_decl(
        &mut self,
        kind: BindingKind,
        decl: NodeId<ast::IdentOrPattern>,
    ) -> Result<()> {
        match self.ast[decl] {
            ast::IdentOrPattern::Ident(x) => match kind {
                BindingKind::Decl {
                    kind,
                    is_initialized,
                } => {
                    let symbol = self.declare(x, kind, is_initialized)?;
                }
                BindingKind::Destructure { expr } => {
                    self.store(x);
                }
            },
            ast::IdentOrPattern::Pattern(pattern) => {
                self.resolve_binding_pattern(kind, pattern)?;
            }
        }
        Ok(())
    }

    pub fn resolve_binding_pattern(
        &mut self,
        kind: BindingKind,
        pattern: NodeId<BindingPattern>,
    ) -> Result<()> {
        match self.ast[pattern] {
            BindingPattern::Object { properties, rest } => {
                if let ListHead::Present(mut head) = properties {
                    loop {
                        self.resolve_binding_property(kind, self.ast[head].item)?;
                        if let Some(x) = self.ast[head].next {
                            head = x;
                        } else {
                            break;
                        }
                    }
                }

                if let Some(rest) = rest {
                    match kind {
                        BindingKind::Decl {
                            kind,
                            is_initialized,
                        } => {
                            let symbol = self.declare(rest, kind, is_initialized)?;
                        }
                        BindingKind::Destructure { expr } => {
                            self.store(rest);
                        }
                    }
                }
            }
            BindingPattern::Array { elements, rest } => {
                if let Some(mut head) = elements {
                    loop {
                        if let Some(elem) = self.ast[head].data {
                            self.resolve_binding_element(kind, elem)?;
                        }
                        if let Some(x) = self.ast[head].next {
                            head = x;
                        } else {
                            break;
                        }
                    }
                }

                if let Some(rest) = rest {
                    self.resolve_decl(kind, rest)?;
                }
            }
        }
        Ok(())
    }

    pub fn resolve_binding_property(
        &mut self,
        kind: BindingKind,
        property: NodeId<BindingProperty>,
    ) -> Result<()> {
        match self.ast[property] {
            BindingProperty::Binding {
                symbol,
                initializer,
            } => {
                if let Some(init) = initializer {
                    self.resolve_expr(init)?;
                }
                match kind {
                    BindingKind::Decl {
                        kind,
                        is_initialized,
                    } => {
                        let symbol =
                            self.declare(symbol, kind, is_initialized || initializer.is_some())?;
                    }
                    BindingKind::Destructure { expr } => {
                        self.store(symbol);
                    }
                }
            }
            BindingProperty::Property { name, element } => {
                if let ast::PropertyName::Computed(x) = name {
                    self.resolve_expr(x)?;
                }
                self.resolve_binding_element(kind, element)?;
            }
        }
        Ok(())
    }

    pub fn resolve_binding_element(
        &mut self,
        kind: BindingKind,
        element: NodeId<BindingElement>,
    ) -> Result<()> {
        match self.ast[element] {
            BindingElement::SingleName {
                symbol: name,
                initializer,
            } => {
                if let Some(init) = initializer {
                    self.resolve_expr(init)?;
                }

                match kind {
                    BindingKind::Decl {
                        kind,
                        is_initialized,
                    } => {
                        let symbol =
                            self.declare(name, kind, is_initialized || initializer.is_some())?;
                    }
                    BindingKind::Destructure { expr } => {
                        self.store(name);
                    }
                }
            }
            BindingElement::Pattern {
                pattern,
                initializer,
            } => {
                self.resolve_binding_pattern(kind, pattern)?;
                if let Some(init) = initializer {
                    self.resolve_expr(init)?;
                }
            }
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
                self.push_scope(ScopeKind::Function(func))?;
                let mut head: Option<ListId<ast::BindingElement>> = params.into();
                while let Some(item) = head {
                    let elem = self.ast[item].item;
                    head = self.ast[item].next;
                    self.resolve_binding_element(
                        BindingKind::Decl {
                            kind: Kind::Arg,
                            is_initialized: true,
                        },
                        elem,
                    )?;
                }
                if let Some(rest) = rest_param {
                    self.resolve_decl(
                        BindingKind::Decl {
                            kind: Kind::Arg,
                            is_initialized: true,
                        },
                        rest,
                    )?;
                }
                match body {
                    ast::ArrowFunctionBody::Expr(x) => self.resolve_expr(x)?,
                    ast::ArrowFunctionBody::Stmt(body) => self.resolve_stmts(body)?,
                }
                self.pop_scope()?;
                Ok(())
            }
            Function::Declared {
                name,
                params,
                rest_param,
                body,
                ..
            } => {
                self.declare(name, Kind::Function, true)?;
                self.push_scope(ScopeKind::Function(func))?;
                let mut head: Option<ListId<ast::BindingElement>> = params.into();
                while let Some(item) = head {
                    let elem = self.ast[item].item;
                    head = self.ast[item].next;
                    self.resolve_binding_element(
                        BindingKind::Decl {
                            kind: Kind::Arg,
                            is_initialized: true,
                        },
                        elem,
                    )?;
                }
                if let Some(rest) = rest_param {
                    self.resolve_decl(
                        BindingKind::Decl {
                            kind: Kind::Arg,
                            is_initialized: true,
                        },
                        rest,
                    )?;
                }
                self.resolve_stmts(body)?;
                self.pop_scope()?;
                Ok(())
            }
            Function::Expr {
                params,
                rest_param,
                body,
                ..
            } => {
                self.push_scope(ScopeKind::Function(func))?;
                let mut head: Option<ListId<ast::BindingElement>> = params.into();
                while let Some(item) = head {
                    let elem = self.ast[item].item;
                    head = self.ast[item].next;
                    self.resolve_binding_element(
                        BindingKind::Decl {
                            kind: Kind::Arg,
                            is_initialized: true,
                        },
                        elem,
                    )?;
                }
                if let Some(rest) = rest_param {
                    self.resolve_decl(
                        BindingKind::Decl {
                            kind: Kind::Arg,
                            is_initialized: true,
                        },
                        rest,
                    )?;
                }
                self.resolve_stmts(body)?;
                self.pop_scope()?;
                Ok(())
            }
        }
    }

    pub fn resolve_class(&mut self, class: NodeId<ast::Class>) -> Result<()> {
        if let Some(name) = self.ast[class].name {
            self.declare(name, Kind::Function, true)?;
        }
        if let Some(heritage) = self.ast[class].heritage {
            self.resolve_expr(heritage)?;
        }

        let mut cur: Option<ListId<ast::ClassMember>> = self.ast[class].body.into();
        while let Some(c) = cur {
            cur = self.ast[c].next;
            let item = self.ast[c].item;
            match self.ast[item] {
                ast::ClassMember::StaticBlock { stmts } => {
                    self.push_scope(ScopeKind::Static)?;
                    self.resolve_stmts(stmts)?;
                    self.pop_scope()?;
                }

                ast::ClassMember::Method { property, func, .. }
                | ast::ClassMember::Getter { property, func, .. }
                | ast::ClassMember::Setter { property, func, .. } => {
                    match property {
                        ast::PropertyName::Ident(_)
                        | ast::PropertyName::String(_)
                        | ast::PropertyName::Number(_) => {}
                        ast::PropertyName::Computed(x) => {
                            self.resolve_expr(x)?;
                        }
                    }
                    self.resolve_func(func)?;
                }
                ast::ClassMember::Field {
                    property,
                    initializer,
                    ..
                } => {
                    if let Some(init) = initializer {
                        self.resolve_expr(init)?;
                    }
                    match property {
                        ast::PropertyName::Ident(_)
                        | ast::PropertyName::String(_)
                        | ast::PropertyName::Number(_) => {}
                        ast::PropertyName::Computed(x) => {
                            self.resolve_expr(x)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
