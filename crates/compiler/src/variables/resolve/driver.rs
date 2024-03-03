use std::mem;

use ast::{visitor::Visitor, Ast, Expr, ListHead, NodeId};

use crate::{
    variables::{Kind, ScopeKind},
    Error, Result,
};

pub trait VariableVisitor {
    fn ast(&self) -> &Ast;

    fn push_scope(&mut self, kind: ScopeKind) -> Result<()> {
        Ok(())
    }

    fn pop_scope(&mut self) -> Result<()> {
        Ok(())
    }

    fn declare(&mut self, ast_node: NodeId<ast::Symbol>, kind: Kind) -> Result<()> {
        let _ = kind;
        let _ = ast_node;
        Ok(())
    }

    fn use_symbol(&mut self, ast_node: NodeId<ast::Symbol>) -> Result<()> {
        let _ = ast_node;
        Ok(())
    }
}

pub struct VisitorDriver<V: VariableVisitor> {
    driven: V,
    declaring: Option<Kind>,
    decl_initialized: bool,
}

impl<V: VariableVisitor> VisitorDriver<V> {
    pub fn new(v: V) -> Self {
        VisitorDriver {
            driven: v,
            declaring: None,
            decl_initialized: false,
        }
    }

    pub fn into_inner(self) -> V {
        self.driven
    }

    fn resolve_params(
        &mut self,
        params: ListHead<ast::BindingElement>,
        rest: Option<NodeId<ast::IdentOrPattern>>,
    ) -> Result<()> {
        let before = self.declaring.replace(Kind::Arg);
        if let ListHead::Present(p) = params {
            self.super_binding_element_list(p)?;
        }
        if let Some(p) = rest {
            self.super_ident_or_pattern(p)?;
        }
        self.declaring = before;
        Ok(())
    }
}

impl<V: VariableVisitor> Visitor<Error> for VisitorDriver<V> {
    fn ast(&self) -> &Ast {
        self.driven.ast()
    }

    fn super_function(&mut self, func: NodeId<ast::Function>) -> Result<()> {
        match self.ast()[func] {
            ast::Function::Arrow {
                params,
                rest_param,
                body,
                ..
            } => {
                self.driven.push_scope(ScopeKind::Function(func))?;
                self.resolve_params(params, rest_param)?;
                match body {
                    ast::ArrowFunctionBody::Expr(x) => self.super_expr(x)?,
                    ast::ArrowFunctionBody::Stmt(x) => {
                        if let ListHead::Present(x) = x {
                            self.super_stmt_list(x)?;
                        }
                    }
                }
                self.driven.pop_scope()?;
            }
            ast::Function::Declared {
                name,
                params,
                rest_param,
                body,
                ..
            } => {
                self.driven.push_scope(ScopeKind::Function(func))?;
                self.resolve_params(params, rest_param)?;
                if let ListHead::Present(x) = body {
                    self.super_stmt_list(x)?;
                }
                self.driven.pop_scope()?;
                self.driven.declare(name, Kind::Function)?;
            }
            ast::Function::Expr {
                name,
                params,
                rest_param,
                body,
                ..
            } => {
                self.driven.push_scope(ScopeKind::Function(func))?;
                if let Some(sym) = name {
                    self.driven.declare(sym, Kind::Function)?;
                    self.resolve_params(params, rest_param)?;
                    self.driven
                        .push_scope(ScopeKind::Block { has_loop: false })?;
                } else {
                    self.resolve_params(params, rest_param)?;
                }
                if let ListHead::Present(x) = body {
                    self.super_stmt_list(x)?;
                }
                if name.is_some() {
                    self.driven.pop_scope()?;
                }
                self.driven.pop_scope()?;
            }
        }

        Ok(())
    }

    fn super_stmt(&mut self, stmt: NodeId<ast::Stmt>) -> std::prelude::v1::Result<(), Error> {
        match self.ast()[stmt] {
            ast::Stmt::Block { .. } => {
                self.driven
                    .push_scope(ScopeKind::Block { has_loop: false })?;
                self.visit_stmt(stmt)?;
                self.driven.pop_scope()?;
                return Ok(());
            }
            ast::Stmt::VariableDecl { kind, decl } => {
                let before = self.declaring.replace(kind.into());
                self.super_variable_decl_list(decl)?;
                self.declaring = before;
                return Ok(());
            }
            ast::Stmt::For { head, body } => {
                self.driven
                    .push_scope(ScopeKind::Block { has_loop: true })?;
                self.super_head_pre(head)?;
                self.super_stmt(body)?;
                self.super_head_post(head)?;
                self.driven.pop_scope()?;
                return Ok(());
            }
            ast::Stmt::While { cond, body } => {
                self.driven
                    .push_scope(ScopeKind::Block { has_loop: true })?;
                self.visit_expr_list(cond)?;
                self.visit_stmt(body)?;
                self.driven.pop_scope()?;
                return Ok(());
            }
            ast::Stmt::DoWhile { body, cond } => {
                self.driven
                    .push_scope(ScopeKind::Block { has_loop: true })?;
                self.visit_stmt(body)?;
                self.visit_expr_list(cond)?;
                self.driven.pop_scope()?;
                return Ok(());
            }
            ast::Stmt::Break { .. } | ast::Stmt::Continue { .. } => return Ok(()),
            ast::Stmt::Try {
                block,
                catch,
                finally,
            } => {
                self.driven
                    .push_scope(ScopeKind::Block { has_loop: false })?;
                if let ListHead::Present(stmt) = block {
                    self.super_stmt_list(stmt)?;
                }
                self.driven.pop_scope()?;

                if let Some(catch) = catch {
                    self.super_catch(catch)?;
                }

                self.driven
                    .push_scope(ScopeKind::Block { has_loop: false })?;
                if let Some(ListHead::Present(stmt)) = finally {
                    self.super_stmt_list(stmt)?;
                }
                self.driven.pop_scope()?;

                return Ok(());
            }
            ast::Stmt::Switch {
                cond,
                cases,
                default,
            } => {
                self.visit_expr_list(cond)?;
                self.driven
                    .push_scope(ScopeKind::Block { has_loop: false })?;
                if let ListHead::Present(cases) = cases {
                    self.super_cases(cases)?;
                }
                if let Some(ListHead::Present(stmt)) = default {
                    self.super_stmt_list(stmt)?;
                }
                self.driven.pop_scope()?;
                return Ok(());
            }
            _ => (),
        }
        self.visit_stmt(stmt)
    }

    fn super_head_pre(
        &mut self,
        head: NodeId<ast::ForLoopHead>,
    ) -> std::prelude::v1::Result<(), Error> {
        match self.ast()[head] {
            ast::ForLoopHead::In { decl, expr } => {
                if let ast::InOfDecl::Decl { kind, binding } = decl {
                    let before = self.declaring.replace(kind.into());
                    let before_init = mem::replace(&mut self.decl_initialized, true);
                    self.super_ident_or_pattern(binding)?;
                    self.decl_initialized = before_init;
                    self.declaring = before;
                }
                self.super_expr_list(expr)?;
            }
            ast::ForLoopHead::Of { decl, expr } => {
                if let ast::InOfDecl::Decl { kind, binding } = decl {
                    let before = self.declaring.replace(kind.into());
                    let before_init = mem::replace(&mut self.decl_initialized, true);
                    self.super_ident_or_pattern(binding)?;
                    self.decl_initialized = before_init;
                    self.declaring = before;
                }
                self.super_expr(expr)?;
            }
            ast::ForLoopHead::CStyle { decl, cond, .. } => {
                match decl {
                    ast::CstyleDecl::Empty => {}
                    ast::CstyleDecl::Expr(x) => {
                        self.super_expr_list(x)?;
                    }
                    ast::CstyleDecl::Decl { kind, decl } => {
                        let before = self.declaring.replace(kind.into());
                        self.super_variable_decl_list(decl)?;
                        self.declaring = before;
                    }
                }
                if let Some(cond) = cond {
                    self.super_expr_list(cond)?;
                }
            }
        }
        Ok(())
    }

    fn super_catch(&mut self, catch: NodeId<ast::CatchStmt>) -> Result<()> {
        self.driven
            .push_scope(ScopeKind::Block { has_loop: false })?;
        if let Some(binding) = self.ast()[catch].binding {
            let before = self.declaring.replace(Kind::Let);
            self.super_ident_or_pattern(binding)?;
            self.declaring = before;
        }
        if let ListHead::Present(block) = self.ast()[catch].block {
            self.super_stmt_list(block)?;
        }
        self.driven.pop_scope()?;
        Ok(())
    }

    fn super_class_member(
        &mut self,
        cls_mem: NodeId<ast::ClassMember>,
    ) -> std::prelude::v1::Result<(), Error> {
        if let ast::ClassMember::StaticBlock { .. } = self.ast()[cls_mem] {
            self.driven.push_scope(ScopeKind::Static)?;
            self.visit_class_member(cls_mem)?;
            self.driven.pop_scope()?;
            return Ok(());
        }
        self.visit_class_member(cls_mem)
    }

    fn super_expr(&mut self, expr: NodeId<ast::Expr>) -> Result<()> {
        if let Expr::Destructure { pattern, expr } = self.ast()[expr] {
            self.super_binding_pattern(pattern)?;
            self.super_expr(expr)?;
            return Ok(());
        }

        let before = self.declaring.take();
        self.visit_expr(expr)?;
        self.declaring = before;
        Ok(())
    }

    fn super_variable_decl(&mut self, decl: NodeId<ast::VariableDecl>) -> Result<()> {
        let new = self.ast()[decl].initializer.is_some();
        let old = std::mem::replace(&mut self.decl_initialized, new);
        self.visit_variable_decl(decl)?;
        self.decl_initialized = old;
        Ok(())
    }

    fn super_symbol(&mut self, s: NodeId<ast::Symbol>) -> Result<()> {
        if let Some(d) = self.declaring {
            self.driven.declare(s, d)?;
            if self.decl_initialized {
                self.driven.use_symbol(s)?;
            }
        } else {
            self.driven.use_symbol(s)?;
        }
        Ok(())
    }
}
