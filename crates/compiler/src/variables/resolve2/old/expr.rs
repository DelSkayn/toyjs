use crate::Result;
use ast::{BinaryOp, ListId, NodeId};

use super::{stmt::BindingKind, VariablesBuilder};

impl<'a> VariablesBuilder<'a> {
    pub(super) fn resolve_exprs(&mut self, mut expr: ListId<ast::Expr>) -> Result<()> {
        loop {
            self.resolve_expr(self.ast[expr].item)?;
            if let Some(x) = self.ast[expr].next {
                expr = x;
            } else {
                break;
            }
        }

        Ok(())
    }
    pub(super) fn resolve_expr(&mut self, expr: NodeId<ast::Expr>) -> Result<()> {
        match self.ast[expr] {
            ast::Expr::Binary { left, right, op } => {
                if let BinaryOp::Assign(op) = op {
                    self.resolve_assign_expr(left, right)?;
                    if op.loads() {
                        self.resolve_expr(left)?;
                    }
                } else {
                    self.resolve_expr(left)?;
                }
                self.resolve_expr(right)?;
            }
            ast::Expr::Index { index, expr } => {
                self.resolve_expr(index)?;
                self.resolve_expr(expr)?;
            }
            ast::Expr::Prefix { expr, .. }
            | ast::Expr::Postfix { expr, .. }
            | ast::Expr::Dot { expr, .. }
            | ast::Expr::Yield { expr, .. } => {
                self.resolve_expr(expr)?;
            }
            ast::Expr::Call { expr, args } => {
                self.resolve_expr(expr)?;
                let mut head = args;
                while let Some(id) = head {
                    self.resolve_expr(self.ast[id].data.expr)?;
                    head = self.ast[id].next;
                }
            }
            ast::Expr::Prime { expr: prime } => match self.ast[prime] {
                ast::PrimeExpr::Number(_)
                | ast::PrimeExpr::String(_)
                | ast::PrimeExpr::Regex(_)
                | ast::PrimeExpr::Boolean(_)
                | ast::PrimeExpr::NewTarget
                | ast::PrimeExpr::Null
                | ast::PrimeExpr::This
                | ast::PrimeExpr::Object(ast::ObjectLiteral::Empty)
                | ast::PrimeExpr::Super => {}
                ast::PrimeExpr::Ident(name) => {
                    self.load(dbg!(name));
                }
                ast::PrimeExpr::Function(func) => self.resolve_func(func)?,
                ast::PrimeExpr::Class(class) => self.resolve_class(class)?,
                ast::PrimeExpr::Covered(expr) => self.resolve_exprs(expr)?,
                ast::PrimeExpr::Template(mut tpl) => {
                    while let ast::Template::Head { expr, next, .. } = self.ast[tpl] {
                        self.resolve_exprs(expr)?;
                        tpl = next
                    }
                }
                ast::PrimeExpr::Object(ast::ObjectLiteral::Item(mut def)) => loop {
                    let item = self.ast[def].item;
                    match self.ast[item] {
                        ast::PropertyDefinition::Ident { .. } => {}
                        ast::PropertyDefinition::Covered { .. } => {
                            panic!("A covered object should not make it to the compiler");
                        }
                        ast::PropertyDefinition::Define { expr, property } => {
                            match property {
                                ast::PropertyName::Ident(_)
                                | ast::PropertyName::String(_)
                                | ast::PropertyName::Number(_) => {}
                                ast::PropertyName::Computed(x) => {
                                    self.resolve_expr(x)?;
                                }
                            }
                            self.resolve_expr(expr)?;
                        }
                        ast::PropertyDefinition::Method { func, property }
                        | ast::PropertyDefinition::Getter { func, property }
                        | ast::PropertyDefinition::Setter { func, property } => {
                            match property {
                                ast::PropertyName::Ident(_)
                                | ast::PropertyName::String(_)
                                | ast::PropertyName::Number(_) => {}
                                ast::PropertyName::Computed(x) => {
                                    self.resolve_expr(x)?;
                                }
                            };
                            self.resolve_func(func)?
                        }
                        ast::PropertyDefinition::Rest(expr) => self.resolve_expr(expr)?,
                    }
                    if let Some(next) = self.ast[def].next {
                        def = next
                    } else {
                        break;
                    }
                },
                ast::PrimeExpr::Array(x) => {
                    let mut elements: Option<ListId<ast::ArrayLiteralEntry>> = x.into();
                    while let Some(elem) = elements {
                        let item = self.ast[elem].item;
                        if let Some(expr) = self.ast[item].expr {
                            self.resolve_expr(expr)?;
                        }
                        elements = self.ast[elem].next;
                    }
                }
            },
            ast::Expr::Tenary(x) => {
                self.resolve_expr(self.ast[x].cond)?;
                self.resolve_expr(self.ast[x].then)?;
                self.resolve_expr(self.ast[x].r#else)?;
            }
            ast::Expr::TaggedTemplate { tag, mut template } => {
                self.resolve_expr(tag)?;
                while let ast::Template::Head { expr, next, .. } = self.ast[template] {
                    self.resolve_exprs(expr)?;
                    template = next;
                }
            }
            ast::Expr::Destructure { pattern, expr } => {
                self.resolve_binding_pattern(BindingKind::Destructure { expr }, pattern)?;
                self.resolve_expr(expr)?;
            }
        }
        Ok(())
    }

    pub fn resolve_assign_expr(
        &mut self,
        expr: NodeId<ast::Expr>,
        from: NodeId<ast::Expr>,
    ) -> Result<()> {
        match self.ast[expr] {
            ast::Expr::Binary { .. }
            | ast::Expr::Prefix { .. }
            | ast::Expr::Postfix { .. }
            | ast::Expr::Tenary(_)
            | ast::Expr::Call { .. }
            | ast::Expr::TaggedTemplate { .. } => unreachable!(),
            ast::Expr::Index { index, expr } => {
                self.resolve_expr(expr)?;
                self.resolve_expr(index)
            }
            ast::Expr::Dot { ident, expr } => self.resolve_expr(expr),
            ast::Expr::Prime { expr } => match self.ast[expr] {
                ast::PrimeExpr::Number(_)
                | ast::PrimeExpr::String(_)
                | ast::PrimeExpr::Template(_)
                | ast::PrimeExpr::Regex(_)
                | ast::PrimeExpr::Boolean(_)
                | ast::PrimeExpr::Function(_)
                | ast::PrimeExpr::Class(_)
                | ast::PrimeExpr::Object(_)
                | ast::PrimeExpr::Array(_)
                | ast::PrimeExpr::NewTarget
                | ast::PrimeExpr::Null
                | ast::PrimeExpr::This
                | ast::PrimeExpr::Super
                | ast::PrimeExpr::Covered(_) => unreachable!(),
                ast::PrimeExpr::Ident(s) => {
                    self.store(s);
                    Ok(())
                }
            },
            ast::Expr::Yield { star, expr } => todo!(),
            ast::Expr::Destructure { pattern, expr } => {
                self.resolve_expr(expr)?;
                self.resolve_binding_pattern(BindingKind::Destructure { expr }, pattern)
            }
        }
    }
}
