use crate::{Compiler, Result};
use ast::{ListId, NodeId};

impl<'a> Compiler<'a> {
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
            ast::Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            ast::Expr::Index { index, expr } => {
                self.resolve_expr(index)?;
                self.resolve_expr(expr)?;
            }
            ast::Expr::Prefix { expr, .. }
            | ast::Expr::Postfix { expr, .. }
            | ast::Expr::Dot { expr, .. }
            | ast::Expr::Call { expr, .. }
            | ast::Expr::Yield { expr, .. } => {
                self.resolve_expr(expr)?;
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
                    self.variables.load(name, expr)?;
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
                        ast::PropertyDefinition::Covered { initializer, .. } => {
                            panic!("A covered object should not make it to the compiler");
                        }
                        ast::PropertyDefinition::Define { expr, .. } => self.resolve_expr(expr)?,
                        ast::PropertyDefinition::Method { func, .. }
                        | ast::PropertyDefinition::Getter { func, .. }
                        | ast::PropertyDefinition::Setter { func, .. } => {
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
                    let mut elements = self.ast[x].elements;
                    while let Some(elem) = elements {
                        if let Some(expr) = self.ast[elem].data {
                            self.resolve_expr(expr)?;
                        }
                        elements = self.ast[elem].next;
                    }
                    if let Some(spread) = self.ast[x].spread {
                        self.resolve_expr(spread)?;
                    }
                }
            },
            ast::Expr::Tenary(x) => {
                self.resolve_expr(self.ast[x].cond)?;
                self.resolve_expr(self.ast[x].then)?;
                self.resolve_expr(self.ast[x].r#else)?;
            }
            ast::Expr::TaggedTemplate { tag, .. } => {
                self.resolve_expr(tag)?;
                // TODO template
            }
            ast::Expr::Destructure { pattern, expr } => to_do!(),
        }
        Ok(())
    }
}
