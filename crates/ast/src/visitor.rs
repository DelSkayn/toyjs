use crate::{
    Argument, ArrayLiteralEntry, Ast, BindingElement, BindingPattern, BindingProperty, CaseItem,
    CatchStmt, Class, ClassMember, CstyleDecl, Expr, ForLoopHead, Function, IdentOrPattern,
    InOfDecl, ListHead, ListId, NodeId, NodeList, ObjectLiteral, PrimeExpr, PropertyDefinition,
    PropertyName, Stmt, Symbol, Template, Tenary, VariableDecl,
};

macro_rules! visit_list {
    ($this:expr=>$method:ident($value:expr)) => {{
        let mut cur = $value;
        loop {
            $this.$method($this.ast()[cur].item)?;
            let Some(next) = $this.ast()[cur].next else {
                return Ok(());
            };
            cur = next;
        }
    }};
}

pub trait VisitValue: Sized {
    fn none() -> Self {
        panic!("None not implemented");
    }
}

impl VisitValue for () {
    fn none() -> Self {}
}

pub trait Visitor<E> {
    fn ast(&self) -> &Ast;

    fn super_stmt_list(&mut self, stmt: ListId<Stmt>) -> Result<(), E> {
        self.visit_stmt_list(stmt)
    }

    fn visit_stmt_list(&mut self, stmt: ListId<Stmt>) -> Result<(), E> {
        visit_list!(self=>super_stmt(stmt));
    }

    fn super_variable_decl_list(&mut self, decl: ListId<VariableDecl>) -> Result<(), E> {
        self.visit_variable_decl_list(decl)
    }

    fn visit_variable_decl_list(&mut self, decl: ListId<VariableDecl>) -> Result<(), E> {
        visit_list!(self=>super_variable_decl(decl));
    }

    fn super_expr_list(&mut self, expr: ListId<Expr>) -> Result<(), E> {
        self.visit_expr_list(expr)
    }

    fn visit_expr_list(&mut self, expr: ListId<Expr>) -> Result<(), E> {
        visit_list!(self=>super_expr(expr));
    }

    fn super_binding_property_list(&mut self, prop: ListId<BindingProperty>) -> Result<(), E> {
        self.visit_binding_property_list(prop)
    }

    fn visit_binding_property_list(&mut self, prop: ListId<BindingProperty>) -> Result<(), E> {
        visit_list!(self=>super_binding_property(prop));
    }

    fn super_element_list(
        &mut self,
        elem: NodeId<NodeList<Option<NodeId<BindingElement>>>>,
    ) -> Result<(), E> {
        self.visit_element_list(elem)
    }

    fn visit_element_list(
        &mut self,
        mut elem: NodeId<NodeList<Option<NodeId<BindingElement>>>>,
    ) -> Result<(), E> {
        loop {
            if let Some(elem) = self.ast()[elem].data {
                self.super_binding_element(elem)?;
            }
            let Some(n) = self.ast()[elem].next else {
                return Ok(());
            };
            elem = n;
        }
    }

    fn super_binding_element_list(&mut self, elem: ListId<BindingElement>) -> Result<(), E> {
        self.visit_binding_element_list(elem)
    }

    fn visit_binding_element_list(&mut self, elem: ListId<BindingElement>) -> Result<(), E> {
        visit_list!(self=>super_binding_element(elem));
    }

    fn super_argument_list(&mut self, args: NodeId<NodeList<Argument>>) -> Result<(), E> {
        self.visit_argument_list(args)
    }

    fn visit_argument_list(&mut self, mut args: NodeId<NodeList<Argument>>) -> Result<(), E> {
        loop {
            self.super_expr(self.ast()[args].data.expr)?;
            let Some(n) = self.ast()[args].next else {
                return Ok(());
            };
            args = n;
        }
    }

    fn super_cases(&mut self, case: ListId<CaseItem>) -> Result<(), E> {
        self.visit_cases(case)
    }

    fn visit_cases(&mut self, case: ListId<CaseItem>) -> Result<(), E> {
        visit_list!(self=>visit_case(case));
    }

    fn super_stmt(&mut self, stmt: NodeId<Stmt>) -> Result<(), E> {
        self.visit_stmt(stmt)
    }

    fn visit_stmt(&mut self, stmt: NodeId<Stmt>) -> Result<(), E> {
        match self.ast()[stmt] {
            Stmt::Block { list } => {
                if let ListHead::Present(x) = list {
                    self.super_stmt_list(x)?;
                }
            }
            Stmt::VariableDecl { decl, .. } => self.super_variable_decl_list(decl)?,
            Stmt::Empty => {}
            Stmt::Expr { expr } => self.super_expr_list(expr)?,
            Stmt::DoWhile { body, cond } => {
                self.super_stmt(body)?;
                self.super_expr_list(cond)?
            }
            Stmt::If { cond, body, r#else } => {
                self.super_expr_list(cond)?;
                self.super_stmt(body)?;
                if let Some(r#else) = r#else {
                    self.super_stmt(r#else)?;
                }
            }
            Stmt::While { cond, body } => {
                self.super_expr_list(cond)?;
                self.super_stmt(body)?;
            }
            Stmt::For { head, body } => {
                self.super_head_pre(head)?;
                self.super_stmt(body)?;
                self.super_head_post(head)?;
            }
            Stmt::Switch {
                cond,
                cases,
                default,
            } => {
                self.super_expr_list(cond)?;
                if let ListHead::Present(x) = cases {
                    self.super_cases(x)?;
                }
                if let Some(ListHead::Present(default)) = default {
                    self.super_stmt_list(default)?;
                }
            }
            Stmt::Throw { expr } => self.super_expr_list(expr)?,
            Stmt::Try {
                block,
                catch,
                finally,
            } => {
                if let ListHead::Present(x) = block {
                    self.super_stmt_list(x)?;
                }
                if let Some(x) = catch {
                    self.super_catch(x)?;
                }
                if let Some(ListHead::Present(x)) = finally {
                    self.super_stmt_list(x)?;
                }
            }
            Stmt::With { expr, stmt } => {
                self.super_expr_list(expr)?;
                self.super_stmt(stmt)?;
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {}
            Stmt::Return { expr } => {
                if let Some(x) = expr {
                    self.super_expr_list(x)?;
                }
            }
            Stmt::Labeled { stmt, .. } => self.super_stmt(stmt)?,
            Stmt::Function { func } => self.super_function(func)?,
            Stmt::Class { class } => self.super_class(class)?,
            Stmt::Debugger => {}
        }
        Ok(())
    }

    fn super_expr(&mut self, expr: NodeId<Expr>) -> Result<(), E> {
        self.visit_expr(expr)
    }

    fn visit_expr(&mut self, expr: NodeId<Expr>) -> Result<(), E> {
        match self.ast()[expr] {
            Expr::Binary { left, right, .. } => {
                self.super_expr(left)?;
                self.super_expr(right)?;
            }
            Expr::Prefix { expr, .. } => {
                self.super_expr(expr)?;
            }
            Expr::Postfix { expr, .. } => {
                self.super_expr(expr)?;
            }
            Expr::Tenary(tenary) => {
                self.super_tenary(tenary)?;
            }
            Expr::Index { index, expr } => {
                self.super_expr(expr)?;
                self.super_expr(index)?;
            }
            Expr::Dot { expr, .. } => self.super_expr(expr)?,
            Expr::Call { args, expr } => {
                self.super_expr(expr)?;
                if let Some(a) = args {
                    self.super_argument_list(a)?;
                }
            }
            Expr::Prime { expr } => {
                self.super_prime_expr(expr)?;
            }
            Expr::Yield { expr, .. } => {
                self.super_expr(expr)?;
            }
            Expr::Destructure { expr, .. } => {
                self.super_expr(expr)?;
            }
            Expr::TaggedTemplate { template, .. } => {
                self.super_expr(expr)?;
                self.super_template(template)?;
            }
        }
        Ok(())
    }

    fn super_catch(&mut self, catch: NodeId<CatchStmt>) -> Result<(), E> {
        self.visit_catch(catch)
    }

    fn visit_catch(&mut self, catch: NodeId<CatchStmt>) -> Result<(), E> {
        if let Some(binding) = self.ast()[catch].binding {
            self.super_ident_or_pattern(binding)?;
        }
        if let ListHead::Present(b) = self.ast()[catch].block {
            self.super_stmt_list(b)?;
        }
        Ok(())
    }

    fn super_ident_or_pattern(&mut self, decl: NodeId<IdentOrPattern>) -> Result<(), E> {
        self.visit_ident_or_pattern(decl)
    }

    fn visit_ident_or_pattern(&mut self, decl: NodeId<IdentOrPattern>) -> Result<(), E> {
        match self.ast()[decl] {
            IdentOrPattern::Ident(x) => {
                self.super_symbol(x)?;
            }
            IdentOrPattern::Pattern(x) => {
                self.super_binding_pattern(x)?;
            }
        }
        Ok(())
    }

    fn super_symbol(&mut self, _s: NodeId<Symbol>) -> Result<(), E> {
        Ok(())
    }

    fn super_binding_property(&mut self, prop: NodeId<BindingProperty>) -> Result<(), E> {
        self.visit_binding_property(prop)
    }

    fn visit_binding_property(&mut self, prop: NodeId<BindingProperty>) -> Result<(), E> {
        match self.ast()[prop] {
            BindingProperty::Binding {
                symbol,
                initializer,
            } => {
                if let Some(init) = initializer {
                    self.super_expr(init)?;
                }
                self.super_symbol(symbol)?;
            }
            BindingProperty::Property { name, element } => {
                self.super_binding_element(element)?;
                {
                    match name {
                        PropertyName::Ident(_)
                        | PropertyName::String(_)
                        | PropertyName::Number(_) => {}
                        PropertyName::Computed(expr) => self.super_expr(expr)?,
                    }
                }
            }
        }
        Ok(())
    }

    fn super_binding_pattern(&mut self, pat: NodeId<BindingPattern>) -> Result<(), E> {
        self.visit_binding_pattern(pat)
    }

    fn visit_binding_pattern(&mut self, pat: NodeId<BindingPattern>) -> Result<(), E> {
        match self.ast()[pat] {
            BindingPattern::Object { properties, rest } => {
                if let Some(r) = rest {
                    self.super_symbol(r)?;
                }
                if let ListHead::Present(x) = properties {
                    self.super_binding_property_list(x)?;
                }
            }
            BindingPattern::Array { elements, rest } => {
                if let Some(r) = rest {
                    self.super_ident_or_pattern(r)?;
                }
                if let Some(elem) = elements {
                    self.super_element_list(elem)?;
                }
            }
        }
        Ok(())
    }

    fn super_binding_element(&mut self, elem: NodeId<BindingElement>) -> Result<(), E> {
        self.visit_binding_element(elem)
    }

    fn visit_binding_element(&mut self, elem: NodeId<BindingElement>) -> Result<(), E> {
        match self.ast()[elem] {
            BindingElement::SingleName {
                symbol,
                initializer,
            } => {
                if let Some(init) = initializer {
                    self.super_expr(init)?;
                }
                self.super_symbol(symbol)?;
            }
            BindingElement::Pattern {
                pattern,
                initializer,
            } => {
                if let Some(init) = initializer {
                    self.super_expr(init)?;
                }
                self.super_binding_pattern(pattern)?;
            }
        }
        Ok(())
    }

    fn super_variable_decl(&mut self, decl: NodeId<VariableDecl>) -> Result<(), E> {
        self.visit_variable_decl(decl)
    }

    fn visit_variable_decl(&mut self, decl: NodeId<VariableDecl>) -> Result<(), E> {
        if let Some(x) = self.ast()[decl].initializer {
            self.super_expr(x)?;
        }
        self.super_ident_or_pattern(self.ast()[decl].decl)?;
        Ok(())
    }

    fn super_head_pre(&mut self, head: NodeId<ForLoopHead>) -> Result<(), E> {
        self.visit_head_pre(head)
    }

    fn visit_head_pre(&mut self, head: NodeId<ForLoopHead>) -> Result<(), E> {
        match self.ast()[head] {
            ForLoopHead::CStyle { decl, cond, .. } => {
                match decl {
                    CstyleDecl::Expr(x) => {
                        self.super_expr_list(x)?;
                    }
                    CstyleDecl::Decl { decl, .. } => {
                        self.super_variable_decl_list(decl)?;
                    }
                    CstyleDecl::Empty => todo!(),
                }
                if let Some(cond) = cond {
                    self.super_expr_list(cond)?;
                }
            }
            ForLoopHead::In { decl, expr } => {
                match decl {
                    InOfDecl::Expr(x) => self.super_expr(x)?,
                    InOfDecl::Decl { binding, .. } => self.super_ident_or_pattern(binding)?,
                }
                self.super_expr_list(expr)?;
            }
            ForLoopHead::Of { decl, expr } => {
                match decl {
                    InOfDecl::Expr(x) => self.super_expr(x)?,
                    InOfDecl::Decl { binding, .. } => self.super_ident_or_pattern(binding)?,
                }
                self.super_expr(expr)?;
            }
        }
        todo!()
    }

    fn super_head_post(&mut self, head: NodeId<ForLoopHead>) -> Result<(), E> {
        self.visit_head_post(head)
    }

    fn visit_head_post(&mut self, head: NodeId<ForLoopHead>) -> Result<(), E> {
        match self.ast()[head] {
            ForLoopHead::CStyle { post, .. } => {
                if let Some(post) = post {
                    self.super_expr_list(post)?;
                }
            }
            ForLoopHead::In { .. } | ForLoopHead::Of { .. } => {}
        }
        Ok(())
    }

    fn super_case(&mut self, case: NodeId<CaseItem>) -> Result<(), E> {
        self.visit_case(case)
    }

    fn visit_case(&mut self, case: NodeId<CaseItem>) -> Result<(), E> {
        self.super_expr_list(self.ast()[case].expr)?;
        if let ListHead::Present(stmt) = self.ast()[case].stmts {
            self.super_stmt_list(stmt)?;
        }
        Ok(())
    }

    fn super_function(&mut self, func: NodeId<Function>) -> Result<(), E> {
        self.visit_function(func)
    }

    fn visit_function(&mut self, func: NodeId<Function>) -> Result<(), E> {
        match self.ast()[func] {
            Function::Arrow {
                params,
                rest_param,
                body,
                ..
            } => {
                if let ListHead::Present(param) = params {
                    self.super_binding_element_list(param)?;
                }
                if let Some(rest) = rest_param {
                    self.super_ident_or_pattern(rest)?;
                }
                match body {
                    crate::ArrowFunctionBody::Expr(x) => {
                        self.super_expr(x)?;
                    }
                    crate::ArrowFunctionBody::Stmt(ListHead::Present(x)) => {
                        self.super_stmt_list(x)?;
                    }
                    _ => {}
                }
                Ok(())
            }
            Function::Declared {
                params,
                rest_param,
                body,
                ..
            } => {
                if let ListHead::Present(param) = params {
                    self.super_binding_element_list(param)?;
                }
                if let Some(rest) = rest_param {
                    self.super_ident_or_pattern(rest)?;
                }
                if let ListHead::Present(x) = body {
                    self.super_stmt_list(x)?;
                }
                Ok(())
            }
            Function::Expr {
                params,
                rest_param,
                body,
                ..
            } => {
                if let ListHead::Present(param) = params {
                    self.super_binding_element_list(param)?;
                }
                if let Some(rest) = rest_param {
                    self.super_ident_or_pattern(rest)?;
                }
                if let ListHead::Present(x) = body {
                    self.super_stmt_list(x)?;
                }
                Ok(())
            }
        }
    }

    fn super_class(&mut self, cls: NodeId<Class>) -> Result<(), E> {
        self.visit_class(cls)
    }

    fn visit_class(&mut self, cls: NodeId<Class>) -> Result<(), E> {
        if let Some(name) = self.ast()[cls].name {
            self.super_symbol(name)?;
        }
        if let Some(heritage) = self.ast()[cls].heritage {
            self.super_expr(heritage)?;
        }
        if let ListHead::Present(mem) = self.ast()[cls].body {
            self.super_class_member_list(mem)?;
        }
        Ok(())
    }

    fn super_class_member_list(&mut self, cls_mem: ListId<ClassMember>) -> Result<(), E> {
        self.visit_class_member_list(cls_mem)
    }

    fn visit_class_member_list(&mut self, cls_mem: ListId<ClassMember>) -> Result<(), E> {
        visit_list!(self=>super_class_member(cls_mem));
    }

    fn super_class_member(&mut self, cls_mem: NodeId<ClassMember>) -> Result<(), E> {
        self.visit_class_member(cls_mem)
    }

    fn visit_class_member(&mut self, cls_mem: NodeId<ClassMember>) -> Result<(), E> {
        match self.ast()[cls_mem] {
            ClassMember::StaticBlock { stmts } => {
                if let ListHead::Present(x) = stmts {
                    self.super_stmt_list(x)?;
                }
            }
            ClassMember::Method { property, func, .. } => {
                self.super_function(func)?;
                match property {
                    PropertyName::Ident(_) | PropertyName::String(_) | PropertyName::Number(_) => {}
                    PropertyName::Computed(x) => {
                        self.super_expr(x)?;
                    }
                }
            }
            ClassMember::Field {
                property,
                initializer,
                ..
            } => {
                if let Some(init) = initializer {
                    self.super_expr(init)?;
                }
                match property {
                    PropertyName::Ident(_) | PropertyName::String(_) | PropertyName::Number(_) => {}
                    PropertyName::Computed(x) => {
                        self.super_expr(x)?;
                    }
                }
            }
            ClassMember::Getter { property, func, .. } => {
                self.super_function(func)?;
                match property {
                    PropertyName::Ident(_) | PropertyName::String(_) | PropertyName::Number(_) => {}
                    PropertyName::Computed(x) => {
                        self.super_expr(x)?;
                    }
                }
            }
            ClassMember::Setter { property, func, .. } => {
                self.super_function(func)?;
                match property {
                    PropertyName::Ident(_) | PropertyName::String(_) | PropertyName::Number(_) => {}
                    PropertyName::Computed(x) => {
                        self.super_expr(x)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn super_tenary(&mut self, ten: NodeId<Tenary>) -> Result<(), E> {
        self.visit_tenary(ten)
    }

    fn visit_tenary(&mut self, ten: NodeId<Tenary>) -> Result<(), E> {
        self.super_expr(self.ast()[ten].cond)?;
        self.super_expr(self.ast()[ten].then)?;
        self.super_expr(self.ast()[ten].r#else)?;
        Ok(())
    }

    fn super_prime_expr(&mut self, expr: NodeId<PrimeExpr>) -> Result<(), E> {
        self.visit_prime_expr(expr)
    }

    fn visit_prime_expr(&mut self, expr: NodeId<PrimeExpr>) -> Result<(), E> {
        match self.ast()[expr] {
            PrimeExpr::Number(_) => {}
            PrimeExpr::String(_) => {}
            PrimeExpr::Template(tem) => self.super_template(tem)?,
            PrimeExpr::Regex(_) => {}
            PrimeExpr::Ident(s) => self.super_symbol(s)?,
            PrimeExpr::Boolean(_) => {}
            PrimeExpr::Function(func) => self.super_function(func)?,
            PrimeExpr::Class(cls) => self.super_class(cls)?,
            PrimeExpr::Object(obj) => match obj {
                ObjectLiteral::Empty => {}
                ObjectLiteral::Item(prop) => {
                    self.super_propery_definition_list(prop)?;
                }
            },
            PrimeExpr::Array(array) => {
                if let ListHead::Present(x) = array {
                    self.super_array_literal_entry_list(x)?;
                }
            }
            PrimeExpr::NewTarget => {}
            PrimeExpr::Null => {}
            PrimeExpr::This => {}
            PrimeExpr::Super => {}
            PrimeExpr::Covered(expr) => self.super_expr_list(expr)?,
        }
        Ok(())
    }

    fn super_propery_definition_list(&mut self, prop: ListId<PropertyDefinition>) -> Result<(), E> {
        self.visit_propery_definition_list(prop)
    }

    fn visit_propery_definition_list(&mut self, prop: ListId<PropertyDefinition>) -> Result<(), E> {
        visit_list!(self=>visit_propery_definition(prop))
    }

    fn super_propery_definition(&mut self, prop: NodeId<PropertyDefinition>) -> Result<(), E> {
        self.visit_propery_definition(prop)
    }

    fn visit_propery_definition(&mut self, prop: NodeId<PropertyDefinition>) -> Result<(), E> {
        match self.ast()[prop] {
            PropertyDefinition::Ident { .. } => {}
            PropertyDefinition::Covered {
                symbol,
                initializer,
            } => {
                self.super_symbol(symbol)?;
                self.super_expr(initializer)?;
            }
            PropertyDefinition::Define { property, expr } => {
                self.super_expr(expr)?;
                match property {
                    PropertyName::Ident(_) | PropertyName::String(_) | PropertyName::Number(_) => {}
                    PropertyName::Computed(expr) => self.super_expr(expr)?,
                }
            }
            PropertyDefinition::Method { property, func }
            | PropertyDefinition::Getter { property, func }
            | PropertyDefinition::Setter { property, func } => {
                self.super_function(func)?;
                match property {
                    PropertyName::Ident(_) | PropertyName::String(_) | PropertyName::Number(_) => {}
                    PropertyName::Computed(expr) => self.super_expr(expr)?,
                }
            }
            PropertyDefinition::Rest(rest) => {
                self.super_expr(rest)?;
            }
        }
        Ok(())
    }

    fn super_array_literal_entry_list(&mut self, prop: ListId<ArrayLiteralEntry>) -> Result<(), E> {
        self.visit_array_literal_entry_list(prop)
    }

    fn visit_array_literal_entry_list(&mut self, prop: ListId<ArrayLiteralEntry>) -> Result<(), E> {
        visit_list!(self=>visit_array_literal_entry(prop))
    }

    fn super_array_literal_entry(&mut self, entry: NodeId<ArrayLiteralEntry>) -> Result<(), E> {
        self.visit_array_literal_entry(entry)
    }

    fn visit_array_literal_entry(&mut self, entry: NodeId<ArrayLiteralEntry>) -> Result<(), E> {
        if let Some(ent) = self.ast()[entry].expr {
            self.super_expr(ent)?;
        }
        Ok(())
    }

    fn super_template(&mut self, tem: NodeId<Template>) -> Result<(), E> {
        self.visit_template(tem)
    }

    fn visit_template(&mut self, tem: NodeId<Template>) -> Result<(), E> {
        match self.ast()[tem] {
            Template::Head { expr, next, .. } => {
                self.super_expr_list(expr)?;
                self.super_template(next)?;
            }
            Template::Tail { .. } => {}
        }
        Ok(())
    }
}
