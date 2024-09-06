use crate::{
    Argument, ArrayLiteralEntry, Ast, BindingElement, BindingPattern, BindingProperty, CaseItem,
    CatchStmt, Class, ClassMember, CstyleDecl, Expr, ForLoopHead, Function, IdentOrPattern,
    InOfDecl, NodeId, NodeList, NodeListId, ObjectLiteral, OptionNodeList, PrimeExpr,
    PropertyDefinition, PropertyName, Stmt, Symbol, Template, Tenary, VariableDecl,
};

macro_rules! visit_list {
    ($this:expr=>$method:ident($value:expr)) => {{
        let mut cur = Some($value);
        while let Some(x) = $this.ast().next_list(&mut cur) {
            $this.$method(x)?;
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

pub trait Visitor {
    type Error;

    fn ast(&self) -> &Ast;

    fn visit_stmt_list(&mut self, stmt: NodeListId<Stmt>) -> Result<(), Self::Error> {
        visit_stmt_list(self, stmt)
    }

    fn visit_variable_decl_list(
        &mut self,
        decl: NodeListId<VariableDecl>,
    ) -> Result<(), Self::Error> {
        visit_variable_decl_list(self, decl)
    }

    fn visit_expr_list(&mut self, expr: NodeListId<Expr>) -> Result<(), Self::Error> {
        visit_expr_list(self, expr)
    }

    fn visit_binding_property_list(
        &mut self,
        prop: NodeListId<BindingProperty>,
    ) -> Result<(), Self::Error> {
        visit_binding_property_list(self, prop)
    }

    fn visit_element_list(
        &mut self,
        elem: NodeId<OptionNodeList<BindingElement>>,
    ) -> Result<(), Self::Error> {
        visit_element_list(self, elem)
    }

    fn visit_binding_element_list(
        &mut self,
        elem: NodeListId<BindingElement>,
    ) -> Result<(), Self::Error> {
        visit_binding_element_list(self, elem)
    }

    fn visit_argument_list(&mut self, args: NodeId<NodeList<Argument>>) -> Result<(), Self::Error> {
        visit_argument_list(self, args)
    }

    fn visit_cases(&mut self, case: NodeListId<CaseItem>) -> Result<(), Self::Error> {
        visit_cases(self, case)
    }

    fn visit_stmt(&mut self, stmt: NodeId<Stmt>) -> Result<(), Self::Error> {
        visit_stmt(self, stmt)
    }

    fn visit_expr(&mut self, expr: NodeId<Expr>) -> Result<(), Self::Error> {
        visit_expr(self, expr)
    }

    fn visit_catch(&mut self, catch: NodeId<CatchStmt>) -> Result<(), Self::Error> {
        visit_catch(self, catch)
    }

    fn visit_ident_or_pattern(&mut self, decl: NodeId<IdentOrPattern>) -> Result<(), Self::Error> {
        visit_ident_or_pattern(self, decl)
    }

    fn visit_symbol(&mut self, _s: NodeId<Symbol>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_binding_property(&mut self, prop: NodeId<BindingProperty>) -> Result<(), Self::Error> {
        visit_binding_property(self, prop)
    }

    fn visit_binding_pattern(&mut self, pat: NodeId<BindingPattern>) -> Result<(), Self::Error> {
        visit_binding_pattern(self, pat)
    }

    fn visit_binding_element(&mut self, elem: NodeId<BindingElement>) -> Result<(), Self::Error> {
        visit_binding_element(self, elem)
    }

    fn visit_variable_decl(&mut self, decl: NodeId<VariableDecl>) -> Result<(), Self::Error> {
        visit_variable_decl(self, decl)
    }

    fn visit_head_pre(&mut self, head: NodeId<ForLoopHead>) -> Result<(), Self::Error> {
        visit_head_pre(self, head)
    }

    fn visit_head_post(&mut self, head: NodeId<ForLoopHead>) -> Result<(), Self::Error> {
        visit_head_post(self, head)
    }

    fn visit_case(&mut self, case: NodeId<CaseItem>) -> Result<(), Self::Error> {
        visit_case(self, case)
    }

    fn visit_function(&mut self, func: NodeId<Function>) -> Result<(), Self::Error> {
        visit_function(self, func)
    }

    fn visit_class(&mut self, cls: NodeId<Class>) -> Result<(), Self::Error> {
        visit_class(self, cls)
    }

    fn visit_class_member_list(
        &mut self,
        cls_mem: NodeListId<ClassMember>,
    ) -> Result<(), Self::Error> {
        visit_class_member_list(self, cls_mem)
    }

    fn visit_class_member(&mut self, cls_mem: NodeId<ClassMember>) -> Result<(), Self::Error> {
        visit_class_member(self, cls_mem)
    }

    fn visit_ternary(&mut self, ten: NodeId<Tenary>) -> Result<(), Self::Error> {
        visit_tenary(self, ten)
    }

    fn visit_prime_expr(&mut self, expr: NodeId<PrimeExpr>) -> Result<(), Self::Error> {
        visit_prime_expr(self, expr)
    }

    fn visit_propery_definition_list(
        &mut self,
        prop: NodeListId<PropertyDefinition>,
    ) -> Result<(), Self::Error> {
        visit_propery_definition_list(self, prop)
    }

    fn visit_propery_definition(
        &mut self,
        prop: NodeId<PropertyDefinition>,
    ) -> Result<(), Self::Error> {
        visit_propery_definition(self, prop)
    }

    fn visit_array_literal_entry_list(
        &mut self,
        prop: NodeListId<ArrayLiteralEntry>,
    ) -> Result<(), Self::Error> {
        visit_array_literal_entry_list(self, prop)
    }
    fn visit_array_literal_entry(
        &mut self,
        entry: NodeId<ArrayLiteralEntry>,
    ) -> Result<(), Self::Error> {
        visit_array_literal_entry(self, entry)
    }

    fn visit_template(&mut self, tem: NodeId<Template>) -> Result<(), Self::Error> {
        visit_template(self, tem)
    }
}

pub fn visit_stmt_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    stmt: NodeListId<Stmt>,
) -> Result<(), V::Error> {
    visit_list!(visitor=>visit_stmt(stmt));
    Ok(())
}

pub fn visit_variable_decl_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    decl: NodeListId<VariableDecl>,
) -> Result<(), V::Error> {
    visit_list!(visitor=>visit_variable_decl(decl));
    Ok(())
}

pub fn visit_expr_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    expr: NodeListId<Expr>,
) -> Result<(), V::Error> {
    visit_list!(visitor=>visit_expr(expr));
    Ok(())
}

pub fn visit_binding_property_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    prop: NodeListId<BindingProperty>,
) -> Result<(), V::Error> {
    visit_list!(visitor=>visit_binding_property(prop));
    Ok(())
}

pub fn visit_element_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    mut elem: NodeId<OptionNodeList<BindingElement>>,
) -> Result<(), V::Error> {
    loop {
        if let Some(elem) = visitor.ast()[elem].item {
            visitor.visit_binding_element(elem)?;
        }
        let Some(n) = visitor.ast()[elem].next else {
            return Ok(());
        };
        elem = n;
    }
}

pub fn visit_binding_element_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    elem: NodeListId<BindingElement>,
) -> Result<(), V::Error> {
    visit_list!(visitor=>visit_binding_element(elem));
    Ok(())
}

pub fn visit_argument_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    args: NodeId<NodeList<Argument>>,
) -> Result<(), V::Error> {
    let mut cur = Some(args);
    while let Some(item) = visitor.ast().next_list(&mut cur) {
        visitor.visit_expr(item.index(visitor.ast()).expr)?;
    }
    Ok(())
}

pub fn visit_cases<V: Visitor + ?Sized>(
    visitor: &mut V,
    case: NodeListId<CaseItem>,
) -> Result<(), V::Error> {
    visit_list!(visitor=>visit_case(case));
    Ok(())
}

pub fn visit_stmt<V: Visitor + ?Sized>(
    visitor: &mut V,
    stmt: NodeId<Stmt>,
) -> Result<(), V::Error> {
    match visitor.ast()[stmt] {
        Stmt::Block { list } => {
            if let Some(x) = list {
                visitor.visit_stmt_list(x)?;
            }
        }
        Stmt::VariableDecl { decl, .. } => visitor.visit_variable_decl_list(decl)?,
        Stmt::Empty => {}
        Stmt::Expr { expr } => visitor.visit_expr_list(expr)?,
        Stmt::DoWhile { body, cond } => {
            visitor.visit_stmt(body)?;
            visitor.visit_expr_list(cond)?
        }
        Stmt::If { cond, body, r#else } => {
            visitor.visit_expr_list(cond)?;
            visitor.visit_stmt(body)?;
            if let Some(r#else) = r#else {
                visitor.visit_stmt(r#else)?;
            }
        }
        Stmt::While { cond, body } => {
            visitor.visit_expr_list(cond)?;
            visitor.visit_stmt(body)?;
        }
        Stmt::For { head, body } => {
            visitor.visit_head_pre(head)?;
            visitor.visit_stmt(body)?;
            visitor.visit_head_post(head)?;
        }
        Stmt::Switch {
            cond,
            cases,
            default,
        } => {
            visitor.visit_expr_list(cond)?;
            if let Some(x) = cases {
                visitor.visit_cases(x)?;
            }
            if let Some(default) = default {
                visitor.visit_stmt_list(default)?;
            }
        }
        Stmt::Throw { expr } => visitor.visit_expr_list(expr)?,
        Stmt::Try {
            block,
            catch,
            finally,
        } => {
            if let Some(x) = block {
                visitor.visit_stmt_list(x)?;
            }
            if let Some(x) = catch {
                visitor.visit_catch(x)?;
            }
            if let Some(x) = finally {
                visitor.visit_stmt_list(x)?;
            }
        }
        Stmt::With { expr, stmt } => {
            visitor.visit_expr_list(expr)?;
            visitor.visit_stmt(stmt)?;
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
        Stmt::Return { expr } => {
            if let Some(x) = expr {
                visitor.visit_expr_list(x)?;
            }
        }
        Stmt::Labeled { stmt, .. } => visitor.visit_stmt(stmt)?,
        Stmt::Function { func } => visitor.visit_function(func)?,
        Stmt::Class { class } => visitor.visit_class(class)?,
        Stmt::Debugger => {}
    }
    Ok(())
}

pub fn visit_expr<V: Visitor + ?Sized>(
    visitor: &mut V,
    expr: NodeId<Expr>,
) -> Result<(), V::Error> {
    match visitor.ast()[expr] {
        Expr::Binary { left, right, .. } => {
            visitor.visit_expr(left)?;
            visitor.visit_expr(right)?;
        }
        Expr::Prefix { expr, .. } => {
            visitor.visit_expr(expr)?;
        }
        Expr::Postfix { expr, .. } => {
            visitor.visit_expr(expr)?;
        }
        Expr::Ternary { ternary } => {
            visitor.visit_ternary(ternary)?;
        }
        Expr::Index { index, expr } => {
            visitor.visit_expr(expr)?;
            visitor.visit_expr(index)?;
        }
        Expr::Dot { expr, .. } => visitor.visit_expr(expr)?,
        Expr::Call { args, expr } => {
            visitor.visit_expr(expr)?;
            if let Some(a) = args {
                visitor.visit_argument_list(a)?;
            }
        }
        Expr::Prime { expr } => {
            visitor.visit_prime_expr(expr)?;
        }
        Expr::Yield { expr, .. } => {
            visitor.visit_expr(expr)?;
        }
        Expr::Destructure { expr, pattern } => {
            visitor.visit_expr(expr)?;
            visitor.visit_binding_pattern(pattern)?;
        }
        Expr::TaggedTemplate { template, tag } => {
            visitor.visit_expr(tag)?;
            visitor.visit_template(template)?;
        }
    }
    Ok(())
}

pub fn visit_catch<V: Visitor + ?Sized>(
    visitor: &mut V,
    catch: NodeId<CatchStmt>,
) -> Result<(), V::Error> {
    if let Some(binding) = visitor.ast()[catch].binding {
        visitor.visit_ident_or_pattern(binding)?;
    }
    if let Some(b) = visitor.ast()[catch].block {
        visitor.visit_stmt_list(b)?;
    }
    Ok(())
}

pub fn visit_ident_or_pattern<V: Visitor + ?Sized>(
    visitor: &mut V,
    decl: NodeId<IdentOrPattern>,
) -> Result<(), V::Error> {
    match visitor.ast()[decl] {
        IdentOrPattern::Ident { name } => {
            visitor.visit_symbol(name)?;
        }
        IdentOrPattern::Pattern { pattern } => {
            visitor.visit_binding_pattern(pattern)?;
        }
    }
    Ok(())
}

pub fn visit_binding_property<V: Visitor + ?Sized>(
    visitor: &mut V,
    prop: NodeId<BindingProperty>,
) -> Result<(), V::Error> {
    match visitor.ast()[prop] {
        BindingProperty::Binding {
            symbol,
            initializer,
        } => {
            if let Some(init) = initializer {
                visitor.visit_expr(init)?;
            }
            visitor.visit_symbol(symbol)?;
        }
        BindingProperty::Property { name, element } => {
            visitor.visit_binding_element(element)?;
            {
                match name {
                    PropertyName::Ident { .. }
                    | PropertyName::String { .. }
                    | PropertyName::Number { .. } => {}
                    PropertyName::Computed { expr } => visitor.visit_expr(expr)?,
                }
            }
        }
    }
    Ok(())
}

pub fn visit_binding_pattern<V: Visitor + ?Sized>(
    visitor: &mut V,
    pat: NodeId<BindingPattern>,
) -> Result<(), V::Error> {
    match visitor.ast()[pat] {
        BindingPattern::Object { properties, rest } => {
            if let Some(r) = rest {
                visitor.visit_symbol(r)?;
            }
            if let Some(x) = properties {
                visitor.visit_binding_property_list(x)?;
            }
        }
        BindingPattern::Array { elements, rest } => {
            if let Some(r) = rest {
                visitor.visit_ident_or_pattern(r)?;
            }
            if let Some(elem) = elements {
                visitor.visit_element_list(elem)?;
            }
        }
    }
    Ok(())
}

pub fn visit_binding_element<V: Visitor + ?Sized>(
    visitor: &mut V,
    elem: NodeId<BindingElement>,
) -> Result<(), V::Error> {
    match visitor.ast()[elem] {
        BindingElement::SingleName {
            symbol,
            initializer,
        } => {
            if let Some(init) = initializer {
                visitor.visit_expr(init)?;
            }
            visitor.visit_symbol(symbol)?;
        }
        BindingElement::Pattern {
            pattern,
            initializer,
        } => {
            if let Some(init) = initializer {
                visitor.visit_expr(init)?;
            }
            visitor.visit_binding_pattern(pattern)?;
        }
    }
    Ok(())
}

pub fn visit_variable_decl<V: Visitor + ?Sized>(
    visitor: &mut V,
    decl: NodeId<VariableDecl>,
) -> Result<(), V::Error> {
    if let Some(x) = visitor.ast()[decl].initializer {
        visitor.visit_expr(x)?;
    }
    visitor.visit_ident_or_pattern(visitor.ast()[decl].decl)?;
    Ok(())
}

pub fn visit_head_pre<V: Visitor + ?Sized>(
    visitor: &mut V,
    head: NodeId<ForLoopHead>,
) -> Result<(), V::Error> {
    match visitor.ast()[head] {
        ForLoopHead::CStyle { decl, cond, .. } => {
            match decl {
                CstyleDecl::Expr { expr } => {
                    visitor.visit_expr_list(expr)?;
                }
                CstyleDecl::Decl { decl, .. } => {
                    visitor.visit_variable_decl_list(decl)?;
                }
                CstyleDecl::Empty => {}
            }
            if let Some(cond) = cond {
                visitor.visit_expr_list(cond)?;
            }
        }
        ForLoopHead::In { decl, expr } => {
            match decl {
                InOfDecl::Expr { expr } => visitor.visit_expr(expr)?,
                InOfDecl::Decl { binding, .. } => visitor.visit_ident_or_pattern(binding)?,
            }
            visitor.visit_expr_list(expr)?;
        }
        ForLoopHead::Of { decl, expr } => {
            match decl {
                InOfDecl::Expr { expr } => visitor.visit_expr(expr)?,
                InOfDecl::Decl { binding, .. } => visitor.visit_ident_or_pattern(binding)?,
            }
            visitor.visit_expr(expr)?;
        }
    }
    Ok(())
}

pub fn visit_head_post<V: Visitor + ?Sized>(
    visitor: &mut V,
    head: NodeId<ForLoopHead>,
) -> Result<(), V::Error> {
    match visitor.ast()[head] {
        ForLoopHead::CStyle { post, .. } => {
            if let Some(post) = post {
                visitor.visit_expr_list(post)?;
            }
        }
        ForLoopHead::In { .. } | ForLoopHead::Of { .. } => {}
    }
    Ok(())
}

pub fn visit_case<V: Visitor + ?Sized>(
    visitor: &mut V,
    case: NodeId<CaseItem>,
) -> Result<(), V::Error> {
    visitor.visit_expr_list(visitor.ast()[case].expr)?;
    if let Some(stmt) = visitor.ast()[case].stmts {
        visitor.visit_stmt_list(stmt)?;
    }
    Ok(())
}

pub fn visit_function<V: Visitor + ?Sized>(
    visitor: &mut V,
    func: NodeId<Function>,
) -> Result<(), V::Error> {
    match visitor.ast()[func] {
        Function::Arrow {
            params,
            rest_param,
            body,
            ..
        } => {
            if let Some(param) = params {
                visitor.visit_binding_element_list(param)?;
            }
            if let Some(rest) = rest_param {
                visitor.visit_ident_or_pattern(rest)?;
            }
            match body {
                crate::ArrowFunctionBody::Expr { expr } => {
                    visitor.visit_expr(expr)?;
                }
                crate::ArrowFunctionBody::Stmt { body: Some(x) } => {
                    visitor.visit_stmt_list(x)?;
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
            if let Some(param) = params {
                visitor.visit_binding_element_list(param)?;
            }
            if let Some(rest) = rest_param {
                visitor.visit_ident_or_pattern(rest)?;
            }
            if let Some(x) = body {
                visitor.visit_stmt_list(x)?;
            }
            Ok(())
        }
        Function::Expr {
            params,
            rest_param,
            body,
            ..
        } => {
            if let Some(param) = params {
                visitor.visit_binding_element_list(param)?;
            }
            if let Some(rest) = rest_param {
                visitor.visit_ident_or_pattern(rest)?;
            }
            if let Some(x) = body {
                visitor.visit_stmt_list(x)?;
            }
            Ok(())
        }
    }
}

pub fn visit_class<V: Visitor + ?Sized>(
    visitor: &mut V,
    cls: NodeId<Class>,
) -> Result<(), V::Error> {
    if let Some(name) = visitor.ast()[cls].name {
        visitor.visit_symbol(name)?;
    }
    if let Some(heritage) = visitor.ast()[cls].heritage {
        visitor.visit_expr(heritage)?;
    }
    if let Some(mem) = visitor.ast()[cls].body {
        visitor.visit_class_member_list(mem)?;
    }
    Ok(())
}

pub fn visit_class_member_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    cls_mem: NodeListId<ClassMember>,
) -> Result<(), V::Error> {
    visit_list!(visitor=>visit_class_member(cls_mem));
    Ok(())
}

pub fn visit_class_member<V: Visitor + ?Sized>(
    visitor: &mut V,
    cls_mem: NodeId<ClassMember>,
) -> Result<(), V::Error> {
    match visitor.ast()[cls_mem] {
        ClassMember::StaticBlock { stmts } => {
            if let Some(x) = stmts {
                visitor.visit_stmt_list(x)?;
            }
        }
        ClassMember::Method { property, func, .. } => {
            visitor.visit_function(func)?;
            match property {
                PropertyName::Ident { .. }
                | PropertyName::String { .. }
                | PropertyName::Number { .. } => {}
                PropertyName::Computed { expr } => {
                    visitor.visit_expr(expr)?;
                }
            }
        }
        ClassMember::Field {
            property,
            initializer,
            ..
        } => {
            if let Some(init) = initializer {
                visitor.visit_expr(init)?;
            }
            match property {
                PropertyName::Ident { .. }
                | PropertyName::String { .. }
                | PropertyName::Number { .. } => {}
                PropertyName::Computed { expr } => {
                    visitor.visit_expr(expr)?;
                }
            }
        }
        ClassMember::Getter { property, func, .. } => {
            visitor.visit_function(func)?;
            match property {
                PropertyName::Ident { .. }
                | PropertyName::String { .. }
                | PropertyName::Number { .. } => {}
                PropertyName::Computed { expr } => {
                    visitor.visit_expr(expr)?;
                }
            }
        }
        ClassMember::Setter { property, func, .. } => {
            visitor.visit_function(func)?;
            match property {
                PropertyName::Ident { .. }
                | PropertyName::String { .. }
                | PropertyName::Number { .. } => {}
                PropertyName::Computed { expr } => {
                    visitor.visit_expr(expr)?;
                }
            }
        }
    }
    Ok(())
}

pub fn visit_tenary<V: Visitor + ?Sized>(
    visitor: &mut V,
    ten: NodeId<Tenary>,
) -> Result<(), V::Error> {
    visitor.visit_expr(visitor.ast()[ten].cond)?;
    visitor.visit_expr(visitor.ast()[ten].then)?;
    visitor.visit_expr(visitor.ast()[ten].r#else)?;
    Ok(())
}

pub fn visit_prime_expr<V: Visitor + ?Sized>(
    visitor: &mut V,
    expr: NodeId<PrimeExpr>,
) -> Result<(), V::Error> {
    match visitor.ast()[expr] {
        PrimeExpr::Number { .. } => {}
        PrimeExpr::String { .. } => {}
        PrimeExpr::Template { template } => visitor.visit_template(template)?,
        PrimeExpr::Regex { .. } => {}
        PrimeExpr::Ident { symbol } => visitor.visit_symbol(symbol)?,
        PrimeExpr::Boolean { .. } => {}
        PrimeExpr::Function { function } => visitor.visit_function(function)?,
        PrimeExpr::Class { class } => visitor.visit_class(class)?,
        PrimeExpr::Object { object } => match object {
            ObjectLiteral::Empty => {}
            ObjectLiteral::Item { definition } => {
                visitor.visit_propery_definition_list(definition)?;
            }
        },
        PrimeExpr::Array { array } => {
            if let Some(x) = array {
                visitor.visit_array_literal_entry_list(x)?;
            }
        }
        PrimeExpr::NewTarget => {}
        PrimeExpr::Null => {}
        PrimeExpr::This => {}
        PrimeExpr::Super => {}
        PrimeExpr::Covered { expr } => visitor.visit_expr_list(expr)?,
    }
    Ok(())
}

pub fn visit_propery_definition_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    prop: NodeListId<PropertyDefinition>,
) -> Result<(), V::Error> {
    visit_list!(visitor=>visit_propery_definition(prop));
    Ok(())
}

pub fn visit_propery_definition<V: Visitor + ?Sized>(
    visitor: &mut V,
    prop: NodeId<PropertyDefinition>,
) -> Result<(), V::Error> {
    match visitor.ast()[prop] {
        PropertyDefinition::Ident { ident } => visitor.visit_symbol(ident)?,
        PropertyDefinition::Covered {
            symbol,
            initializer,
        } => {
            visitor.visit_symbol(symbol)?;
            visitor.visit_expr(initializer)?;
        }
        PropertyDefinition::Define { property, expr } => {
            visitor.visit_expr(expr)?;
            match property {
                PropertyName::Ident { .. }
                | PropertyName::String { .. }
                | PropertyName::Number { .. } => {}
                PropertyName::Computed { expr } => visitor.visit_expr(expr)?,
            }
        }
        PropertyDefinition::Method { property, func }
        | PropertyDefinition::Getter { property, func }
        | PropertyDefinition::Setter { property, func } => {
            visitor.visit_function(func)?;
            match property {
                PropertyName::Ident { .. }
                | PropertyName::String { .. }
                | PropertyName::Number { .. } => {}
                PropertyName::Computed { expr } => visitor.visit_expr(expr)?,
            }
        }
        PropertyDefinition::Rest { rest } => {
            visitor.visit_expr(rest)?;
        }
    }
    Ok(())
}

pub fn visit_array_literal_entry_list<V: Visitor + ?Sized>(
    visitor: &mut V,
    prop: NodeListId<ArrayLiteralEntry>,
) -> Result<(), V::Error> {
    visit_list!(visitor=>visit_array_literal_entry(prop));
    Ok(())
}

pub fn visit_array_literal_entry<V: Visitor + ?Sized>(
    visitor: &mut V,
    entry: NodeId<ArrayLiteralEntry>,
) -> Result<(), V::Error> {
    if let Some(ent) = visitor.ast()[entry].expr {
        visitor.visit_expr(ent)?;
    }
    Ok(())
}

pub fn visit_template<V: Visitor + ?Sized>(
    visitor: &mut V,
    tem: NodeId<Template>,
) -> Result<(), V::Error> {
    match visitor.ast()[tem] {
        Template::Head { expr, next, .. } => {
            visitor.visit_expr_list(expr)?;
            visitor.visit_template(next)?;
        }
        Template::Tail { .. } => {}
    }
    Ok(())
}
