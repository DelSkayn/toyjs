use super::{Allocator, Error, ErrorKind, Parser, Result};
use ast::{
    symbol_table::{DeclType, ScopeKind},
    Binding, BindingRestElement, Expr, Literal,
};
use token::{t, TokenKind};

impl<'a, 'b, A: Allocator + Clone> Parser<'a, 'b, A> {
    pub(crate) fn parse_stmt(&mut self) -> Result<ast::Stmt<A>> {
        let peek = match self.peek_kind()? {
            Some(x) => x,
            None => return Ok(ast::Stmt::Empty),
        };
        let res = match peek {
            t!("if") => self.parse_if(),
            t!("while") => self.parse_while(),
            t!("switch") => self.parse_switch(),
            t!("do") => self.parse_do_while(),
            t!("for") => self.parse_for(),
            t!("let") => self.parse_let_binding(),
            t!("var") => self.parse_var_binding(),
            t!("const") => self.parse_const_binding(),
            t!("return") => self.parse_return(),
            t!("throw") => self.parse_throw(),
            t!(";") => {
                self.next()?;
                Ok(ast::Stmt::Empty)
            }
            t!("break") => {
                if !self.state.r#break {
                    unexpected!(self => "`break` is disallowed in this scope.");
                }
                self.next()?;
                Ok(ast::Stmt::Break)
            }
            t!("continue") => {
                if !self.state.r#continue {
                    unexpected!(self => "`continue` is disallowed in this scope.");
                }
                self.next()?;
                Ok(ast::Stmt::Continue)
            }
            t!("{") => self.parse_block(),
            t!("function") => self.parse_function(),
            t!("try") => self.parse_try(),
            _ => {
                let expr = self.parse_expr()?;
                match self.peek_lt()?.map(|x| x.kind) {
                    Some(t!(";")) | Some(t!("\n")) | Some(t!("}")) | None => {}
                    _ => {
                        unexpected!(self, "\n", ";" => "expected expression to end");
                    }
                }
                Ok(ast::Stmt::Expr(expr))
            }
        };
        self.eat(t!(";"))?;
        res
    }

    fn parse_if(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "if");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        let if_stmt = self.parse_stmt()?;
        let if_stmt = Box::new_in(if_stmt, self.alloc.clone());
        let else_stmt = if self.eat(t!("else"))? {
            let else_stmt = self.parse_stmt()?;
            Some(Box::new_in(else_stmt, self.alloc.clone()))
        } else {
            None
        };
        Ok(ast::Stmt::If(expr, if_stmt, else_stmt))
    }

    fn parse_while(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "while");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        let stmt = self.alter_state(
            |x| {
                x.r#continue = true;
                x.r#break = true;
            },
            Parser::parse_stmt,
        )?;
        Ok(ast::Stmt::While(
            expr,
            Box::new_in(stmt, self.alloc.clone()),
        ))
    }

    fn parse_switch(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "switch");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        expect!(self, "{");
        let mut clauses = Vec::new_in(self.alloc.clone());
        let mut r#default = None;
        self.alter_state(
            |x| {
                x.r#break = true;
            },
            |this| loop {
                if this.eat(t!("case"))? {
                    let expr = this.parse_single_expr()?;
                    expect!(this, ":");
                    let mut stmts = Vec::new_in(this.alloc.clone());
                    loop {
                        let peek = this.peek_kind()?;
                        match peek {
                            Some(t!("case")) | Some(t!("default")) | Some(t!("}")) => break,
                            _ => {
                                stmts.push(this.parse_stmt()?);
                            }
                        }
                    }
                    clauses.push(ast::Case { expr, stmts });
                } else if this.eat(t!("default"))? {
                    expect!(this, ":");
                    let mut peek = this.peek_kind()?;
                    let mut stmts = Vec::new_in(this.alloc.clone());
                    while peek.is_some() && peek.unwrap() != t!("}") {
                        stmts.push(this.parse_stmt()?);
                        peek = this.peek_kind()?;
                    }
                    r#default = Some(stmts);
                    return Ok(());
                } else {
                    return Ok(());
                }
            },
        )?;
        if r#default.is_some() && self.peek_kind()? == Some(t!("case")) {
            unexpected!(self => "`default` must be the last case");
        }
        expect!(self, "}" => "switch block not closed");
        Ok(ast::Stmt::Switch(expr, clauses, r#default))
    }

    fn parse_do_while(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "do");
        let stmt = self.alter_state(
            |x| {
                x.r#continue = true;
                x.r#break = true;
            },
            Self::parse_stmt,
        )?;
        expect!(self, "while");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        Ok(ast::Stmt::DoWhile(
            Box::new_in(stmt, self.alloc.clone()),
            expr,
        ))
    }

    pub(crate) fn parse_symbol_or_binding(
        &mut self,
        decl_type: Option<DeclType>,
    ) -> Result<ast::SymbolOrBinding<A>> {
        match self.peek_kind()? {
            Some(t!("{")) | Some(t!("[")) => {
                let binding = self.parse_binding(decl_type)?;
                Ok(ast::SymbolOrBinding::Binding(binding))
            }
            Some(t!("ident", ident)) => {
                self.next()?;
                let sym = if let Some(decl_type) = decl_type {
                    self.symbol_table.define(ident, decl_type).ok_or(Error {
                        kind: ErrorKind::RedeclaredVariable,
                        origin: self.last_span,
                    })?
                } else {
                    self.symbol_table.use_symbol(ident)
                };
                Ok(ast::SymbolOrBinding::Single(sym))
            }
            _ => unexpected!(self, "ident", "[", "{" => "expected binding"),
        }
    }

    /*
    pub(crate) fn parse_for_decl(&mut self) -> Result<ast::ForDecl<A>> {
        match self.peek_kind()?{
            None => unexpected!(self, "let", "var", "const", "ident", "expression"),
            Some(t!("let")) => {
                self.next();
                let decl = self.parse_symbol_or_binding(Some(DeclType::Let))?;
                Ok(ast::ForDecl::Decl(decl))
            }
            Some(t!("const")) => {
                self.next();
                let decl = self.parse_symbol_or_binding(Some(DeclType::Const))?;
                Ok(ast::ForDecl::Decl(decl))
            }
            Some(t!("var")) => {
                self.next();
                let decl = self.parse_symbol_or_binding(Some(DeclType::Var))?;
                Ok(ast::ForDecl::Decl(decl))
            }
            _ => {
                let expr = self.parse_single_expr()?;
                Ok(ast::ForDecl::Expr(expr))
            }
        }
    }
    */

    pub(crate) fn parse_for(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "for");
        expect!(self, "(");
        let _scope = self.symbol_table.push_scope(ScopeKind::Lexical);
        let decl = if self.eat(t!(";"))? {
            None
        } else {
            let peek = if let Some(x) = self.peek_kind()? {
                x
            } else {
                unexpected!(self, "let", "var", "const", "ident", "expression")
            };

            let decl_type = match peek {
                t!("let") => {
                    self.next()?;
                    Some(DeclType::Let)
                }
                t!("const") => {
                    self.next()?;
                    Some(DeclType::Const)
                }
                t!("var") => {
                    self.next()?;
                    Some(DeclType::Var)
                }
                _ => None,
            };

            let decl = if let Some(decl) = decl_type {
                let decl = ast::ForInOfDecl::Define(self.parse_symbol_or_binding(Some(decl))?);
                let peek = self.peek_kind()?;
                if peek == Some(t!("in")) {
                    return self.parse_for_in_of(false, decl);
                }
                if peek == Some(t!("of")) {
                    return self.parse_for_in_of(true, decl);
                }
                decl
            } else {
                let expr = self.parse_single_expr()?;
                let peek = self.peek_kind()?;
                if peek == Some(t!("in")) {
                    let invalid = match expr {
                        Expr::Prime(ast::PrimeExpr::Object(_))
                        | Expr::Prime(ast::PrimeExpr::Array(_)) => false,
                        ref x => !x.is_assignable(),
                    };
                    if invalid {
                        unexpected!(self => "left hand is not a assignable expression or object or array literal");
                    }
                    let decl = ast::ForInOfDecl::Expr(expr);
                    return self.parse_for_in_of(false, decl);
                }
                if peek == Some(t!("of")) {
                    let invalid = match expr {
                        Expr::Prime(ast::PrimeExpr::Object(_))
                        | Expr::Prime(ast::PrimeExpr::Array(_)) => false,
                        ref x => !x.is_assignable(),
                    };
                    if invalid {
                        unexpected!(self => "left hand is not a assignable expression or object or array literal");
                    }
                    let decl = ast::ForInOfDecl::Expr(expr);
                    return self.parse_for_in_of(true, decl);
                }
                ast::ForInOfDecl::Expr(expr)
            };

            let decl = match decl {
                ast::ForInOfDecl::Expr(x) => {
                    let mut expr = Vec::with_capacity_in(1, self.alloc.clone());
                    expr.push(x);
                    while self.eat(t!(","))? {
                        let e = self.parse_single_expr()?;
                        expr.push(e);
                    }
                    ast::CForDecl::Expr(expr)
                }
                ast::ForInOfDecl::Define(x) => {
                    let binding = match x {
                        ast::SymbolOrBinding::Binding(binding) => {
                            expect!(self, "=" => "binding declarations must be initialized");
                            let init = self.parse_single_expr()?;
                            ast::Define::Binding { binding, init }
                        }
                        ast::SymbolOrBinding::Single(symbol) => {
                            let init = if decl_type == Some(DeclType::Const) {
                                expect!(self, "=" => "const declarations must be initialized");
                                Some(self.parse_single_expr()?)
                            } else {
                                self.eat(t!("="))?
                                    .then(|| self.parse_single_expr())
                                    .transpose()?
                            };
                            ast::Define::Single { symbol, init }
                        }
                    };
                    let mut defs = Vec::with_capacity_in(1, self.alloc.clone());
                    defs.push(binding);
                    while self.eat(t!(","))? {
                        let def = self.parse_define(decl_type.unwrap())?;
                        defs.push(def)
                    }
                    ast::CForDecl::Define(defs)
                }
            };

            expect!(self, ";");
            Some(decl)
        };
        let cond = if self.eat(t!(";"))? {
            None
        } else {
            let res = self.parse_expr()?;
            expect!(self, ";");
            Some(res)
        };
        let post = if self.eat(t!(")"))? {
            None
        } else {
            let res = self.parse_expr()?;
            expect!(self, ")");
            Some(res)
        };
        let stmt = self.alter_state(
            |x| {
                x.r#continue = true;
                x.r#break = true;
            },
            Self::parse_stmt,
        )?;

        debug_assert_eq!(_scope, self.symbol_table.current_scope());
        self.symbol_table.pop_scope();

        Ok(ast::Stmt::For(ast::For::CStyle(
            decl,
            cond,
            post,
            Box::new_in(stmt, self.alloc.clone()),
        )))
    }

    fn parse_for_in_of(&mut self, is_of: bool, decl: ast::ForInOfDecl<A>) -> Result<ast::Stmt<A>> {
        if is_of {
            expect!(self, "of");
        } else {
            expect!(self, "in");
        }

        let e = self.parse_expr()?;

        expect!(self, ")");

        let stmt = self.alter_state(
            |x| {
                x.r#continue = true;
                x.r#break = true;
            },
            Self::parse_stmt,
        )?;

        self.symbol_table.pop_scope();

        if is_of {
            Ok(ast::Stmt::For(ast::For::ForOf(
                decl,
                e,
                Box::new_in(stmt, self.alloc.clone()),
            )))
        } else {
            Ok(ast::Stmt::For(ast::For::ForIn(
                decl,
                e,
                Box::new_in(stmt, self.alloc.clone()),
            )))
        }
    }

    fn parse_define(&mut self, decl_type: DeclType) -> Result<ast::Define<A>> {
        match self.peek_kind()? {
            Some(t!("{")) | Some(t!("[")) => {
                let binding = self.parse_binding(Some(decl_type))?;
                expect!(self,"=" => "binding must be initialized");
                let init = self.parse_single_expr()?;
                Ok(ast::Define::Binding { binding, init })
            }
            Some(TokenKind::Ident(id)) => {
                self.next()?;
                let symbol = self.symbol_table.define(id, decl_type).ok_or(Error {
                    kind: ErrorKind::RedeclaredVariable,
                    origin: self.last_span,
                })?;
                let init = if decl_type == DeclType::Const {
                    expect!(self,"=" => "const binding must be initialized");
                    Some(self.parse_single_expr()?)
                } else if self.eat(t!("="))? {
                    Some(self.parse_single_expr()?)
                } else {
                    None
                };
                Ok(ast::Define::Single { symbol, init })
            }
            _ => unexpected!(self, "{", "[", "ident"),
        }
    }

    fn parse_let_binding(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "let");
        let define = self.parse_define(DeclType::Let)?;
        let mut vec = Vec::with_capacity_in(1, self.alloc.clone());
        vec.push(define);
        while self.eat(t!(","))? {
            let define = self.parse_define(DeclType::Let)?;
            vec.push(define);
        }
        Ok(ast::Stmt::Let(vec))
    }

    fn parse_var_binding(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "var");
        let define = self.parse_define(DeclType::Var)?;
        let mut vec = Vec::with_capacity_in(1, self.alloc.clone());
        vec.push(define);
        while self.eat(t!(","))? {
            let define = self.parse_define(DeclType::Var)?;
            vec.push(define);
        }
        Ok(ast::Stmt::Var(vec))
    }

    fn parse_const_binding(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "const");
        let define = self.parse_define(DeclType::Const)?;
        let mut vec = Vec::with_capacity_in(1, self.alloc.clone());
        vec.push(define);
        while self.eat(t!(","))? {
            let define = self.parse_define(DeclType::Const)?;
            vec.push(define);
        }
        Ok(ast::Stmt::Var(vec))
    }

    fn parse_binding(&mut self, define: Option<DeclType>) -> Result<ast::Binding<A>> {
        if self.eat(t!("{"))? {
            let mut properties = Vec::new_in(self.alloc.clone());
            while !self.eat(t!("}"))? {
                if self.eat(t!("..."))? {
                    expect_bind!(self,let id = "ident");
                    let symbol = if let Some(d) = define {
                        self.symbol_table.define(id, d).ok_or(Error {
                            kind: ErrorKind::RedeclaredVariable,
                            origin: self.last_span,
                        })?
                    } else {
                        self.symbol_table.use_symbol(id)
                    };
                    expect!(self,"}" => "binding pattern should end after rest property");
                    return Ok(Binding::Object(properties, Some(symbol)));
                }

                properties.push(self.parse_binding_property(define)?);

                if self.eat(t!(","))? {
                    if self.peek_kind()? == Some(t!("}")) {
                        unexpected!(self => "trailing comma");
                    }
                }
            }
            return Ok(Binding::Object(properties, None));
        }
        if self.eat(t!("["))? {
            let mut elements = Vec::new_in(self.alloc.clone());
            while !self.eat(t!("]"))? {
                if self.eat(t!(","))? {
                    elements.push(None);
                    if self.eat(t!("]"))? {
                        elements.push(None);
                        break;
                    }
                    continue;
                }

                if self.eat(t!("..."))? {
                    match self.peek_kind()? {
                        Some(t!("{")) | Some(t!("[")) => {
                            let binding = self.parse_binding(define)?;
                            return Ok(Binding::Array(
                                elements,
                                BindingRestElement::Binding(Box::new_in(
                                    binding,
                                    self.alloc.clone(),
                                )),
                            ));
                        }
                        Some(t!("ident", id)) => {
                            let symbol = if let Some(d) = define {
                                self.symbol_table.define(id, d).ok_or(Error {
                                    kind: ErrorKind::RedeclaredVariable,
                                    origin: self.last_span,
                                })?
                            } else {
                                self.symbol_table.use_symbol(id)
                            };
                            return Ok(Binding::Array(
                                elements,
                                BindingRestElement::Single(symbol),
                            ));
                        }
                        _ => unexpected!(self, "{", "[", "ident"),
                    }
                }

                elements.push(Some(self.parse_binding_element(define)?));

                if self.eat(t!(","))? {
                    if self.eat(t!("]"))? {
                        elements.push(None);
                        break;
                    }
                } else {
                    expect!(self, "]");
                    break;
                }
            }
            return Ok(Binding::Array(elements, BindingRestElement::None));
        }
        unexpected!(self, "{","[" => "expected binding pattern")
    }

    fn parse_binding_property(
        &mut self,
        define: Option<DeclType>,
    ) -> Result<ast::BindingProperty<A>> {
        if self.eat(t!("["))? {
            let computed = self.parse_single_expr()?;
            expect!(self, "]");
            expect!(self,":" => "computed bindings require an element binding");
            let element = self.parse_binding_element(define)?;
            return Ok(ast::BindingProperty::Computed(computed, element));
        } else if let Some(TokenKind::Literal(x)) = self.peek_kind()? {
            self.next()?;
            let computed = match x {
                token::Literal::Number(token::Number::Integer(x)) => {
                    Expr::Prime(ast::PrimeExpr::Literal(Literal::Integer(x)))
                }
                token::Literal::Number(token::Number::Float(x)) => {
                    Expr::Prime(ast::PrimeExpr::Literal(Literal::Float(x)))
                }
                token::Literal::String(x) => {
                    Expr::Prime(ast::PrimeExpr::Literal(Literal::String(x)))
                }
                _ => {
                    unexpected!(self => "bigints are not allowed as a computed properties")
                }
            };
            let element = self.parse_binding_element(define)?;
            return Ok(ast::BindingProperty::Computed(computed, element));
        } else if let Some(TokenKind::Ident(id)) = self.next()?.map(|x| x.kind) {
            if self.eat(t!(":"))? {
                let element = self.parse_binding_element(define)?;
                return Ok(ast::BindingProperty::Ident(id, element));
            } else {
                let symbol = if let Some(d) = define {
                    self.symbol_table.define(id, d).ok_or(Error {
                        kind: ErrorKind::RedeclaredVariable,
                        origin: self.last_span,
                    })?
                } else {
                    self.symbol_table.use_symbol(id)
                };
                let init = if self.eat(t!("="))? {
                    Some(self.parse_single_expr()?)
                } else {
                    None
                };
                return Ok(ast::BindingProperty::Single(symbol, init));
            }
        }
        unexpected!(self, "[", "ident")
    }

    fn parse_binding_element(
        &mut self,
        define: Option<DeclType>,
    ) -> Result<ast::BindingElement<A>> {
        match self.peek_kind()? {
            Some(t!("{")) | Some(t!("[")) => {
                let binding = self.parse_binding(define)?;
                let init = if self.eat(t!("="))? {
                    Some(self.parse_single_expr()?)
                } else {
                    None
                };
                return Ok(ast::BindingElement::Binding(
                    Box::new_in(binding, self.alloc.clone()),
                    init,
                ));
            }
            Some(TokenKind::Ident(id)) => {
                self.next()?;
                let symbol = if let Some(d) = define {
                    self.symbol_table.define(id, d).ok_or(Error {
                        kind: ErrorKind::RedeclaredVariable,
                        origin: self.last_span,
                    })?
                } else {
                    self.symbol_table.use_symbol(id)
                };
                let init = if self.eat(t!("="))? {
                    Some(self.parse_single_expr()?)
                } else {
                    None
                };
                return Ok(ast::BindingElement::Single(symbol, init));
            }
            _ => unexpected!(self, "{", "[", "ident"),
        }
    }

    fn parse_block(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "{");
        let scope = self.symbol_table.push_scope(ScopeKind::Lexical);
        let mut stmts = Vec::new_in(self.alloc.clone());
        while !self.eat(t!("}"))? {
            stmts.push(self.parse_stmt()?);
        }
        debug_assert_eq!(scope, self.symbol_table.current_scope());
        self.symbol_table.pop_scope();
        Ok(ast::Stmt::Block(scope, stmts))
    }

    fn parse_function(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "function");
        expect_bind!(self, let name = "ident");
        let var = self.symbol_table.define(name, DeclType::Var).ok_or(Error {
            kind: ErrorKind::RedeclaredVariable,
            origin: self.last_span,
        })?;
        let scope = self.symbol_table.push_scope(ScopeKind::Function);
        let params = self.parse_params(false)?;
        expect!(self, "{");
        let stmts = self.alter_state::<_, _, Result<_>>(
            |s| {
                s.r#return = true;
                s.r#continue = false;
                s.r#break = false;
            },
            |this| {
                let mut stmts = Vec::new_in(this.alloc.clone());
                while !this.eat(t!("}"))? {
                    stmts.push(this.parse_stmt()?);
                }
                Ok(stmts)
            },
        )?;
        debug_assert_eq!(scope, self.symbol_table.current_scope());
        self.symbol_table.pop_scope();
        Ok(ast::Stmt::Function(scope, var, params, stmts))
    }

    pub(crate) fn parse_params(&mut self, openbrace_eaten: bool) -> Result<ast::Params<A>> {
        if !openbrace_eaten {
            expect!(self, "(");
        }
        let mut stmt = Vec::new_in(self.alloc.clone());
        let mut rest = None;
        while let Some(peek) = self.peek_kind()? {
            match peek {
                t!("...") => {
                    self.next()?;
                    expect_bind!(self, let arg = "ident");
                    let arg_var =
                        self.symbol_table
                            .define(arg, DeclType::Argument)
                            .ok_or(Error {
                                kind: ErrorKind::RedeclaredVariable,
                                origin: self.last_span,
                            })?;
                    rest = Some(ast::Rest::BindingIdent(arg_var));
                    break;
                }
                t!("ident", arg) => {
                    self.next()?;
                    let arg_var =
                        self.symbol_table
                            .define(arg, DeclType::Argument)
                            .ok_or(Error {
                                kind: ErrorKind::RedeclaredVariable,
                                origin: self.last_span,
                            })?;
                    stmt.push(ast::SymbolOrBinding::Single(arg_var));
                    if !self.eat(t!(","))? {
                        break;
                    }
                }
                t!("[") | t!("{") => {
                    let binding = self.parse_binding(Some(DeclType::Let))?;
                    stmt.push(ast::SymbolOrBinding::Binding(binding));
                    if !self.eat(t!(","))? {
                        break;
                    }
                }
                t!(")") => {
                    break;
                }
                _ => unexpected!(self, "...", ")", "ident"),
            }
        }
        expect!(self, ")");
        Ok(ast::Params(stmt, rest))
    }

    fn parse_return(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "return");
        let expr = self.parse_expr().ok();
        Ok(ast::Stmt::Return(expr))
    }

    fn parse_throw(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "throw");
        let expr = self.parse_single_expr()?;
        Ok(ast::Stmt::Throw(expr))
    }

    fn parse_try(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "try");
        let r#try = Box::new_in(self.parse_block()?, self.alloc.clone());
        let catch = if self.peek_kind()? == Some(t!("catch")) {
            self.next()?;
            let scope = self.symbol_table.push_scope(ScopeKind::Lexical);
            let binding = if self.peek_kind()? == Some(t!("(")) {
                self.next()?;
                let binding = if let Some(t!("{")) | Some(t!("[")) = self.peek_kind()? {
                    let binding = self.parse_binding(Some(DeclType::Let))?;
                    ast::SymbolOrBinding::Binding(binding)
                } else {
                    expect_bind!(self, let binding = "ident");
                    let binding = self.symbol_table.define(binding, DeclType::Let).unwrap();
                    ast::SymbolOrBinding::Single(binding)
                };
                expect!(self, ")");
                Some(binding)
            } else {
                None
            };
            expect!(self, "{");
            let mut stmts = Vec::new_in(self.alloc.clone());
            while !self.eat(t!("}"))? {
                stmts.push(self.parse_stmt()?);
            }
            debug_assert_eq!(scope, self.symbol_table.current_scope());
            self.symbol_table.pop_scope();
            let stmt = Box::new_in(ast::Stmt::Block(scope, stmts), self.alloc.clone());
            Some(ast::Catch { binding, stmt })
        } else {
            None
        };
        let finally = if self.peek_kind()? == Some(t!("finally")) {
            self.next()?;
            Some(Box::new_in(self.parse_block()?, self.alloc.clone()))
        } else {
            None
        };

        Ok(ast::Stmt::Try(r#try, catch, finally))
    }
}
