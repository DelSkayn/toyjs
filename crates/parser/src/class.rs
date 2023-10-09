use ast::{Class, ClassMember, FunctionKind, ListHead, NodeId, PropertyName};
use common::string::String;
use token::t;

use crate::{expect, function::FunctionCtx, Parser, Result};

impl<'a> Parser<'a> {
    pub fn parse_class(&mut self, stmt: bool) -> Result<NodeId<Class>> {
        let name = if stmt {
            Some(self.parse_symbol()?)
        } else {
            match self.peek_kind() {
                t!("extends") | t!("{") => None,
                _ => Some(self.parse_symbol()?),
            }
        };
        let heritage = self
            .eat(t!("extends"))
            .then(|| self.parse_assignment_expr())
            .transpose()?;

        expect!(self, "{");
        let mut head = ListHead::Empty;
        let mut prev = None;
        while !self.eat(t!("}")) {
            let is_static = self.eat(t!("static"));

            let property = match self.peek_kind() {
                t!("get") => {
                    self.next();
                    if Self::is_property_name(self.peek_kind()) {
                        let property = self.parse_property_name()?;
                        let func = self.parse_getter()?;
                        let item = self.ast.push_node(ClassMember::Getter {
                            is_static,
                            property,
                            func,
                        });
                        prev = Some(self.ast.append_list(item, prev));
                        head = head.or(prev.into());
                        continue;
                    } else {
                        PropertyName::Ident(
                            self.lexer.data.strings.intern(&String::new_const("get")),
                        )
                    }
                }
                t!("set") => {
                    self.next();
                    if Self::is_property_name(self.peek_kind()) {
                        let property = self.parse_property_name()?;
                        let func = self.parse_setter()?;
                        let item = self.ast.push_node(ClassMember::Setter {
                            is_static,
                            property,
                            func,
                        });
                        prev = Some(self.ast.append_list(item, prev));
                        head = head.or(prev.into());
                        continue;
                    } else {
                        PropertyName::Ident(
                            self.lexer.data.strings.intern(&String::new_const("get")),
                        )
                    }
                }
                t!("async") => {
                    self.next();
                    let kind = if self.eat(t!("*")) {
                        FunctionKind::AsyncGenerator
                    } else {
                        FunctionKind::Async
                    };
                    self.no_line_terminator()?;
                    let property = self.parse_property_name()?;
                    let func = self.parse_function(FunctionCtx::Method, kind)?;
                    let item = self.ast.push_node(ClassMember::Method {
                        is_static,
                        property,
                        func,
                    });
                    prev = Some(self.ast.append_list(item, prev));
                    head = head.or(prev.into());
                    continue;
                }
                t!("*") => {
                    self.next();
                    self.peek();
                    self.no_line_terminator()?;
                    let property = self.parse_property_name()?;
                    let func = self.parse_function(FunctionCtx::Method, FunctionKind::Generator)?;
                    let item = self.ast.push_node(ClassMember::Method {
                        is_static,
                        property,
                        func,
                    });
                    prev = Some(self.ast.append_list(item, prev));
                    head = head.or(prev.into());
                    continue;
                }
                _ => self.parse_property_name()?,
            };

            let item = match self.peek_kind() {
                t!("(") => {
                    let func = self.parse_function(FunctionCtx::Method, FunctionKind::Simple)?;
                    self.ast.push_node(ClassMember::Method {
                        is_static,
                        property,
                        func,
                    })
                }
                t!("=") => {
                    self.next();
                    let initializer = Some(self.parse_assignment_expr()?);
                    self.ast.push_node(ClassMember::Field {
                        is_static,
                        property,
                        initializer,
                    })
                }
                _ => {
                    self.semicolon()?;
                    self.ast.push_node(ClassMember::Field {
                        is_static,
                        property,
                        initializer: None,
                    })
                }
            };
            prev = Some(self.ast.append_list(item, prev));
            head = head.or(prev.into());
        }

        Ok(self.ast.push_node(Class {
            name,
            heritage,
            body: head,
        }))
    }
}
