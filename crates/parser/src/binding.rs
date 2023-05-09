use ast::{
    BindingElement, BindingPattern, BindingProperty, IdentOrPattern, ListHead, NodeId, PropertyName,
};
use common::string::String;
use token::{t, StringId};

use crate::{expect, next_expect, peek_expect, unexpected, Parser, Result};

impl<'a> Parser<'a> {
    pub fn parse_ident_or_pattern(&mut self) -> Result<NodeId<IdentOrPattern>> {
        let first = peek_expect!(self, "ident", "{", "[");
        match first.kind() {
            t!("ident") => {
                self.next();
                Ok(self
                    .ast
                    .push_node(IdentOrPattern::Ident(first.data_id().unwrap())))
            }
            t!("{") | t!("[") => {
                let pattern = self.parse_pattern()?;
                Ok(self.ast.push_node(IdentOrPattern::Pattern(pattern)))
            }
            x => unexpected!(self, x, "ident", "{", "["),
        }
    }

    pub fn parse_pattern(&mut self) -> Result<BindingPattern> {
        let first = peek_expect!(self, "{", "[");
        match first.kind() {
            t!("{") => self.parse_object_pattern(),
            t!("[") => self.parse_array_pattern(),
            x => unexpected!(self, x, "{", "["),
        }
    }

    pub fn parse_ident(&mut self) -> Result<StringId> {
        let next = peek_expect!(self);
        match next.kind() {
            t!("ident") => Ok(next.data_id().unwrap()),
            t!("yield") => {
                if self.state.yield_ident {
                    unexpected!(self,t!("yield"),"ident" => "yield not allowed as an identifier in this context");
                } else {
                    Ok(self.lexer.data.strings.intern(&String::new_const("yield")))
                }
            }
            t!("await") => {
                if self.state.yield_ident {
                    unexpected!(self,t!("await"),"ident" => "await not allowed as an identifier in this context");
                } else {
                    Ok(self.lexer.data.strings.intern(&String::new_const("await")))
                }
            }
            x => unexpected!(self, x, "ident"),
        }
    }

    pub fn parse_object_pattern(&mut self) -> Result<BindingPattern> {
        expect!(self, "{");
        let mut head = ListHead::Empty;
        let mut prev = None;
        let mut rest = None;
        loop {
            let next = peek_expect!(self, "}");
            match next.kind() {
                t!("}") => {
                    self.next();
                    break;
                }
                t!("...") => {
                    self.next();
                    let ident = expect!(self, "ident");
                    expect!(self, "}");
                    rest = Some(ident.data_id().unwrap());
                    break;
                }
                _ => {
                    let prop = self.parse_binding_property()?;
                    let item = self.ast.push_node(prop);
                    prev = Some(self.ast.append_list(item, prev));
                    head = head.or(prev.into());
                }
            }
            if !self.eat(t!(",")) {
                expect!(self, "}");
                break;
            }
        }

        Ok(BindingPattern::Object {
            properties: head,
            rest,
        })
    }

    pub fn parse_binding_property(&mut self) -> Result<BindingProperty> {
        let next = peek_expect!(self);
        match next.kind() {
            t!("ident") => {
                self.next();
                if self.eat(t!(":")) {
                    let name = PropertyName::Ident(next.data_id().unwrap());
                    let element = self.parse_binding_element()?;
                    Ok(BindingProperty::Property { name, element })
                } else {
                    let initializer = self
                        .eat(t!("="))
                        .then(|| self.parse_assignment_expr())
                        .transpose()?;
                    Ok(BindingProperty::Binding {
                        name: next.data_id().unwrap(),
                        initializer,
                    })
                }
            }
            t!("string") => {
                self.next();
                let name = PropertyName::String(next.data_id().unwrap());
                let element = self.parse_binding_element()?;
                Ok(BindingProperty::Property { name, element })
            }
            t!("num") => {
                self.next();
                let name = PropertyName::Number(next.data_id().unwrap());
                let element = self.parse_binding_element()?;
                Ok(BindingProperty::Property { name, element })
            }
            t!("[") => {
                self.next();
                let expr = self.parse_assignment_expr()?;
                expect!(self, "]");
                let name = PropertyName::Computed(expr);
                let element = self.parse_binding_element()?;
                Ok(BindingProperty::Property { name, element })
            }
            x => unexpected!(self, x, "ident", "string", "num", "["),
        }
    }

    pub fn parse_array_pattern(&mut self) -> Result<BindingPattern> {
        expect!(self, "[");
        let mut head = ListHead::Empty;
        let mut prev = None;
        let mut rest = None;
        loop {
            let next = peek_expect!(self, "]");
            match dbg!(next.kind()) {
                t!("]") => {
                    self.next();
                    break;
                }
                t!("...") => {
                    self.next();
                    rest = Some(self.parse_ident_or_pattern()?);
                    break;
                }
                t!(",") => {
                    self.next();
                    let item = self.ast.push_node(None);
                    prev = Some(self.ast.append_list(item, prev));
                    head = head.or(prev.into());
                }
                _ => {
                    let elem = self.parse_binding_element()?;
                    let item = self.ast.push_node(Some(elem));
                    prev = Some(self.ast.append_list(item, prev));
                    head = head.or(prev.into());
                    if !self.eat(t!(",")) {
                        break;
                    }
                }
            }
        }

        Ok(BindingPattern::Array {
            elements: head,
            rest,
        })
    }

    pub fn parse_binding_element(&mut self) -> Result<BindingElement> {
        let next = peek_expect!(self);
        match next.kind() {
            t!("ident") => {
                self.next();
                let name = next.data_id().unwrap();
                let initializer = self
                    .eat(t!("="))
                    .then(|| self.parse_assignment_expr())
                    .transpose()?;
                Ok(BindingElement::SingleName { name, initializer })
            }
            t!("{") | t!("[") => {
                let pattern = self.parse_pattern()?;
                let initializer = self
                    .eat(t!("="))
                    .then(|| self.parse_assignment_expr())
                    .transpose()?;
                Ok(BindingElement::Pattern {
                    pattern,
                    initializer,
                })
            }
            x => unexpected!(self, x, "ident", "{", "["),
        }
    }
}
