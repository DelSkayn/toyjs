use super::*;

impl<'a> Parser<'a> {
    /// Parses a declration in to form of
    /// `[let|var|const] ident [= expr](,ident [= expr])*`
    pub fn parse_decl(&mut self) -> PResult<'a, DeclKind<'a>> {
        trace_log!("declaration");
        let mut kind = None;
        if eat!(self, "var") {
            kind = Some(LexicalKind::Var);
        } else if eat!(self, "let") {
            kind = Some(LexicalKind::Let);
        } else if eat!(self, "const") {
            kind = Some(LexicalKind::Const);
        }
        let kind = if let Some(x) = kind {
            x
        } else {
            unexpected!(self, "var", "let", "const");
        };

        let mut decl = Vec::new();
        loop {
            let binding = self.parse_binding()?;
            let initializer = match binding {
                Binding::ArrayPattern {
                    bindings: _,
                    rest: _,
                }
                | Binding::ObjectPattern {
                    bindings: _,
                    rest: _,
                } => {
                    expect!(self, "=");
                    Some(self.parse_assignment_expr()?)
                }
                _ => {
                    if eat!(self, "=") {
                        Some(self.parse_assignment_expr()?)
                    } else {
                        None
                    }
                }
            };
            decl.push(LexicalDecl {
                binding,
                initializer,
            });
            if !eat!(self, ",") {
                break;
            }
        }
        return Ok(DeclKind::Lexical { kind, decl });
    }

    pub fn parse_binding(&mut self) -> PResult<'a, Binding<'a>> {
        trace_log!("binding");
        if is!(self, "[") {
            return self.parse_array_binding();
        }
        if is!(self, "{") {
            return self.parse_object_binding();
        }
        if is!(self, "ident") {
            let ident = self.next().unwrap();
            return Ok(Binding::Ident { ident });
        }
        unexpected!(self, "[", "{", "ident")
    }

    pub fn parse_array_binding(&mut self) -> PResult<'a, Binding<'a>> {
        expect!(self, "[");
        let mut bindings = Vec::new();
        let mut rest = None;
        while !eat!(self, "]") {
            if eat!(self, ",") {
                bindings.push(ArrayBinding::Elision);
            } else if eat!(self, "...") {
                if !is!(self, "ident") {
                    unexpected!(self, "ident");
                }
                rest = Some(self.next().unwrap());
                expect!(self, "]");
                break;
            }
            let binding = self.parse_binding()?;
            let expr = if eat!(self, "=") {
                Some(self.parse_assignment_expr()?)
            } else {
                None
            };
            bindings.push(ArrayBinding::Binding {
                bind: binding,
                expr,
            });
            if !is!(self, "]") {
                expect!(self, ",");
            }
        }
        return Ok(Binding::ArrayPattern { bindings, rest });
    }

    pub fn parse_object_binding(&mut self) -> PResult<'a, Binding<'a>> {
        expect!(self, "{");
        let mut bindings = Vec::new();
        let mut rest = None;
        while !eat!(self, "}") {
            if !bindings.is_empty() {
                expect!(self, ",");
            }

            if eat!(self, "...") {
                rest = Some(expect!(self, "ident"));
                expect!(self, "}");
                break;
            }

            if is!(self, "lit") {
                let lit = self.next().unwrap();
                expect!(self, ":");
                let bind = self.parse_binding()?;
                let init = if eat!(self, "=") {
                    Some(self.parse_assignment_expr()?)
                } else {
                    None
                };
                bindings.push(ObjectBinding::Literal { lit, bind, init });
                continue;
            }
            if is!(self, "[") {
                let expr = self.parse_expr()?;
                expect!(self, "]");
                expect!(self, ":");
                let bind = self.parse_binding()?;
                let init = if eat!(self, "=") {
                    Some(self.parse_assignment_expr()?)
                } else {
                    None
                };
                bindings.push(ObjectBinding::Computed { expr, bind, init });
                continue;
            }
            if is!(self, "ident") {
                let ident = self.next().unwrap();
                let init = if eat!(self, "=") {
                    Some(self.parse_assignment_expr()?)
                } else {
                    None
                };
                bindings.push(ObjectBinding::Ident { ident, init });
                continue;
            }
            unexpected!(self, "lit", "[", "...", "ident");
        }
        return Ok(Binding::ObjectPattern { bindings, rest });
    }

    pub fn parse_property_name(&mut self) -> PResult<'a, PropertyName<'a>> {
        if is!(self, "ident") {
            return Ok(PropertyName::Ident(self.next().unwrap()));
        }
        if is!(self, "lit") {
            return Ok(PropertyName::Literal(self.next().unwrap()));
        }
        if eat!(self, "[") {
            let res = self.parse_assignment_expr()?;
            expect!(self, "]");
            return Ok(PropertyName::Computed(res));
        }
        unexpected!(self, "ident", "lit", "[");
    }

    // TODO check for uniqueness
    pub fn parse_params(&mut self) -> PResult<'a, Parameters<'a>> {
        trace_log!("parameters");
        expect!(self, "(");
        let mut params = Vec::new();
        let mut rest = None;
        while !eat!(self, ")") {
            if eat!(self, "...") {
                rest = Some(self.parse_binding()?);
                expect!(self,")" => "rest binding should be last parameter");
                break;
            }
            let binding = self.parse_binding()?;
            let initializer = if eat!(self, "=") {
                Some(self.parse_assignment_expr()?)
            } else {
                None
            };
            params.push((binding, initializer));
            if !eat!(self, ",") {
                expect!(self,")" => "expected end of parameters, missing comma?");
                break;
            }
        }
        Ok(Parameters { params, rest })
    }
}
