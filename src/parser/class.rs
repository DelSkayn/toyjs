use super::*;

impl<'a> Parser<'a> {
    pub fn parse_class(&mut self, stmt: bool) -> PResult<'a, Class<'a>> {
        expect!(self, "class");
        let name = if stmt {
            Some(expect!(self, "ident" => "expected class name"))
        } else {
            if is!(self, "ident") {
                Some(self.next().unwrap())
            } else {
                None
            }
        };
        let heritage = if eat!(self, "extends") {
            Some(self.parse_lhs_expr()?)
        } else {
            None
        };
        expect!(self, "{");
        let mut methods = Vec::new();
        while !eat!(self, "}") {
            if eat!(self, ";") {
                continue;
            }
            let is_static = eat!(self, "static");

            let mut name = self.parse_property_name()?;
            let mut ty = MethodType::Normal;
            if let PropertyName::Ident(x) = name {
                if x.kind == TokenKind::Ident("get") {
                    if !is!(self, "(") {
                        name = self.parse_property_name()?;
                        ty = MethodType::Getter;
                    }
                }
                if x.kind == TokenKind::Ident("set") {
                    if !is!(self, "(") {
                        name = self.parse_property_name()?;
                        ty = MethodType::Setter;
                    }
                }
            }
            let params = if ty == MethodType::Getter {
                expect!(self, "(");
                expect!(self,")" => "getters dont have arguments");
                Parameters {
                    params: Vec::new(),
                    rest: None,
                }
            } else {
                self.parse_params()?
            };
            let block = self.parse_block_stmt()?;
            methods.push(Method {
                name,
                params,
                ty,
                is_static,
                block,
            });
        }
        Ok(Class {
            name,
            heritage,
            methods,
        })
    }
}
