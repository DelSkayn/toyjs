use super::*;

impl<'a> Parser<'a> {
    pub fn parse_class(&mut self, stmt: bool) -> PResult<'a, Class> {
        expect!(self, "class");
        let name = if stmt {
            if let Some(x) = self.next_ident() {
                Some(x)
            } else {
                unexpected!(self, "ident" => "expected class name");
            }
        } else {
            self.next_ident()
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

            let name = self.parse_property_name()?;
            let mut ty = MethodType::Normal;
            let name = match name {
                PropertyName::Ident(x) => {
                    if x.0 == "get" && !is!(self, "(") {
                        ty = MethodType::Getter;
                        self.parse_property_name()?
                    } else if x.0 == "set" && !is!(self, "(") {
                        ty = MethodType::Setter;
                        self.parse_property_name()?
                    } else {
                        PropertyName::Ident(x)
                    }
                }
                x => x,
            };
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
