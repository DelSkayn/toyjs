use super::*;
use crate::{
    interner::StringId,
    ssa::{BindingType, Expr, Instruction, SsaBuilder, SsaId},
    token::{LitToken, NumberKind},
};

enum Binding {
    Ident(StringId),
    Array,
    Object,
}

impl<'a> Parser<'a> {
    pub fn parse_decl(&mut self, builder: &mut SsaBuilder) -> PResult<Option<SsaId>> {
        let kind = match self.peek_kind()? {
            Some(t!("var")) => BindingType::Var,
            Some(t!("let")) => BindingType::Let,
            Some(t!("const")) => BindingType::Const,
            _ => unexpected!(self, "var", "let", "const"),
        };
        self.next()?;
        loop {
            let binding = self.parse_binding(builder)?;
            let expr = match binding {
                Binding::Array => {
                    expect!(self, "=" => "array bindings need to be initialized");
                    Some(self.parse_ops(builder)?)
                }
                Binding::Object => {
                    expect!(self, "=" => "object bindings need to be initialized");
                    Some(self.parse_ops(builder)?)
                }
                Binding::Ident(x) => {
                    let var = if let Some(x) = builder.declare(x, kind) {
                        x
                    } else {
                        syntax_error!(self, ParseErrorKind::RedeclaredVariable)
                    };
                    if eat!(self, "=") {
                        let expr = self.parse_ops(builder)?;
                        builder.assign(var, expr.into());
                        Some(expr)
                    } else if kind == BindingType::Const {
                        unexpected!(self => "const variables must be initialized");
                    } else {
                        None
                    }
                }
            };
            if !eat!(self, ",") {
                break Ok(expr);
            }
        }
    }

    fn parse_binding(&mut self, builder: &mut SsaBuilder) -> PResult<Binding> {
        trace_log!("binding");
        match self.peek_kind()? {
            Some(t!("[")) => self.parse_array_binding(builder),
            Some(t!("{")) => self.parse_object_binding(builder),
            Some(TokenKind::Ident(x)) => {
                self.next()?;
                Ok(Binding::Ident(x))
            }
            _ => unexpected!(self, "[", "{", "ident"),
        }
    }

    fn parse_array_binding(&mut self, _builder: &mut SsaBuilder) -> PResult<Binding> {
        to_do!(self)
    }

    fn parse_object_binding(&mut self, _builder: &mut SsaBuilder) -> PResult<Binding> {
        to_do!(self)
    }
}
