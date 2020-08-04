use super::*;
use crate::{
    interner::StringId,
    ssa::{InstrVar, Instruction, SsaVar},
    token::{LitToken, NumberKind},
};

#[derive(Clone, Copy)]
enum LexicalKind {
    Var,
    Let,
    Const,
}

enum Binding {
    Ident(StringId),
    Array,
    Object,
}

impl<'a> Parser<'a> {
    pub fn parse_decl(&mut self) -> PResult<Option<SsaVar>> {
        let kind = match self.peek_kind()? {
            Some(t!("var")) => LexicalKind::Var,
            Some(t!("let")) => LexicalKind::Let,
            Some(t!("const")) => LexicalKind::Const,
            _ => unexpected!(self, "var", "let", "const"),
        };
        self.next()?;
        loop {
            let binding = self.parse_binding()?;
            let expr = match binding {
                Binding::Array => {
                    expect!(self, "=" => "array bindings need to be initialized");
                    Some(self.parse_ops()?)
                }
                Binding::Object => {
                    expect!(self, "=" => "object bindings need to be initialized");
                    Some(self.parse_ops()?)
                }
                Binding::Ident(_) => {
                    if eat!(self, "=") {
                        Some(self.parse_ops()?)
                    } else {
                        None
                    }
                }
            };
            let last = self.compile_decl(kind, binding, expr)?;
            if !eat!(self, ",") {
                break Ok(last);
            }
        }
    }

    fn compile_decl(
        &mut self,
        kind: LexicalKind,
        binding: Binding,
        initializer: Option<SsaVar>,
    ) -> PResult<Option<SsaVar>> {
        match kind {
            LexicalKind::Var => match binding {
                Binding::Ident(ident) => {
                    if let Some(init) = initializer {
                        let glob = self.builder.push_instruction(Instruction::LoadGlobal);
                        let key = self.builder.load_constant(ident);
                        self.builder.push_instruction(Instruction::ObjectSet {
                            object: glob.into(),
                            key: key.into(),
                            value: init.into(),
                        });
                        return Ok(Some(init));
                    } else {
                        return Ok(None);
                    }
                }
                _ => to_do!(self),
            },
            _ => to_do!(self),
        }
    }

    fn parse_binding(&mut self) -> PResult<Binding> {
        trace_log!("binding");
        match self.peek_kind()? {
            Some(t!("[")) => self.parse_array_binding(),
            Some(t!("{")) => self.parse_object_binding(),
            Some(TokenKind::Ident(x)) => {
                self.next()?;
                Ok(Binding::Ident(x))
            }
            _ => unexpected!(self, "[", "{", "ident"),
        }
    }

    fn parse_array_binding(&mut self) -> PResult<Binding> {
        to_do!(self)
    }

    fn parse_object_binding(&mut self) -> PResult<Binding> {
        to_do!(self)
    }
}
