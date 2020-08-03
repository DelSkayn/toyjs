use crate::{
    lexer::Lexer,
    parser::*,
    source::{Source, Span},
    ssa::{InstrVar, Instruction, SsaVar},
    token::{DelimToken, Token, TokenKind},
};

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> PResult<Option<SsaVar>> {
        let peek = match self.peek()? {
            Some(x) => x,
            None => return Ok(None),
        };
        match peek.kind {
            t!("if") => self.parse_if()?,
            t!("{") => return self.parse_block(),
            _ => return Ok(Some(self.parse_expr()?)),
        }
        Ok(None)
    }

    fn parse_block(&mut self) -> PResult<Option<SsaVar>> {
        expect!(self, "{");
        let mut last = None;
        while !eat!(self, "}") {
            let res = self.parse_stmt()?;
            if res.is_some() {
                last = res;
            }
            eat!(self, ";");
        }
        Ok(last)
    }

    fn parse_if(&mut self) -> PResult<()> {
        expect!(self, "if");
        expect!(self, "(");
        let expr = self.alter_state(|s| s._in = true, |this| this.parse_expr())?;
        expect!(self, ")");
        let jump_cond = self.builder.push_instruction(Instruction::CondJump {
            negative: true,
            condition: expr.into(),
            target: InstrVar::null(),
        });
        self.parse_stmt()?;
        let jump_cond_target = if eat!(self, "else") {
            let jump = self.builder.push_instruction(Instruction::Jump {
                target: InstrVar::null(),
            });
            let jump_cond_target = self.builder.next_id();
            self.parse_stmt()?;
            self.builder
                .patch_jump_target(jump, self.builder.next_id().into());
            jump_cond_target
        } else {
            self.builder.next_id()
        };
        self.builder
            .patch_jump_target(jump_cond, jump_cond_target.into());
        Ok(())
    }
}
