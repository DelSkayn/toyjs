use crate::{
    lexer::Lexer,
    parser::*,
    source::{Source, Span},
    ssa::{InstrVar, Instruction, SsaVar},
    token::{DelimToken, Token, TokenKind},
};

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> PResult<Option<SsaVar>> {
        trace_log!("statement");
        let peek = match self.peek()? {
            Some(x) => x,
            None => return Ok(None),
        };
        match peek.kind {
            t!("var") | t!("let") | t!("const") => {
                return self.alter_state(|x| x._in = true, |this| this.parse_decl());
            }
            t!("if") => self.parse_if()?,
            t!("while") => self.parse_while()?,
            t!("do") => self.parse_do_while()?,
            t!("{") => return self.parse_block(),
            t!("break") => self.parse_break()?,
            _ => return Ok(Some(self.parse_expr()?)),
        }
        Ok(None)
    }

    fn parse_block(&mut self) -> PResult<Option<SsaVar>> {
        trace_log!("block");
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
        self.builder.clear_expr_jump_context();
        let expr = self.alter_state(|s| s._in = true, |this| this.parse_expr())?;
        let jump_context = self.builder.take_expr_jump_context();
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
            self.builder.patch_jump_next(jump);
            jump_cond_target
        } else {
            self.builder.next_id()
        };
        self.builder
            .patch_jump_target(jump_cond, jump_cond_target.into());
        self.builder
            .patch_context_jump(jump_cond_target, &jump_context, false);
        Ok(())
    }

    fn parse_do_while(&mut self) -> PResult<()> {
        expect!(self, "do");
        let loop_target = self.builder.next_id();
        self.builder.clear_stmt_jump_context();
        self.alter_state(
            |s| {
                s._break = true;
                s._continue = true;
            },
            |this| this.parse_stmt(),
        )?;
        let stmt_context = self.builder.take_stmt_jump_context();
        expect!(self, "while");
        expect!(self, "(");
        self.builder.clear_expr_jump_context();
        let cond = self.parse_expr()?;
        let jump_context = self.builder.take_expr_jump_context();
        expect!(self, ")");
        self.builder.push_instruction(Instruction::CondJump {
            target: loop_target.into(),
            negative: false,
            condition: cond.into(),
        });
        self.builder
            .patch_context_jump(loop_target, &jump_context, true);
        self.builder.patch_continue_jump(loop_target, &stmt_context);
        self.builder
            .patch_break_jump(self.builder.next_id(), &stmt_context);
        Ok(())
    }

    fn parse_while(&mut self) -> PResult<()> {
        expect!(self, "while");
        expect!(self, "(");
        let again = self.builder.next_id();
        self.builder.clear_expr_jump_context();
        let expr = self.parse_expr()?;
        let jump_context = self.builder.take_expr_jump_context();
        expect!(self, ")");
        let cond_jump = self.builder.push_instruction(Instruction::CondJump {
            negative: true,
            condition: expr.into(),
            target: InstrVar::null(),
        });
        self.builder.clear_stmt_jump_context();
        self.alter_state(
            |s| {
                s._break = true;
                s._continue = true;
            },
            |this| this.parse_stmt(),
        )?;
        let stmt_context = self.builder.take_stmt_jump_context();
        self.builder.push_instruction(Instruction::Jump {
            target: again.into(),
        });

        self.builder.patch_continue_jump(again, &stmt_context);
        self.builder
            .patch_break_jump(self.builder.next_id(), &stmt_context);
        self.builder.patch_jump_next(cond_jump);
        self.builder
            .patch_context_jump(self.builder.next_id(), &jump_context, false);
        Ok(())
    }

    fn parse_break(&mut self) -> PResult<()> {
        if !self.state._break {
            unexpected!(self => "break is not allowed in this context");
        }
        expect!(self, "break");
        if let Some(t!("ident")) = self.peek_with_lt()?.map(|e| e.kind) {
            to_do!(self)
        }
        eat!(self, ";");
        self.builder.push_break();
        Ok(())
    }

    fn parse_continue(&mut self) -> PResult<()> {
        if !self.state._continue {
            unexpected!(self => "is not allowed in this context");
        }
        expect!(self, "continue");
        if let Some(t!("ident")) = self.peek_with_lt()?.map(|e| e.kind) {
            to_do!(self)
        }
        eat!(self, ";");
        self.builder.push_continue();
        Ok(())
    }
}
