use crate::{
    lexer::Lexer,
    parser::*,
    source::{Source, Span},
    ssa::{Instruction, SsaBuilder, SsaId},
    token::{DelimToken, Token, TokenKind},
};

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self, builder: &mut SsaBuilder, first: bool) -> PResult<Option<SsaId>> {
        trace_log!("statement");
        let peek = match self.peek()? {
            Some(x) => x,
            None => return Ok(None),
        };
        if first {
            if let t!("strict_directive") = peek.kind {
                self.set_strict(true);
                return Ok(None);
            }
        }
        match peek.kind {
            t!("var") | t!("let") | t!("const") => {
                return self.alter_state(|x| x._in = true, |this| this.parse_decl(builder));
            }
            t!("if") => self.parse_if(builder)?,
            t!("while") => self.parse_while(builder)?,
            t!("do") => self.parse_do_while(builder)?,
            t!("{") => return self.parse_block(builder),
            t!("break") => self.parse_break(builder)?,
            t!("continue") => self.parse_continue(builder)?,
            _ => return Ok(Some(self.parse_expr(builder)?)),
        }
        Ok(None)
    }

    fn parse_block(&mut self, builder: &mut SsaBuilder) -> PResult<Option<SsaId>> {
        trace_log!("block");
        expect!(self, "{");
        let mut last = None;
        while !eat!(self, "}") {
            //TODO figure out if opening a block no longers allows a strict directive
            let res = self.parse_stmt(builder, false)?;
            if res.is_some() {
                last = res;
            }
            eat!(self, ";");
        }
        Ok(last)
    }

    fn parse_if(&mut self, builder: &mut SsaBuilder) -> PResult<()> {
        expect!(self, "if");
        expect!(self, "(");
        builder.take_expr_context();
        let expr = self.alter_state(|s| s._in = true, |this| this.parse_expr(builder))?;
        let jump_context = builder.take_expr_context();
        expect!(self, ")");
        let jump_cond = builder.next();
        builder.jump_cond(SsaId::null(), expr.into(), false);
        self.parse_stmt(builder, false)?;

        let jump_cond_target = if eat!(self, "else") {
            //parse else
            let jump = builder.next();
            builder.jump(SsaId::null());
            let jump_cond_target = builder.next();
            self.parse_stmt(builder, false)?;
            builder.patch_jump(jump, builder.next());
            jump_cond_target
        } else {
            builder.next()
        };

        //Patch the jumps in the expression and the if.
        builder.patch_jump(jump_cond, jump_cond_target);
        builder.patch_expr_jump(&jump_context, jump_cond_target, false);
        Ok(())
    }

    fn parse_do_while(&mut self, builder: &mut SsaBuilder) -> PResult<()> {
        expect!(self, "do");
        let loop_target = builder.next();
        builder.take_stmt_context();
        self.alter_state(
            |s| {
                s._break = true;
                s._continue = true;
            },
            |this| this.parse_stmt(builder, false),
        )?;
        let stmt_context = builder.take_stmt_context();
        expect!(self, "while");
        expect!(self, "(");
        builder.take_expr_context();
        let cond = self.parse_expr(builder)?;
        let jump_context = builder.take_expr_context();
        expect!(self, ")");
        builder.jump_cond(loop_target, cond.into(), true);
        builder.patch_expr_jump(&jump_context, loop_target, true);
        builder.patch_continue_jump(&stmt_context, loop_target);
        builder.patch_break_jump(&stmt_context, builder.next());
        Ok(())
    }

    fn parse_while(&mut self, builder: &mut SsaBuilder) -> PResult<()> {
        expect!(self, "while");
        expect!(self, "(");
        let again = builder.next();
        builder.take_expr_context();
        let expr = self.parse_expr(builder)?;
        let jump_context = builder.take_expr_context();
        expect!(self, ")");
        let cond_jump = builder.next();
        builder.jump_cond(SsaId::null(), expr.into(), false);
        builder.take_stmt_context();
        self.alter_state(
            |s| {
                s._break = true;
                s._continue = true;
            },
            |this| this.parse_stmt(builder, false),
        )?;
        let stmt_context = builder.take_stmt_context();
        builder.jump(again);

        builder.patch_continue_jump(&stmt_context, again);
        builder.patch_break_jump(&stmt_context, builder.next());
        builder.patch_jump(cond_jump, builder.next());
        builder.patch_expr_jump(&jump_context, builder.next(), false);
        Ok(())
    }

    fn parse_break(&mut self, builder: &mut SsaBuilder) -> PResult<()> {
        if !self.state._break {
            unexpected!(self => "break is not allowed in this context");
        }
        expect!(self, "break");
        if let Some(t!("ident")) = self.peek_with_lt()?.map(|e| e.kind) {
            to_do!(self)
        }
        eat!(self, ";");
        builder.jump_break();
        Ok(())
    }

    fn parse_continue(&mut self, builder: &mut SsaBuilder) -> PResult<()> {
        if !self.state._continue {
            unexpected!(self => "continue is not allowed in this context");
        }
        expect!(self, "continue");
        if let Some(t!("ident")) = self.peek_with_lt()?.map(|e| e.kind) {
            to_do!(self)
        }
        eat!(self, ";");
        builder.jump_continue();
        Ok(())
    }
}
