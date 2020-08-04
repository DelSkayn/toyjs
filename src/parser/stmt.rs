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
        println!("{:?}", t!("{"));
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
            t!("{") => return self.parse_block(),
            _ => return Ok(Some(self.parse_expr()?)),
        }
        Ok(None)
    }

    fn parse_block(&mut self) -> PResult<Option<SsaVar>> {
        trace_log!("block");
        expect!(self, "{");
        println!("block");
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
            self.builder.patch_jump_target_next(jump);
            jump_cond_target
        } else {
            self.builder.next_id()
        };
        self.builder
            .patch_jump_target(jump_cond, jump_cond_target.into());
        Ok(())
    }

    fn parse_while(&mut self) -> PResult<()> {
        expect!(self, "while");
        expect!(self, "(");
        let again = self.builder.next_id();
        let expr = self.parse_expr()?;
        expect!(self, ")");
        let cond_jump = self.builder.push_instruction(Instruction::CondJump {
            negative: true,
            condition: expr.into(),
            target: InstrVar::null(),
        });
        self.alter_state(
            |s| {
                s._break = true;
                s._continue = true;
            },
            |this| this.parse_stmt(),
        )?;
        self.builder.push_instruction(Instruction::Jump {
            target: again.into(),
        });
        self.builder.patch_jump_target_next(cond_jump);
        Ok(())
    }
}
