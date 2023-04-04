use common::unicode::{CharExt, Utf16Ext};
use token::{t, Token};

use crate::Lexer;

impl<'a> Lexer<'a> {
    pub(super) fn lex_ident(&mut self, start: char) -> Token {
        let (lead, trail) = start.encode_utf16_code_point();
        self.buffer.push(lead);
        if let Some(x) = trail {
            self.buffer.push(x);
        }

        while let Some(x) = self.next_unit() {
            debug_assert!(self.peek.is_none());
            let char = x.decode_utf16_with(|| self.next_unit().expect("invalid utf16"));
            if !char.is_xid_continue() {
                // We read a unit to much so put it back for the next token.
                if let (lead, Some(trail)) = char.encode_utf16_code_point() {
                    self.overread = Some(lead);
                    self.end -= 2;
                    self.peek = Some(trail);
                } else {
                    self.end -= 1;
                    self.peek = Some(x);
                }
                break;
            }
            self.buffer.push(x);
        }

        //TODO: check for keywords.

        let id = self.finish_string();
        self.finish_token(t!("ident"), Some(id))
    }
}
