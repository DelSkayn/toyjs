use super::*;
use crate::token::Kw;
use unicode_xid::UnicodeXID;

impl<'a> Lexer<'a> {
    // TODO handle unicode escape codes
    fn is_token_start(c: char) -> bool {
        if c.is_xid_start() || c == '$' || c == '_' {
            return true;
        }
        return false;
    }

    // TODO handle unicode escape codes
    fn is_token_cont(c: char) -> bool {
        if c.is_xid_continue() || c == '$' || c == chars::ZWNJ || c == chars::ZWJ {
            return true;
        }
        return false;
    }

    fn to_keyword(x: &str) -> Option<Kw> {
        match x {
            "await" => Some(Kw::Await),
            "break" => Some(Kw::Break),
            "case" => Some(Kw::Case),
            "catch" => Some(Kw::Catch),
            "class" => Some(Kw::Class),
            "let" => Some(Kw::Let),
            "const" => Some(Kw::Const),
            "continue" => Some(Kw::Continue),
            "debugger" => Some(Kw::Debugger),
            "default" => Some(Kw::Default),
            "delete" => Some(Kw::Delete),
            "do" => Some(Kw::Do),
            "else" => Some(Kw::Else),
            "enum" => Some(Kw::Enum),
            "export" => Some(Kw::Export),
            "extends" => Some(Kw::Extends),
            "false" => Some(Kw::False),
            "finally" => Some(Kw::Finally),
            "for" => Some(Kw::For),
            "function" => Some(Kw::Function),
            "if" => Some(Kw::If),
            "import" => Some(Kw::Import),
            "in" => Some(Kw::In),
            "instanceof" => Some(Kw::Instanceof),
            "new" => Some(Kw::New),
            "null" => Some(Kw::Null),
            "return" => Some(Kw::Return),
            "super" => Some(Kw::Super),
            "switch" => Some(Kw::Switch),
            "this" => Some(Kw::This),
            "throw" => Some(Kw::Throw),
            "true" => Some(Kw::True),
            "try" => Some(Kw::Try),
            "typeof" => Some(Kw::Typeof),
            "var" => Some(Kw::Var),
            "void" => Some(Kw::Void),
            "while" => Some(Kw::While),
            "with" => Some(Kw::With),
            _ => None,
        }
    }

    pub fn lex_ident(&mut self) -> Result<Option<Token>> {
        self.buffer.clear();
        self.cur -= 1;
        let c = self.next_char()?.unwrap();
        if !Self::is_token_start(c) {
            dbg!(c);
            return Err(self.error(LexerErrorKind::UnknownToken));
        }
        self.buffer.push(c);
        while let Some(x) = self.peek_char()? {
            if !Self::is_token_cont(x) {
                break;
            }
            self.buffer.push(x);
            self.next_char().ok();
        }
        if let Some(x) = Self::to_keyword(&self.buffer) {
            return self.token(TokenKind::Kw(x));
        }
        let s = self.interner.intern(&self.buffer);
        return self.token(TokenKind::Ident(s));
    }
}
