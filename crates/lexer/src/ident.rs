use common::unicode::{self, chars, CharExt, Utf16Ext};
use phf::phf_map;
use token::{Keyword, Token, TokenKind, UnreservedKeyword};

use crate::Lexer;

static KEYWORDS: phf::Map<&'static [u8], TokenKind> = phf_map! {
    b"break" => TokenKind::Keyword(Keyword::Break),
    b"case" => TokenKind::Keyword(Keyword::Case),
    b"catch" => TokenKind::Keyword(Keyword::Catch),
    b"class" => TokenKind::Keyword(Keyword::Class),
    b"const" => TokenKind::Keyword(Keyword::Const),
    b"continue" => TokenKind::Keyword(Keyword::Continue),
    b"debugger" => TokenKind::Keyword(Keyword::Debugger),
    b"default" => TokenKind::Keyword(Keyword::Default),
    b"delete" => TokenKind::Keyword(Keyword::Delete),
    b"do" => TokenKind::Keyword(Keyword::Do),
    b"else" => TokenKind::Keyword(Keyword::Else),
    b"enum" => TokenKind::Keyword(Keyword::Enum),
    b"export" => TokenKind::Keyword(Keyword::Export),
    b"extends" => TokenKind::Keyword(Keyword::Extends),
    b"false" => TokenKind::Keyword(Keyword::False),
    b"finally" => TokenKind::Keyword(Keyword::Finally),
    b"for" => TokenKind::Keyword(Keyword::For),
    b"function" => TokenKind::Keyword(Keyword::Function),
    b"if" => TokenKind::Keyword(Keyword::If),
    b"import" => TokenKind::Keyword(Keyword::Import),
    b"in" => TokenKind::Keyword(Keyword::In),
    b"instanceof" => TokenKind::Keyword(Keyword::Instanceof),
    b"new" => TokenKind::Keyword(Keyword::New),
    b"null" => TokenKind::Keyword(Keyword::Null),
    b"return" => TokenKind::Keyword(Keyword::Return),
    b"super" => TokenKind::Keyword(Keyword::Super),
    b"switch" => TokenKind::Keyword(Keyword::Switch),
    b"this" => TokenKind::Keyword(Keyword::This),
    b"throw" => TokenKind::Keyword(Keyword::Throw),
    b"true" => TokenKind::Keyword(Keyword::True),
    b"try" => TokenKind::Keyword(Keyword::Try),
    b"typeof" => TokenKind::Keyword(Keyword::Typeof),
    b"var" => TokenKind::Keyword(Keyword::Var),
    b"void" => TokenKind::Keyword(Keyword::Void),
    b"while" => TokenKind::Keyword(Keyword::While),
    b"with" => TokenKind::Keyword(Keyword::With),
    b"await" => TokenKind::UnreservedKeyword(UnreservedKeyword::Await),
    b"yield" => TokenKind::UnreservedKeyword(UnreservedKeyword::Yield),
    b"let" => TokenKind::UnreservedKeyword(UnreservedKeyword::Let),
    b"static" => TokenKind::UnreservedKeyword(UnreservedKeyword::Static),
    b"implements" => TokenKind::UnreservedKeyword(UnreservedKeyword::Implements),
    b"interface" => TokenKind::UnreservedKeyword(UnreservedKeyword::Interface),
    b"package" => TokenKind::UnreservedKeyword(UnreservedKeyword::Package),
    b"private" => TokenKind::UnreservedKeyword(UnreservedKeyword::Private),
    b"protected" => TokenKind::UnreservedKeyword(UnreservedKeyword::Protected),
    b"public" => TokenKind::UnreservedKeyword(UnreservedKeyword::Public),
    b"as" => TokenKind::UnreservedKeyword(UnreservedKeyword::As),
    b"async" => TokenKind::UnreservedKeyword(UnreservedKeyword::Async),
    b"from" => TokenKind::UnreservedKeyword(UnreservedKeyword::From),
    b"get" => TokenKind::UnreservedKeyword(UnreservedKeyword::Get),
    b"meta" => TokenKind::UnreservedKeyword(UnreservedKeyword::Meta),
    b"of" => TokenKind::UnreservedKeyword(UnreservedKeyword::Of),
    b"set" => TokenKind::UnreservedKeyword(UnreservedKeyword::Set),
    b"target" => TokenKind::UnreservedKeyword(UnreservedKeyword::Target),
};

impl<'a> Lexer<'a> {
    pub fn lex_ident_escape(&mut self) -> bool {
        const U: u16 = b'u' as u16;

        if let Some(U) = self.next_unit() {
            self.lex_escape_unicode()
        } else {
            false
        }
    }

    fn is_keyword(&mut self) -> Option<TokenKind> {
        if self.builder.is_ascii() {
            KEYWORDS.get(&self.builder.ascii).cloned()
        } else {
            None
        }
    }

    pub(super) fn lex_ident(&mut self, start: char) -> Token {
        let mut has_escape_code = false;
        if start == '\\' {
            if !self.lex_ident_escape() {
                self.builder.clear();
                return self.finish_token(TokenKind::Unknown);
            }
            has_escape_code = true;
        } else {
            let (lead, trail) = start.encode_utf16_code_point();
            self.builder.push(lead);
            if let Some(x) = trail {
                self.builder.push(x);
            }
        }

        while let Some(x) = self.next_unit() {
            if x.is_ascii() {
                if unicode::byte_is_continue(x as u8) {
                    self.builder.push(x);
                } else if x as u8 == b'\\' {
                    if !self.lex_ident_escape() {
                        self.builder.clear();
                        return self.finish_token(TokenKind::Unknown);
                    }
                    has_escape_code = true;
                } else {
                    self.peek = Some(x);
                    self.end -= 1;
                    break;
                }
            } else {
                debug_assert!(self.peek.is_none());
                let char = x.decode_utf16_with(|| self.next_unit().expect("invalid utf16"));
                if !char.is_xid_continue() && char != chars::ZWNJ && char != chars::ZWJ {
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
                self.builder.push(x);
            }
        }

        if let Some(x) = self.is_keyword() {
            self.builder.ascii.clear();
            if has_escape_code {
                self.finish_token(TokenKind::Unknown);
            }
            return self.finish_token(x);
        }

        self.finish_token_string(TokenKind::Ident)
    }
}
