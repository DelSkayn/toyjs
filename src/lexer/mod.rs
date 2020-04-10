use crate::{
    source::{Source, Span},
    tok,
    token::*,
};
use std::{convert::TryInto, str::Chars};
mod chars;
pub use crate::token::Kw;
use unicode_xid::UnicodeXID;

pub struct CharStream<'a> {
    chars: Chars<'a>,
    peek: Option<char>,
    peek_str: Option<&'a str>,
}

impl<'a> CharStream<'a> {
    pub fn new(s: &'a str) -> CharStream {
        CharStream {
            chars: s.chars(),
            peek: None,
            peek_str: None,
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        if self.peek.is_none() {
            self.peek_str = Some(self.chars.as_str());
            self.peek = self.chars.next();
        }
        return self.peek;
    }

    pub fn as_str(&self) -> &'a str {
        match self.peek_str {
            Some(x) => x,
            None => self.chars.as_str(),
        }
    }

    pub fn next(&mut self) -> Option<char> {
        if self.peek.is_some() {
            self.peek_str = None;
            return self.peek.take();
        }
        self.chars.next()
    }
}

pub struct LexerError {
    column: u64,
    line: u64,
    msg: String,
}

pub struct Lexer<'a> {
    pub line: u64,
    pub column: u64,
    pub str_offset_start: usize,
    pub src: Source<'a>,
    pub chars: CharStream<'a>,
    pub errors: Vec<LexerError>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        (*self).next()
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            line: 0,
            column: 0,
            src: Source::new(source),
            str_offset_start: source.as_ptr() as usize,
            chars: CharStream::new(source),
            errors: Vec::new(),
        }
    }

    pub fn next(&mut self) -> Option<Token<'a>> {
        let (c, span) = self.next_char()?;
        let t = match c {
            ';' => self.token(span, tok!(";")),
            ',' => self.token(span, tok!(",")),
            '(' => self.token(span, tok!("(")),
            ')' => self.token(span, tok!(")")),
            '{' => self.token(span, tok!("{")),
            '}' => self.token(span, tok!("}")),
            '[' => self.token(span, tok!("[")),
            ']' => self.token(span, tok!("]")),
            '+' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::BinOpAssign(BinOpToken::Plus))
                }
                Some('+') => {
                    self.next_char();
                    self.token(span, tok!("++"))
                }
                _ => self.token(span, TokenKind::BinOp(BinOpToken::Plus)),
            },
            '-' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::BinOpAssign(BinOpToken::Minus))
                }
                Some('-') => {
                    self.next_char();
                    self.token(span, tok!("--"))
                }
                _ => self.token(span, TokenKind::BinOp(BinOpToken::Minus)),
            },
            '*' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::BinOpAssign(BinOpToken::Mul))
                }
                Some('*') => {
                    self.next_char();
                    match self.chars.peek() {
                        Some('=') => {
                            self.next_char();
                            self.token(span, TokenKind::ExponentiateAssign)
                        }
                        _ => self.token(span, TokenKind::Exponentiate),
                    }
                }
                _ => self.token(span, TokenKind::BinOp(BinOpToken::Mul)),
            },
            '/' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::BinOpAssign(BinOpToken::Div))
                }
                Some('/') => {
                    self.next_char();
                    match self.chars.peek() {
                        Some('=') => {
                            self.next_char();
                            self.token(span, TokenKind::BinOpAssign(BinOpToken::IntegerDiv))
                        }
                        _ => self.token(span, TokenKind::BinOp(BinOpToken::IntegerDiv)),
                    }
                }
                _ => self.token(span, tok!("/")),
            },
            '~' => self.token(span, tok!("~")),
            '%' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::BinOpAssign(BinOpToken::Remainder))
                }
                _ => self.token(span, TokenKind::BinOp(BinOpToken::Remainder)),
            },
            '<' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::LessEqual)
                }
                Some('<') => {
                    self.next_char();
                    match self.chars.peek() {
                        Some('=') => {
                            self.next_char();
                            self.token(span, TokenKind::BinOpAssign(BinOpToken::LeftShift))
                        }
                        _ => self.token(span, TokenKind::BinOp(BinOpToken::LeftShift)),
                    }
                }
                _ => self.token(span, TokenKind::Less),
            },
            '>' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::GreaterEqual)
                }
                Some('>') => {
                    self.next_char();
                    match self.chars.peek() {
                        Some('=') => {
                            self.next_char();
                            self.token(span, TokenKind::BinOpAssign(BinOpToken::RightShift))
                        }
                        Some('>') => {
                            self.next_char();
                            match self.chars.peek() {
                                Some('=') => {
                                    self.next_char();
                                    self.token(
                                        span,
                                        TokenKind::BinOpAssign(BinOpToken::UnsignedRightShift),
                                    )
                                }
                                _ => self
                                    .token(span, TokenKind::BinOp(BinOpToken::UnsignedRightShift)),
                            }
                        }
                        _ => self.token(span, TokenKind::BinOp(BinOpToken::RightShift)),
                    }
                }
                _ => self.token(span, TokenKind::Greater),
            },
            '=' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    match self.chars.peek() {
                        Some('=') => {
                            self.next_char();
                            self.token(span, TokenKind::StrictEqual)
                        }
                        _ => self.token(span, TokenKind::Equal),
                    }
                }
                _ => self.token(span, TokenKind::Assign),
            },
            '!' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    match self.chars.peek() {
                        Some('=') => {
                            self.next_char();
                            self.token(span, TokenKind::StrictNotEqual)
                        }
                        _ => self.token(span, TokenKind::NotEqual),
                    }
                }
                _ => self.token(span, tok!("!")),
            },
            '&' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::BinOpAssign(BinOpToken::BitwiseAnd))
                }
                Some('&') => {
                    self.next_char();
                    self.token(span, TokenKind::And)
                }
                _ => self.token(span, TokenKind::BinOp(BinOpToken::BitwiseAnd)),
            },
            '|' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::BinOpAssign(BinOpToken::BitwiseOr))
                }
                Some('|') => {
                    self.next_char();
                    self.token(span, TokenKind::Or)
                }
                _ => self.token(span, TokenKind::BinOp(BinOpToken::BitwiseOr)),
            },
            '^' => match self.chars.peek() {
                Some('=') => {
                    self.next_char();
                    self.token(span, TokenKind::BinOpAssign(BinOpToken::BitwiseXor))
                }
                _ => self.token(span, TokenKind::BinOp(BinOpToken::BitwiseXor)),
            },
            '.' => match self.chars.peek() {
                Some('.') => {
                    self.next_char();
                    match self.chars.peek() {
                        Some('.') => {
                            self.next_char();
                            self.token(span, TokenKind::DotDotDot)
                        }
                        _ => self.token(span, TokenKind::DotDot),
                    }
                }
                Some(x) if x.is_digit(10) => self.parse_number(span, '.'),
                _ => self.token(span, TokenKind::Dot),
            },
            ':' => match self.chars.peek() {
                Some(':') => {
                    self.next_char();
                    self.token(span, TokenKind::DoubleColon)
                }
                _ => self.token(span, TokenKind::Colon),
            },
            '"' => self.parse_string(span, '"'),
            '\'' => self.parse_string(span, '\''),
            x if Lexer::is_line_terminator(x) => {
                self.consume_line_terminator(x);
                self.column = 0;
                self.line += 1;
                self.token(span, tok!("\n"))
            }
            x if Lexer::is_whitespace(x) => self.parse_ident(span, x),
            x if x.is_digit(10) => self.parse_number(span, x),
            x if Lexer::is_token_start(x) => self.parse_ident(span, x),
            _ => {
                self.error("unknown token");
                self.token(span, TokenKind::Unknown)
            }
        };
        Some(t)
    }

    fn token(&self, start: &'a str, kind: TokenKind<'a>) -> Token<'a> {
        let lo = (start.as_ptr() as usize - self.str_offset_start)
            .try_into()
            .unwrap();
        let hi = (self.chars.as_str().as_ptr() as usize - self.str_offset_start)
            .try_into()
            .unwrap();
        let span = Span { lo, hi };
        Token { kind, span }
    }

    fn next_char(&mut self) -> Option<(char, &'a str)> {
        let mut c;
        loop {
            let span = self.chars.as_str();
            c = self.chars.next()?;
            self.column += 1;
            if Lexer::is_whitespace(c) {
                continue;
            }
            if c == '/' {
                match self.chars.peek() {
                    Some('/') => {
                        self.chars.next();
                        self.consume_line_comment();
                        continue;
                    }
                    Some('*') => {
                        self.chars.next();
                        self.consume_multi_line_comment();
                        continue;
                    }
                    _ => {}
                }
            }
            return Some((c, span));
        }
    }

    fn consume_line_comment(&mut self) {
        while let Some(x) = self.chars.peek() {
            if Lexer::is_line_terminator(x) {
                return;
            }
            self.column += 1;
            self.chars.next();
        }
    }

    fn consume_multi_line_comment(&mut self) {
        while let Some(x) = self.chars.next() {
            self.column += 1;
            if Lexer::is_line_terminator(x) {
                self.column = 0;
                self.line += 1;
            }
            if x == '*' {
                match self.chars.peek() {
                    Some('/') => {
                        self.chars.next();
                        self.column += 1;
                        return;
                    }
                    _ => {}
                }
            }
        }
    }

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

    fn is_whitespace(c: char) -> bool {
        c == chars::TAB
            || c == chars::VT
            || c == chars::FF
            || c == chars::SP
            || c == chars::NBSP
            || c == chars::ZWNBSP
            || chars::USP.contains(&c)
    }

    fn is_line_terminator(c: char) -> bool {
        c == chars::LF || c == chars::CR || c == chars::LS || c == chars::PS
    }

    fn consume_line_terminator(&mut self, prev: char) {
        // Consume windows style newline
        if prev == chars::CR && self.chars.peek().map(|x| x == chars::LF).unwrap_or(false) {
            self.chars.next();
        }
    }

    fn parse_ident(&mut self, span: &'a str, start: char) -> Token<'a> {
        let mut t = self.parse_ident_raw(span, start);
        let kw = match t.kind {
            TokenKind::Ident(x) => match x {
                "await" => Kw::Await,
                "break" => Kw::Break,
                "case" => Kw::Case,
                "catch" => Kw::Catch,
                "class" => Kw::Class,
                "let" => Kw::Let,
                "const" => Kw::Const,
                "continue" => Kw::Continue,
                "debugger" => Kw::Debugger,
                "default" => Kw::Default,
                "delete" => Kw::Delete,
                "do" => Kw::Do,
                "else" => Kw::Else,
                "enum" => Kw::Enum,
                "export" => Kw::Export,
                "extends" => Kw::Extends,
                "false" => Kw::False,
                "finally" => Kw::Finally,
                "for" => Kw::For,
                "function" => Kw::Function,
                "if" => Kw::If,
                "import" => Kw::Import,
                "in" => Kw::In,
                "instanceof" => Kw::Instanceof,
                "new" => Kw::New,
                "null" => Kw::Null,
                "return" => Kw::Return,
                "super" => Kw::Super,
                "switch" => Kw::Switch,
                "this" => Kw::This,
                "throw" => Kw::Throw,
                "true" => Kw::True,
                "try" => Kw::Try,
                "typeof" => Kw::Typeof,
                "var" => Kw::Var,
                "void" => Kw::Void,
                "while" => Kw::While,
                "with" => Kw::With,
                "yield" => Kw::Yield,
                _ => return t,
            },
            _ => return t,
        };
        t.kind = TokenKind::Kw(kw);
        t
    }

    fn parse_ident_raw(&mut self, span: &'a str, start: char) -> Token<'a> {
        debug_assert!(Lexer::is_token_start(start));
        let mut next = self.chars.peek();
        while let Some(x) = next {
            if !Lexer::is_token_cont(x) {
                break;
            } else {
                self.chars.next();
            }
            next = self.chars.peek();
        }
        let lo = span.as_ptr() as usize;
        let hi = self.chars.as_str().as_ptr() as usize;
        let string = &span[..hi - lo];
        self.token(span, TokenKind::Ident(string))
    }

    fn parse_number(&mut self, span: &'a str, start: char) -> Token<'a> {
        if start == '0' {
            match self.chars.peek() {
                None => {
                    return self.token(
                        span,
                        TokenKind::Lit(LitToken::Number {
                            big: false,
                            kind: NumberKind::Integer,
                        }),
                    )
                }
                Some('n') => {
                    self.chars.next();
                    return self.token(
                        span,
                        TokenKind::Lit(LitToken::Number {
                            big: true,
                            kind: NumberKind::Integer,
                        }),
                    );
                }
                Some('b') | Some('B') => {
                    self.chars.next();
                    return self.parse_non_decimal(span, NumberKind::Binary);
                }
                Some('o') | Some('O') => {
                    self.chars.next();
                    return self.parse_non_decimal(span, NumberKind::Octal);
                }
                Some('x') | Some('X') => {
                    self.chars.next();
                    return self.parse_non_decimal(span, NumberKind::Hex);
                }
                _ => {}
            }
        }
        if start == '.' {
            debug_assert!(self.chars.peek().map(|x| x.is_digit(10)).unwrap_or(false));
            return self.parse_number_mantissa(span);
        }
        while self.chars.peek().map(|x| x.is_digit(10)).unwrap_or(false) {
            self.chars.next();
        }
        let mut big = false;
        match self.chars.peek() {
            Some('.') => {
                self.chars.next();
                return self.parse_number_mantissa(span);
            }
            Some('e') | Some('E') => {
                self.chars.next();
                return self.parse_number_exponent(span, NumberKind::Integer);
            }
            Some('n') => {
                self.chars.next();
                big = true;
            }
            _ => {}
        }
        self.token(
            span,
            TokenKind::Lit(LitToken::Number {
                big,
                kind: NumberKind::Integer,
            }),
        )
    }

    fn parse_number_mantissa(&mut self, span: &'a str) -> Token<'a> {
        while self.chars.peek().map(|x| x.is_digit(10)).unwrap_or(false) {
            self.chars.next();
        }
        match self.chars.peek() {
            Some('e') | Some('E') => {
                self.chars.next();
                return self.parse_number_exponent(span, NumberKind::Float);
            }
            _ => {}
        }
        self.token(
            span,
            TokenKind::Lit(LitToken::Number {
                big: false,
                kind: NumberKind::Float,
            }),
        )
    }

    // TODO techically exponent must only be parsed if there is  a number following it.
    fn parse_number_exponent(&mut self, span: &'a str, num_kind: NumberKind) -> Token<'a> {
        let start = self.chars.peek();
        if start.is_none() {
            self.error("missing exponent integer");
            return self.token(
                span,
                TokenKind::Lit(LitToken::Number {
                    big: false,
                    kind: num_kind,
                }),
            );
        }
        let start = start.unwrap();
        if start == '-' || start == '+' || !start.is_digit(10) {
            self.chars.next();
            while self.chars.peek().map(|x| x.is_digit(10)).unwrap_or(false) {
                self.chars.next();
            }
            return self.token(
                span,
                TokenKind::Lit(LitToken::Number {
                    big: false,
                    kind: num_kind,
                }),
            );
        }
        self.error("invalid exponent integer");
        self.token(
            span,
            TokenKind::Lit(LitToken::Number {
                big: false,
                kind: num_kind,
            }),
        )
    }

    fn parse_non_decimal(&mut self, span: &'a str, kind: NumberKind) -> Token<'a> {
        let radix = match kind {
            NumberKind::Octal => 8,
            NumberKind::Hex => 16,
            NumberKind::Binary => 2,
            _ => panic!("invalid non decimal number kind"),
        };
        while self
            .chars
            .peek()
            .map(|c| c.is_digit(radix))
            .unwrap_or(false)
        {
            self.chars.next();
        }
        let mut big = false;
        if let Some('n') = self.chars.peek() {
            self.chars.next();
            big = true;
        }
        self.token(
            span,
            TokenKind::Lit(LitToken::Number {
                big,
                kind: NumberKind::Binary,
            }),
        )
    }

    fn parse_string(&mut self, span: &'a str, start: char) -> Token<'a> {
        loop {
            let c = self.chars.next();
            if c.is_none() || Lexer::is_line_terminator(c.unwrap()) {
                self.error("unterminated string");
                return self.token(span, TokenKind::Lit(LitToken::String));
            }
            let c = c.unwrap();
            if c == start {
                return self.token(span, TokenKind::Lit(LitToken::String));
            }
            // Parse escape codes
            if c == '\\' {
                match self.chars.peek() {
                    Some('\'') | Some('"') | Some('\\') | Some('b') | Some('f') | Some('n')
                    | Some('r') | Some('t') | Some('v') | Some('0') => {
                        self.chars.next();
                    }
                    Some('x') => {
                        self.chars.next();
                        for _ in 0..2 {
                            match self.chars.peek() {
                                Some(x) if x.is_digit(16) => {
                                    self.chars.next();
                                }
                                Some(_) => self.error("invalid hex escape sequence"),
                                None => self.error("unfinished hex escape sequence"),
                            }
                        }
                    }
                    Some('u') => {
                        self.chars.next();
                        if let Some('{') = self.chars.peek() {
                            // TODO prob wrong
                            loop {
                                match self.chars.next() {
                                    Some('}') => {
                                        break;
                                    }
                                    Some(x) if x.is_digit(16) => {}
                                    Some(_) => {
                                        self.error("invalid unicode escape sequence");
                                        break;
                                    }
                                    None => {
                                        self.error("unfinished unicode escape sequence");
                                        break;
                                    }
                                }
                            }
                        } else {
                            for _ in 0..4 {
                                match self.chars.peek() {
                                    Some(x) if x.is_digit(16) => {
                                        self.chars.next();
                                    }
                                    Some(_) => self.error("invalid unicode escape sequence"),
                                    None => self.error("unfinished unicode escape sequence"),
                                }
                            }
                        }
                    }
                    Some(x) if Lexer::is_line_terminator(x) => {
                        self.chars.next();
                        self.consume_line_terminator(x);
                    }
                    Some(_) => {
                        self.chars.next();
                    }
                    None => {
                        self.error("unfinished escape sequence");
                    }
                }
            }
        }
    }

    fn error<S: Into<String>>(&mut self, msg: S) {
        self.errors.push(LexerError {
            line: self.line,
            column: self.column,
            msg: msg.into(),
        });
    }
}
