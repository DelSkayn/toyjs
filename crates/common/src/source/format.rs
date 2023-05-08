use core::fmt;
use std::error::Error as ErrorTrait;

use crate::{span::Span, string::Encoding};

use super::Source;

#[derive(Debug)]
pub enum Error {
    InvalidSpan,
    Fmt(fmt::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Error::InvalidSpan => write!(f, "span not valid for error"),
            Error::Fmt(x) => x.fmt(f),
        }
    }
}

impl ErrorTrait for Error {}

impl From<fmt::Error> for Error {
    fn from(value: fmt::Error) -> Self {
        Error::Fmt(value)
    }
}

impl Source {
    pub fn render_span_location<W: fmt::Write>(&self, w: &mut W, span: Span) -> Result<(), Error> {
        let location = self.locate_span(span).ok_or(Error::InvalidSpan)?;

        if let Some(ref name) = self.name {
            write!(w, "{}:", name)?
        } else {
            write!(w, "??:")?
        }

        write!(
            w,
            "{}:{}",
            location.start.line + 1,
            location.start.column + 1
        )?;

        Ok(())
    }
    pub fn render_string_block<W: fmt::Write>(
        &self,
        w: &mut W,
        span: Span,
        message: Option<Encoding>,
    ) -> Result<(), Error> {
        const MAX_LINE_CHARS: usize = 50;

        let location = self.locate_span(span.clone()).ok_or(Error::InvalidSpan)?;

        if location.start.line != location.end.line {
            todo!()
        }

        let line = self
            .line_span(location.start.line as usize)
            .ok_or(Error::InvalidSpan)?;
        let line_name = location.start.line + 1;
        let n_char = line_name.ilog10() + 1;

        let line = self.source.encoding().slice(line);
        let n_chars = line.chars().count();
        let mut offset = 0;

        if n_chars > MAX_LINE_CHARS {
            let rem = n_chars - MAX_LINE_CHARS;
            offset = (rem + 1) / 2;
        }

        // Write initial `    | `
        for _ in 0..n_char {
            write!(w, " ")?
        }
        writeln!(w, "  |")?;

        // Write initial `2 | foo.bar`
        write!(w, " {} | ", line_name)?;
        if offset == 0 {
            writeln!(w, "{}", line)?;
        } else {
            write!(w, "...")?;
            line.chars()
                .skip(offset + 3)
                .take(MAX_LINE_CHARS - 6)
                .try_for_each(|x| w.write_char(x))?;
            writeln!(w, "...")?;
        }

        // Write `  | ^ invalid identifier`
        for _ in 0..n_char {
            write!(w, " ")?
        }
        write!(w, "  | ")?;

        for _ in 0..(location.start.column - offset as u32) {
            write!(w, " ")?
        }

        let span_len = self.source.encoding().slice(span).chars().count();
        for _ in 0..span_len {
            write!(w, "^")?
        }

        if let Some(x) = message {
            writeln!(w, " {}", x)?
        }

        Ok(())
    }
}
