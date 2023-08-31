use core::fmt;
use std::error::Error as ErrorTrait;

use crate::{span::Span, string::Encoding, unicode::units};

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
        const MAX_LINE_CHARS: usize = 80;

        let location = self.locate_span(span).ok_or(Error::InvalidSpan)?;

        if location.start.line != location.end.line {
            writeln!(w, "TODO: render span that covers multiple lines")?;
            return Ok(());
        }

        let line_span = self
            .line_span(location.start.line as usize)
            .ok_or(Error::InvalidSpan)?;
        let line_name = location.start.line + 1;
        // The number of characters in the line number.
        let line_number_length = line_name.ilog10() + 1;

        let line = self.source.encoding().slice(line_span);
        // The number whitespace characters at the start of the line..
        let n_whitespace = line
            .units()
            .enumerate()
            .find(|(_, x)| !units::WHITE_SPACE.contains(x))
            .map(|(x, _)| x)
            .unwrap_or(0);

        let line = line.slice(n_whitespace..).trim_end();
        let line_length = line.chars().count();
        let token_lenght = self.source.encoding().slice(span).chars().count();
        let offset = n_whitespace;

        // Write initial `    | `
        for _ in 0..line_number_length {
            write!(w, " ")?
        }
        writeln!(w, "  |")?;

        if line_length > MAX_LINE_CHARS {
            // The length of the line is to long
            // Render only part of the line

            // Buffer to push the backwards context into.
            let mut buffer = [' '; MAX_LINE_CHARS / 2];

            let context_size = MAX_LINE_CHARS.max(token_lenght + 6) - (token_lenght + 6);
            let front = context_size / 2;
            let end = context_size - front;

            // Retrieve the backwards context.
            self.source
                .encoding()
                .slice(..span.offset())
                .chars()
                .rev()
                .take(front)
                .enumerate()
                .for_each(|(idx, c)| buffer[idx] = c);

            write!(w, " {} | ...", line_name)?;
            for i in (0..front).rev() {
                write!(w, "{}", buffer[i])?;
            }

            // write the span and the forward context.
            self.source
                .encoding()
                .slice(span.offset()..)
                .chars()
                .take(end + token_lenght)
                .try_for_each(|x| write!(w, "{}", x))?;
            writeln!(w, "...")?;

            for _ in 0..line_number_length {
                write!(w, " ")?
            }
            // write the next line with the pointing part
            write!(w, "  |    ")?;
            for _ in 0..front {
                write!(w, " ")?;
            }
            for _ in 0..token_lenght {
                write!(w, "^")?;
            }
        } else {
            // Write initial `2 | foo.bar`
            write!(w, " {} | ", line_name)?;
            writeln!(w, "{}", line)?;

            // Write `  | ^ invalid identifier`
            for _ in 0..line_number_length {
                write!(w, " ")?
            }
            write!(w, "  | ")?;

            for _ in 0..(location.start.column - offset as u32) {
                write!(w, " ")?
            }

            for _ in 0..token_lenght {
                write!(w, "^")?
            }
        }

        // Optionally write message next to the pointer.
        if let Some(x) = message {
            writeln!(w, " {}", x)?
        }
        Ok(())
    }
}
