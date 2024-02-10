use core::fmt;
use std::{
    error::Error as ErrorTrait,
    fmt::{Display, Write},
};

use super::{Location, Source};
use crate::{
    span::Span,
    string::{Ascii, Encoding, String},
};

#[derive(Debug)]
pub enum Truncation {
    None,
    Start,
    End,
    Both,
}

#[derive(Debug)]
pub struct RenderedLine {
    source_name: Option<String>,
    location: Location,
    line: String,
    message: Option<String>,
    truncation: Truncation,
    offset: u32,
    length: u32,
}

impl RenderedLine {
    pub fn as_block(&self) -> SourceBlock {
        SourceBlock(self)
    }

    pub fn as_highlight(&self) -> HighlightToken {
        HighlightToken(self)
    }
}

pub struct SourceBlock<'a>(&'a RenderedLine);

impl Display for SourceBlock<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line_num_length = self.0.location.line.ilog2() + 1;
        for _ in 0..line_num_length {
            f.write_char(' ')?;
        }
        writeln!(
            f,
            " ╭─┤{}:{}:{}│",
            self.0
                .source_name
                .as_ref()
                .map(|x| x.encoding())
                .unwrap_or(Encoding::Ascii(Ascii::from_slice(b"??").unwrap())),
            self.0.location.line + 1,
            self.0.location.column + 1,
        )?;
        write!(f, "{} │ ", self.0.location.line + 1)?;
        if let Truncation::Start | Truncation::Both = self.0.truncation {
            write!(f, "...")?;
        }
        write!(f, "{}", self.0.line)?;
        if let Truncation::End | Truncation::Both = self.0.truncation {
            write!(f, "...")?;
        }

        f.write_char('\n')?;

        for _ in 0..line_num_length {
            f.write_char(' ')?;
        }

        f.write_str(" ╰─")?;

        for _ in 0..self.0.offset {
            f.write_char('─')?;
        }
        for _ in 0..self.0.length {
            f.write_char('^')?;
        }

        if let Some(msg) = self.0.message.as_ref() {
            write!(f, " {msg}")?;
        }

        writeln!(f)?;

        Ok(())
    }
}

pub struct HighlightToken<'a>(&'a RenderedLine);

impl Display for HighlightToken<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "│{}:{}:{}│> ",
            self.0
                .source_name
                .as_ref()
                .map(|x| x.encoding())
                .unwrap_or(Encoding::Ascii(Ascii::from_slice(b"??").unwrap())),
            self.0.location.line + 1,
            self.0.location.column + 1,
        )?;

        if let Truncation::Start | Truncation::Both = self.0.truncation {
            write!(f, "...")?;
        }

        let start = self.0.line.encoding().slice(..(self.0.offset as usize));
        write!(f, "{}", start)?;
        let point = self
            .0
            .line
            .encoding()
            .slice((self.0.offset as usize)..((self.0.offset + self.0.length) as usize));
        write!(f, "\x1b[1;30;44m{}\x1b[0m", point)?;
        let remaining = self
            .0
            .line
            .encoding()
            .slice(((self.0.offset + self.0.length) as usize)..);
        write!(f, "{}", remaining)?;
        if let Truncation::End | Truncation::Both = self.0.truncation {
            write!(f, "...")?;
        }
        Ok(())
    }
}

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
    const MAX_LINE_CHARS: u32 = 80;
    const TRUNCATED_LINE_CHARS: u32 = Self::MAX_LINE_CHARS - 6;

    pub fn render_span(&self, span: Span, message: Option<String>) -> Result<RenderedLine, Error> {
        let mut location = self.locate_span(span).ok_or(Error::InvalidSpan)?;

        if location.start.line != location.end.line {
            // TODO: render span that covers multiple lines
            location.end.line = location.start.line;
            location.end.column = location.start.column + 1;
        }

        let line_span = self
            .line_span(location.start.line as usize)
            .ok_or(Error::InvalidSpan)?;

        let line = self.source.encoding().slice(line_span);
        let start_line_length = line.chars().count();
        let line = line.trim_start();
        let line_length = line.chars().count();
        let whitespace_offset = (start_line_length - line_length) as u32;
        let line = line.trim_end();

        if line_length > Self::MAX_LINE_CHARS as usize {
            let location_length =
                (location.end.column - location.start.column).min(Self::TRUNCATED_LINE_CHARS);

            let middle = location.start.column + location_length / 2;
            let truncated_start = middle - (Self::TRUNCATED_LINE_CHARS / 2).min(middle);
            let truncated_end =
                (truncated_start + Self::TRUNCATED_LINE_CHARS).min(line_length as u32);
            let truncated_start = truncated_end - Self::TRUNCATED_LINE_CHARS;

            let offset = location.start.column - truncated_start - whitespace_offset;

            let truncation = if truncated_start == 0 {
                Truncation::End
            } else if truncated_end == line_length as u32 {
                Truncation::Start
            } else {
                Truncation::Both
            };

            return Ok(RenderedLine {
                truncation,
                location: location.start,
                line: line
                    .slice((truncated_start as usize)..(truncated_end as usize))
                    .into(),
                source_name: self.name.clone(),
                offset,
                length: location_length,
                message,
            });
        }

        Ok(RenderedLine {
            truncation: Truncation::None,
            location: location.start,
            line: line.into(),
            source_name: self.name.clone(),
            offset: location.start.column,
            length: location.end.column - location.start.column,
            message,
        })
    }
}
