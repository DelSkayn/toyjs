use core::fmt;
use std::cell::RefCell;

use crate::{
    span::Span,
    string::{Encoding, String},
    unicode::units,
};

mod format;
pub use format::Error as FormatError;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct SpanLocation {
    /// Where the span starts,
    pub start: Location,
    /// Where the span ends,
    pub end: Location,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

struct SourceInfo {
    lines: Vec<Span>,
}

impl SourceInfo {
    pub fn new(source: &Source) -> Self {
        let mut lines = Vec::new();
        let mut iter = source.source.encoding().units().enumerate().peekable();
        let mut offset = 0;
        while let Some((idx, u)) = iter.next() {
            if units::LINE_TERMINATOR.contains(&u) {
                if u == units::CR {
                    if let Some((_, units::LF)) = iter.peek() {
                        iter.next();
                    }
                }
                lines.push(Span::from(offset..idx));
                offset = idx + 1;
            }
        }

        Self { lines }
    }
}

pub struct Source {
    name: Option<String>,
    source: String,
    info: RefCell<Option<Box<SourceInfo>>>,
}

impl Source {
    pub fn new<N: Into<String>, S: Into<String>>(source: S, name: Option<N>) -> Self {
        Source {
            name: name.map(|x| x.into()),
            source: source.into(),
            info: RefCell::new(None),
        }
    }

    pub fn source(&self) -> Encoding {
        self.source.encoding()
    }

    /// Returns the span for a given line.
    pub fn line_span(&self, line: usize) -> Option<Span> {
        let mut info = self.info.borrow_mut();
        let info = info.get_or_insert_with(|| Box::new(SourceInfo::new(self)));
        info.lines.get(line).cloned()
    }

    /// Returns the start and end location for a given span.
    pub fn locate_span(&self, span: Span) -> Option<SpanLocation> {
        let mut info = self.info.borrow_mut();
        let info = info.get_or_insert_with(|| Box::new(SourceInfo::new(self)));

        let offset = span.offset();
        let (start_line, start_span) = info
            .lines
            .iter()
            .enumerate()
            .find(|(_, x)| x.contains_offset(offset))?;
        let (end_line, end_span) = info
            .lines
            .iter()
            .enumerate()
            .find(|(_, x)| x.contains_offset(offset))?;

        let start_col = self
            .source
            .encoding()
            .slice(start_span.offset()..span.offset())
            .chars()
            .count()
            .try_into()
            .unwrap();
        let end_col = self
            .source
            .encoding()
            .slice(end_span.offset()..(span.offset() + span.size()))
            .chars()
            .count()
            .try_into()
            .unwrap();

        Some(SpanLocation {
            start: Location {
                line: start_line.try_into().unwrap(),
                column: start_col,
            },
            end: Location {
                line: end_line.try_into().unwrap(),
                column: end_col,
            },
        })
    }
}

#[cfg(test)]
mod test {
    use crate::{
        source::{Location, SpanLocation},
        span::Span,
    };

    use super::Source;
    use std::str;

    #[test]
    fn locate() {
        let source = b"\n\n\n \n\n  tgt\n\n";
        let source = Source::new(str::from_utf8(source).unwrap(), Some("test"));
        assert_eq!(source.line_span(0).unwrap(), Span::new(0, 0));
        assert_eq!(source.line_span(1).unwrap(), Span::new(1, 0));
        assert_eq!(source.line_span(2).unwrap(), Span::new(2, 0));
        assert_eq!(source.line_span(3).unwrap(), Span::new(3, 1));
        assert_eq!(source.line_span(4).unwrap(), Span::new(5, 0));
        assert_eq!(source.line_span(5).unwrap(), Span::new(6, 5));
        assert_eq!(source.line_span(6).unwrap(), Span::new(12, 0));
        assert!(source.line_span(7).is_none());

        let span = Span::new(3, 0);
        assert_eq!(
            source.locate_span(span).unwrap(),
            SpanLocation {
                start: Location { line: 3, column: 0 },
                end: Location { line: 3, column: 0 }
            }
        );

        let span = Span::new(8, 3);
        assert_eq!(
            source.locate_span(span).unwrap(),
            SpanLocation {
                start: Location { line: 5, column: 2 },
                end: Location { line: 5, column: 5 }
            }
        );
    }
}
