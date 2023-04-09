use core::fmt;
use std::cell::RefCell;

use crate::{span::Span, string::String, unicode::units};

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
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
                if u == units::LF {
                    if let Some((_, units::CR)) = iter.peek() {
                        iter.next();
                    }
                }
                lines.push(Span::from(offset..idx));
                offset = idx;
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
