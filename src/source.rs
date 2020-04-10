use std::{convert::TryInto, sync::Arc};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

/// A source location
pub struct Pos {
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Debug)]
pub struct Source<'a> {
    /// Line offsets
    base_offset: usize,
    lines: Arc<Vec<Span>>,
    src: &'a str,
}

impl<'a> Source<'a> {
    pub fn new(src: &'a str) -> Self {
        let base_offset = src.as_ptr() as usize;
        let lines = Arc::new(
            src.lines()
                .map(|e| {
                    let start = e.as_ptr() as usize;
                    let end = start + e.len();
                    Span {
                        lo: (start - base_offset).try_into().unwrap(),
                        hi: (end - base_offset).try_into().unwrap(),
                    }
                })
                .collect(),
        );
        Source {
            base_offset,
            lines,
            src,
        }
    }

    pub fn get_source_position(&self, span: Span) -> Option<(Pos, Pos)> {
        let (line, line_span) = self
            .lines
            .iter()
            .enumerate()
            .filter(|(_, line)| line.hi >= span.lo)
            .next()?;
        let start = Pos {
            line: line as u32 + 1,
            column: span.lo - line_span.lo,
        };

        let (end_line, line_span) = self.lines[line..]
            .iter()
            .enumerate()
            .filter(|(_, line)| line.hi >= span.hi)
            .next()?;

        let end = Pos {
            line: end_line as u32 + 1,
            column: span.lo - line_span.lo,
        };
        Some((start, end))
    }

    pub fn line(&self, line: u32) -> &'a str {
        let span = self.lines[(line - 1) as usize];
        &self.src[span.lo as usize..span.hi as usize]
    }

    pub fn str(&self, span: Span) -> &'a str {
        &self.src[span.lo as usize..span.hi as usize]
    }
}
