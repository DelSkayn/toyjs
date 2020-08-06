use std::{
    convert::TryInto,
    fmt,
    path::{Path, PathBuf},
    sync::Arc,
};

#[derive(Debug)]
pub struct Sourced<'a, T> {
    pub source: &'a Source,
    pub value: T,
}

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
pub struct Source {
    /// Line offsets
    base_offset: usize,
    lines: Arc<Vec<Span>>,
    pub src: String,
    path: Option<PathBuf>,
}

impl Source {
    pub fn new(src: String, path: Option<PathBuf>) -> Self {
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
            path,
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

    pub fn line(&self, line: u32) -> &str {
        let span = self.lines[(line - 1) as usize];
        &self.src[span.lo as usize..span.hi as usize]
    }

    pub fn str(&self, span: Span) -> &str {
        &self.src[span.lo as usize..span.hi as usize]
    }

    pub fn path(&self) -> Option<&Path> {
        self.path.as_ref().map(|x| x.as_path())
    }

    pub fn wrap<T>(&self, value: T) -> Sourced<T> {
        Sourced {
            value,
            source: &self,
        }
    }

    pub fn fmt_span(&self, f: &mut fmt::Formatter<'_>, span: Span) -> fmt::Result {
        write!(f, " --> ")?;
        if let Some(x) = self.path() {
            write!(f, "{}", x.display())?;
        } else {
            write!(f, "??")?;
        }
        if let Some((a, _)) = self.get_source_position(span) {
            writeln!(f, ":{}:{}", a.line, a.column)?;
        } else {
            writeln!(f, ":??:??")?;
        }
        Ok(())
    }

    pub fn fmt_span_src(
        &self,
        f: &mut fmt::Formatter<'_>,
        span: Span,
        message: Option<&'static str>,
    ) -> fmt::Result {
        if let Some((a, _)) = self.get_source_position(span) {
            let line = self.line(a.line);
            let num_chars = line[0 as usize..a.column as usize].chars().count();
            writeln!(f, "\t | ")?;
            writeln!(f, "{}\t | {}", a.line, line)?;
            write!(f, "\t | ")?;
            for _ in 0..num_chars {
                write!(f, " ")?;
            }
            write!(f, "^")?;
            if let Some(m) = message {
                writeln!(f, " {}", m)?;
            } else {
                writeln!(f, "")?;
            }
            writeln!(f, "\t | ")?;
        }
        Ok(())
    }
}
