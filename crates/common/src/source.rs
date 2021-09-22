use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::VecDeque,
    convert::TryFrom,
    fmt::{self, Write},
    fs::File,
    io::{self, Read},
    path::{Path, PathBuf},
};

const MAX_LINE_LENGTH: usize = 80;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub low: u32,
    pub hi: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    line: u32,
    column: u32,
}

impl Span {
    pub fn is_within(self, other: Self) -> bool {
        self.hi <= other.hi && self.low >= other.low
    }
}

pub struct Source {
    // TODO
    pub path: Option<PathBuf>,
    source: String,
    lines: RefCell<Vec<Span>>,
}

impl Source {
    /// Create a source from a string
    pub fn from_string(source: String) -> Source {
        Source {
            path: None,
            source,
            lines: RefCell::new(Vec::new()),
        }
    }

    /// Create a source from a path loading the file at that path.
    pub fn from_path<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let mut file = File::open(path.as_ref())?;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        Ok(Source {
            path: Some(path.as_ref().to_path_buf()),
            source: buf,
            lines: RefCell::new(Vec::new()),
        })
    }

    /// Find the remaining lines in a file.
    pub fn finish_lines(&self) {
        let mut lines = self.lines.borrow_mut();
        let split = if let Some(x) = lines.last() {
            &self.source[x.hi as usize..]
        } else {
            &self.source
        };
        let base = self.source.as_ptr() as usize;
        split.lines().for_each(|s| {
            let line = s.trim();
            let low = line.as_ptr() as usize;
            let hi = low + line.len() - 1;
            lines.push(Span {
                low: u32::try_from(low - base).expect("source to big for span"),
                hi: u32::try_from(hi - base).expect("source to big for span"),
            })
        })
    }

    fn get_source_position(&self, span: Span) -> Pos {
        let lines = self.lines.borrow();
        let line = lines
            .binary_search_by(|f| {
                if span.low < f.low {
                    Ordering::Greater
                } else if span.low > f.hi {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            })
            .expect("span outside source file");
        let line_span = lines[line];
        let line_str = &self.source[line_span.low as usize..=line_span.hi as usize];
        let mut column = 0;
        let mut chars = line_str.chars();
        while ((chars.as_str().as_ptr() as usize - self.source.as_ptr() as usize) as u32) < span.low
        {
            column += 1;
            chars.next();
        }
        Pos {
            line: line as u32,
            column,
        }
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    /// Format a span as source line.
    ///
    /// # Example
    ///
    /// ```
    /// --> location/of/file:line:column
    /// ```
    pub fn format_span_line<W: Write>(&self, mut w: W, span: Span) -> fmt::Result {
        self.finish_lines();
        write!(w, " --> ")?;
        if let Some(x) = self.path.as_ref() {
            write!(w, "{}", x.display())?;
        } else {
            write!(w, "??")?;
        }
        let pos = self.get_source_position(span);
        writeln!(w, ":{}:{}", pos.line, pos.column)?;
        Ok(())
    }

    /// Formats a span into a block with its surrounding source
    /// as with rust errror messages.
    ///
    /// # Example
    ///
    /// ```
    ///     |
    /// num |  ...some_source_code().somewhere
    ///     |                        ^^^^^^^^^ optional message
    ///
    /// ```
    pub fn format_span_block<W: Write>(
        &self,
        mut w: W,
        span: Span,
        message: Option<&str>,
    ) -> fmt::Result {
        self.finish_lines();
        let lines = self.lines.borrow();
        let line = lines
            .binary_search_by(|f| {
                if span.low < f.low {
                    Ordering::Greater
                } else if span.low > f.hi {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            })
            .expect("span outside source file");

        // Format the span in its line.
        let line_span = lines[line];
        assert!(span.is_within(line_span));
        let mut prefix = self.source[line_span.low as usize..span.low as usize]
            .chars()
            .rev();
        let mut postfix = self.source[(span.hi + 1) as usize..=line_span.hi as usize].chars();

        let mut buffer = VecDeque::with_capacity(MAX_LINE_LENGTH);
        self.source[span.low as usize..=span.hi as usize]
            .chars()
            .for_each(|e| buffer.push_back(e));
        let span_len = buffer.len();

        let mut pre_len = 0;
        while buffer.len() < MAX_LINE_LENGTH {
            let mut pushed = false;
            if let Some(x) = prefix.next() {
                pre_len += 1;
                buffer.push_front(x);
                pushed = true
            }
            if buffer.len() >= MAX_LINE_LENGTH {
                break;
            }
            if let Some(x) = postfix.next() {
                buffer.push_back(x);
                pushed = true
            }
            if !pushed {
                break;
            }
        }

        // Add dots if the line continues past the max length
        if buffer.len() == MAX_LINE_LENGTH {
            if prefix.next().is_some() {
                buffer[0] = '.';
                buffer[1] = '.';
                buffer[2] = '.';
            }
            if postfix.next().is_some() {
                buffer[MAX_LINE_LENGTH - 1] = '.';
                buffer[MAX_LINE_LENGTH - 2] = '.';
                buffer[MAX_LINE_LENGTH - 3] = '.';
            }
        }

        // Write the formated spa
        // Plus one because lines start at 1
        let line_str = (line + 1).to_string();
        let num_len = line_str.chars().count();
        for _ in 0..=num_len {
            write!(w, " ")?;
        }
        writeln!(w, "|")?;
        write!(w, "{} | ", line_str)?;
        for c in buffer.iter().copied() {
            write!(w, "{}", c)?;
        }
        writeln!(w)?;
        for _ in 0..=num_len {
            write!(w, " ")?;
        }
        write!(w, "| ")?;
        for _ in 0..pre_len {
            write!(w, " ")?;
        }
        for _ in 0..span_len {
            write!(w, "^")?;
        }
        write!(w, " ")?;
        if let Some(x) = message {
            write!(w, "{}", x)?;
        }
        writeln!(w)?;
        Ok(())
    }
}
