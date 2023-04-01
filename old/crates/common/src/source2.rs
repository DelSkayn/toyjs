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
    lines: Vec<Span>,
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

    pub fn source(&self) -> &str {
        &self.source
    }
}
