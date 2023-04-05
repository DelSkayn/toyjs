//! Span's and span related accessories

//TODO: allocate pointer for spans which don't fit into 32 bit size/offset.

use std::ops::Range;

/// A span containing range information mostly used for tokens.
///
// Not marked as copy for future proving.
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Span {
    /// The offset into a code unit range.
    offset: u32,
    /// The size of the span from the offset.
    size: u32,
}

impl Span {
    pub fn new(offset: usize, size: usize) -> Span {
        let offset = offset.try_into().expect("offset did not fit into u32");
        let size = size.try_into().expect("size did not fit into u32");

        Span { offset, size }
    }

    pub fn from_range(range: Range<usize>) -> Span {
        Self::new(range.start, range.end - range.start)
    }

    pub fn offset(&self) -> usize {
        self.offset as usize
    }

    pub fn size(&self) -> usize {
        self.size as usize
    }

    /// Checks whether a span contains an other span.
    pub fn contains(&self, other: &Self) -> bool {
        self.offset <= other.offset
            && (self.offset as usize + self.size as usize)
                >= (other.offset as usize + other.size as usize)
    }

    /// Create a span that covers both spans..
    pub fn covers(&self, other: &Self) -> Self {
        if self.offset < other.offset {
            let diff = other.offset - self.offset;
            let size = (diff + other.size).max(self.size);
            Self {
                offset: self.offset,
                size,
            }
        } else {
            let diff = self.offset - other.offset;
            let size = (diff + self.size).max(other.size);
            Self {
                offset: other.offset,
                size,
            }
        }
    }

    /// Create a span that splits the current span into two after at.
    pub fn split(self, at: usize) -> (Self, Self) {
        assert!(at <= self.size as usize, "split outside of span size");
        (
            Span {
                offset: self.offset,
                size: (self.size as usize - at) as u32,
            },
            Span {
                offset: (self.offset as usize + at)
                    .try_into()
                    .expect("split offset did not fit into u32"),
                size: at as u32,
            },
        )
    }
}
