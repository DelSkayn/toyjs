use std::ops::Index as IndexTrait;

#[cfg(not(feature = "usize_index"))]
pub type Integer = u32;

#[cfg(feature = "usize_index")]
pub type Integer = usize;

/// An index into a set of values which always has a range
/// Used with OptionIndex
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Index(pub Integer);

impl Index {
    pub fn into_usize(self) -> usize {
        self.0 as usize
    }
}

/// An index into a set of values which can optionally not have a value
/// Used instead of `Option<u32>` to save a few bytes, `Option<u32> is twice as
/// large as u32 being 8 bytes while OptionIndex is only 4.
/// As Optional integers are use frequently and stored in large numbers, so saving those 4 bytes can lead
/// to a large memory saving.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct OptionIndex(Integer);

impl OptionIndex {
    pub const fn none() -> OptionIndex {
        OptionIndex(Integer::MAX)
    }

    pub fn some(i: Index) -> OptionIndex {
        assert!(
            i.0 < Integer::MAX,
            "integer value to large to fit in option index"
        );
        OptionIndex(i.0)
    }

    pub fn is_some(&self) -> bool {
        *self != OptionIndex::none()
    }

    pub fn is_none(&self) -> bool {
        *self == OptionIndex::none()
    }

    pub fn unwrap(self) -> Index {
        if self == OptionIndex::none() {
            panic!("tried to unwrap OptionIndex with value none");
        }
        Index(self.0)
    }

    pub fn unwrap_or(self, v: Index) -> Index {
        if self == OptionIndex::none() {
            v
        } else {
            Index(self.0)
        }
    }

    pub fn into_option(self) -> Option<Index> {
        if self == Self::none() {
            None
        } else {
            Some(Index(self.0))
        }
    }
}

impl From<Index> for Integer {
    fn from(v: Index) -> Self {
        v.0
    }
}

impl From<Index> for usize {
    fn from(v: Index) -> Self {
        v.0 as usize
    }
}

impl From<u8> for Index {
    fn from(v: u8) -> Self {
        Index(v as Integer)
    }
}

impl From<u16> for Index {
    fn from(v: u16) -> Self {
        Index(v as Integer)
    }
}

impl From<u32> for Index {
    fn from(v: u32) -> Self {
        assert!(v < Integer::MAX as u32, "index could not contain value!");
        Index(v as Integer)
    }
}

impl From<u64> for Index {
    fn from(v: u64) -> Self {
        assert!(v < Integer::MAX as u64, "index could not contain value!");
        Index(v as Integer)
    }
}

impl From<usize> for Index {
    fn from(v: usize) -> Self {
        assert!(v < Integer::MAX as usize, "index could not contain value!");
        Index(v as Integer)
    }
}
