use std::ops::Index as IndexTrait;

#[cfg(not(feature = "long_index"))]
pub type Integer = u32;

#[cfg(feature = "long_index")]
pub type Integer = u64;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Index(pub(crate) Integer);

impl Index {
    pub const fn invalid() -> Self {
        Index(Integer::MAX)
    }

    pub fn into_usize(self) -> usize {
        if self == Self::invalid() {
            panic!("could not convert invalid index to usize")
        }
        self.0 as usize
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
