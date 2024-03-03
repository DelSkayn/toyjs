use std::ops::BitOr;

#[repr(u8)]
pub enum PtrFlag {
    Heap = 1 << (8 - 2),
    Utf16 = 1 << (8 - 1),
}

pub struct PtrFlags(u8);

impl PtrFlags {
    pub fn none() -> Self {
        Self(0)
    }
}

impl From<PtrFlag> for PtrFlags {
    fn from(value: PtrFlag) -> Self {
        Self(value as u8)
    }
}

impl BitOr<PtrFlag> for PtrFlag {
    type Output = PtrFlags;

    fn bitor(self, rhs: PtrFlag) -> Self::Output {
        PtrFlags::from(self) | PtrFlags::from(rhs)
    }
}

impl BitOr<PtrFlags> for PtrFlags {
    type Output = PtrFlags;

    fn bitor(self, rhs: PtrFlags) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}
