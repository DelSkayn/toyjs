//! Unicode related utilities.

pub trait Utf16Ext {
    fn utf16_surrogate(self) -> bool;

    fn utf16_extend(self, other: Self) -> u32;
}

impl Utf16Ext for u16 {
    fn utf16_surrogate(self) -> bool {
        matches!(self, 0xD800..=0xDFFF)
    }

    fn utf16_extend(self, other: Self) -> u32 {
        (((self & 0x3ff) as u32) << 10 | (other & 0x3ff) as u32) + 0x1_0000
    }
}
