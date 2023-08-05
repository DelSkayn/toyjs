//! Unicode related utilities.

/// Character constants
pub mod chars {
    /// Zero width Non-joiner
    pub const ZWNJ: char = '\u{200C}';
    /// Zero width Joiner
    pub const ZWJ: char = '\u{200D}';
    /// Zero width no-break space
    pub const ZWNBSP: char = '\u{FEFF}';
    /// Character tabulation
    pub const TAB: char = '\u{0009}';
    /// Line tabulation
    pub const VT: char = '\u{000B}';
    /// Form feed
    pub const FF: char = '\u{000C}';

    /// Line feed
    pub const LF: char = '\u{000A}';
    /// Carriage return
    pub const CR: char = '\u{000D}';
    /// Line separator
    pub const LS: char = '\u{2020}';
    /// Paragraph separator
    pub const PS: char = '\u{2029}';

    /// A const array of all characters which ecma considers whitespace.
    pub const WHITE_SPACE_CONST: [char; 20] = [
        '\u{0020}', TAB, FF, ZWNBSP, '\u{00A0}', '\u{1680}', '\u{2000}', '\u{2001}', '\u{2002}',
        '\u{2003}', '\u{2004}', '\u{2005}', '\u{2006}', '\u{2007}', '\u{2008}', '\u{2009}',
        '\u{200A}', '\u{202F}', '\u{205F}', '\u{3000}',
    ];
    /// A static array of all characters which ecma considers whitespace.
    pub static WHITE_SPACE: [char; 20] = WHITE_SPACE_CONST;

    /// A const array of all characters which ecma considers line terminators.
    pub const LINE_TERMINATOR_CONST: [char; 4] = [LF, CR, LS, PS];
    /// A static array of all characters which ecma considers line terminators.
    pub static LINE_TERMINATOR: [char; 4] = LINE_TERMINATOR_CONST;
}

pub mod units {
    /// Zero width Non-joiner
    pub const ZWNJ: u16 = 0x200C;
    /// Zero width Joiner
    pub const ZWJ: u16 = 0x200D;
    /// Zero width no-break space
    pub const ZWNBSP: u16 = 0xFEFF;
    /// character tabulation
    pub const TAB: u16 = 0x0009;
    /// Line tabulation
    pub const VT: u16 = 0x000B;
    /// Form feed
    pub const FF: u16 = 0x000C;

    /// Line feed
    pub const LF: u16 = 0x000A;
    /// Carriage return
    pub const CR: u16 = 0x000D;
    /// Line separator
    pub const LS: u16 = 0x2020;
    /// Paragraph separator
    pub const PS: u16 = 0x2029;

    /// A const array of all characters which ecma considers whitespace.
    pub const WHITE_SPACE_CONST: [u16; 20] = [
        0x0020, TAB, FF, ZWNBSP, 0x00A0, 0x1680, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005,
        0x2006, 0x2007, 0x2008, 0x2009, 0x200A, 0x202F, 0x205F, 0x3000,
    ];
    /// A static array of all u16acters which ecma considers whitespace.
    pub static WHITE_SPACE: [u16; 20] = WHITE_SPACE_CONST;

    /// A const array of all u16acters which ecma considers line terminators.
    pub const LINE_TERMINATOR_CONST: [u16; 4] = [LF, CR, LS, PS];
    /// A static array of all u16acters which ecma considers line terminators.
    pub static LINE_TERMINATOR: [u16; 4] = LINE_TERMINATOR_CONST;
}

pub mod byte {
    /// Zero width Non-joiner
    /// Character tabulation
    pub const TAB: u8 = b'\t';
    /// Line tabulation
    pub const VT: u8 = 0xB;
    /// Form feed
    pub const FF: u8 = 0xC;

    /// Line feed
    pub const LF: u8 = 0xA;
    /// Carriage return
    pub const CR: u8 = 0xD;

    /// Space
    pub const SP: u8 = 0x20;
    /// No Break Space
    pub const NBSP: u8 = 0xA0;
}

pub trait Utf16Ext {
    fn is_utf16_surrogate(&self) -> bool;

    fn is_utf16_leading_surrogate(&self) -> bool;

    fn is_utf16_trailing_surrogate(&self) -> bool;

    fn utf16_extend(&self, other: Self) -> u32;

    fn decode_utf16_with<F>(self, with: F) -> char
    where
        F: FnOnce() -> u16;

    fn is_ascii(&self) -> bool;
}

impl Utf16Ext for u16 {
    #[inline]
    fn is_utf16_surrogate(&self) -> bool {
        matches!(self, 0xD800..=0xDFFF)
    }

    #[inline]
    fn is_utf16_leading_surrogate(&self) -> bool {
        matches!(self, 0xD800..=0xDBFF)
    }

    #[inline]
    fn is_utf16_trailing_surrogate(&self) -> bool {
        matches!(self, 0xDC00..=0xDFFF)
    }

    #[inline]
    fn utf16_extend(&self, other: Self) -> u32 {
        (((self & 0x3ff) as u32) << 10 | (other & 0x3ff) as u32) + 0x1_0000
    }

    #[inline]
    fn decode_utf16_with<F>(self, with: F) -> char
    where
        F: FnOnce() -> u16,
    {
        if self.is_utf16_leading_surrogate() {
            unsafe { char::from_u32_unchecked(self.utf16_extend(with())) }
        } else {
            unsafe { char::from_u32_unchecked(self as u32) }
        }
    }

    #[inline]
    fn is_ascii(&self) -> bool {
        *self < 0x80
    }
}

pub fn byte_is_continue(v: u8) -> bool {
    // Bitmap containing with 1's for the bit representing a character which javascript
    // considers to be an ident continuing character.
    v < 128 && (1 << v) & 0x7fffffe87fffffe03ff001000000000u128 != 0
}

pub fn byte_is_start(v: u8) -> bool {
    (1 << v) & 0x7fffffe87fffffe0000001000000000u128 != 0
}

pub trait CharExt {
    fn is_xid_continue(&self) -> bool;

    fn is_xid_start(&self) -> bool;

    fn encode_utf16_code_point(self) -> (u16, Option<u16>);

    fn is_ecma_whitespace(&self) -> bool;

    fn is_ecma_line_terminator(&self) -> bool;
}

impl CharExt for char {
    #[inline(always)]
    fn is_xid_continue(&self) -> bool {
        unicode_ident::is_xid_continue(*self)
    }

    #[inline(always)]
    fn is_xid_start(&self) -> bool {
        unicode_ident::is_xid_start(*self)
    }

    #[inline]
    fn encode_utf16_code_point(self) -> (u16, Option<u16>) {
        if (self as u32) <= 0xFFFF {
            return (self as u16, None);
        }

        let first = (self as u32 - 0x10000u32) / 0x400 + 0xD800;
        let second = (self as u32 - 0x10000u32) % 0x400 + 0xD800;

        (first as u16, Some(second as u16))
    }

    #[inline]
    fn is_ecma_whitespace(&self) -> bool {
        for c in chars::WHITE_SPACE.iter().copied() {
            if c == *self {
                return true;
            }
        }
        false
    }

    #[inline]
    fn is_ecma_line_terminator(&self) -> bool {
        for c in chars::LINE_TERMINATOR.iter().copied() {
            if c == *self {
                return true;
            }
        }
        false
    }
}
