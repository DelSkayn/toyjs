//! Unicode related utilities.

/// Character constants
pub mod char {
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

    /// All characters which ecma considers whitespace.
    pub static WHITE_SPACE: [char; 20] = [
        '\u{0020}', TAB, FF, ZWNBSP, '\u{00A0}', '\u{1680}', '\u{2000}', '\u{2001}', '\u{2002}',
        '\u{2003}', '\u{2004}', '\u{2005}', '\u{2006}', '\u{2007}', '\u{2008}', '\u{2009}',
        '\u{200A}', '\u{202F}', '\u{205F}', '\u{3000}',
    ];

    /// All characters which ecma considers line terminators.
    pub static LINE_TERMINATOR: [char; 4] = [LF, CR, LS, PS];
}

pub trait Utf16Ext {
    fn is_utf16_surrogate(&self) -> bool;

    fn is_utf16_extend(&self, other: Self) -> u32;

    fn is_ascii(&self) -> bool;
}

impl Utf16Ext for u16 {
    #[inline]
    fn is_utf16_surrogate(&self) -> bool {
        matches!(self, 0xD800..=0xDFFF)
    }

    #[inline]
    fn is_utf16_extend(&self, other: Self) -> u32 {
        (((self & 0x3ff) as u32) << 10 | (other & 0x3ff) as u32) + 0x1_0000
    }

    #[inline]
    fn is_ascii(&self) -> bool {
        *self < 0x80
    }
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
        if (self as u32) < 0xFFFF {
            return (self as u16, None);
        }

        let first = (self as u32 - 0x10000u32) / 0x400 + 0xD800;
        let second = (self as u32 - 0x10000u32) % 0x400 + 0xD800;

        (first as u16, Some(second as u16))
    }

    #[inline]
    fn is_ecma_whitespace(&self) -> bool {
        for c in char::WHITE_SPACE.iter().copied() {
            if c == *self {
                return true;
            }
        }
        false
    }

    #[inline]
    fn is_ecma_line_terminator(&self) -> bool {
        for c in char::LINE_TERMINATOR.iter().copied() {
            if c == *self {
                return true;
            }
        }
        false
    }
}
