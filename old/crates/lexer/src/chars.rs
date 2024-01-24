/// Characters by there name in the EMCA2020 spec.

/// Character Tabluation
pub const HT: u8 = 0x9;
pub const TAB: u8 = 0x9;
/// Line Tabluation
pub const VT: u8 = 0xB;
/// Form Feed
pub const FF: u8 = 0xC;
/// Space
pub const SP: u8 = 0x20;
/// No Break Space
pub const NBSP: u8 = 0xA0;
/// Zero width no brake space
pub const ZWNBSP: char = '\u{feff}';
/// Zero width NON-joiner
pub const ZWNJ: char = '\u{200c}';
/// Zero width joiner
pub const ZWJ: char = '\u{200D}';

/// Line Feed
pub const LF: u8 = 0x0a;
/// CARRIAGE RETURN
pub const CR: u8 = 0x0d;
/// Line seperator
pub const LS: char = '\u{2028}';
/// Paragraph seperator
pub const PS: char = '\u{2029}';
/// Backspace
pub const BS: char = '\u{0008}';

pub const USP_1: char = '\u{1680}';
pub const USP_2: char = '\u{2000}';
pub const USP_3: char = '\u{2001}';
pub const USP_4: char = '\u{2002}';
pub const USP_5: char = '\u{2003}';
pub const USP_6: char = '\u{2004}';
pub const USP_7: char = '\u{2005}';
pub const USP_8: char = '\u{2006}';
pub const USP_9: char = '\u{2007}';
pub const USP_10: char = '\u{2008}';
pub const USP_11: char = '\u{2009}';
pub const USP_12: char = '\u{200A}';
pub const USP_13: char = '\u{202F}';
pub const USP_14: char = '\u{205F}';
pub const USP_15: char = '\u{3000}';
