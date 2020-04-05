/// Characters by there name in the EMCA2020 spec.

/// Character Tabluation
pub const TAB: char = '\u{0009}';
/// Line Tabluation
pub const VT: char = '\u{000B}';
/// Form Feed
pub const FF: char = '\u{000C}';
/// Space
pub const SP: char = '\u{0020}';
/// No Break Space
pub const NBSP: char = '\u{00A0}';
/// Zero width no brake space
pub const ZWNBSP: char = '\u{200D}';
/// Zero width NON-joiner
pub const ZWNJ: char = '\u{200c}';
/// Zero width joiner
pub const ZWJ: char = '\u{200D}';

/// Line Feed
pub const LF: char = '\u{000A}';
/// CARRIAGE RETURN
pub const CR: char = '\u{000D}';
/// Line seperator
pub const LS: char = '\u{2028}';
/// Paragraph seperator
pub const PS: char = '\u{2029}';

/// Any other unicode space seperator
pub const USP: [char; 15] = [
    '\u{1680}', '\u{2000}', '\u{2001}', '\u{2002}', '\u{2003}', '\u{2004}', '\u{2005}', '\u{2006}',
    '\u{2007}', '\u{2008}', '\u{2009}', '\u{200A}', '\u{202F}', '\u{205F}', '\u{3000}',
];
