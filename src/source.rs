/// A location of a collection of characters in a source as an offset in a slice of bytes
#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub begin: u32,
    pub end: u32,
}
