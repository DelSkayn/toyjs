pub struct Source<'a> {
    /// Line offsets
    lines: Vec<usize>,
    finished: bool,
    src: &'a str,
    chars: Chars<'a>,
}

impl Source {}
