use std::fmt;

pub struct Render<F> {
    f: F,
}

impl<F> fmt::Display for Render<F>
where
    F: Fn(&mut fmt::Formatter) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.f)(f)
    }
}

pub fn render<F>(f: F) -> Render<F>
where
    F: Fn(&mut fmt::Formatter) -> fmt::Result,
{
    Render { f }
}

pub struct IndentFormatter<W> {
    line_buffer: String,
    w: W,
    depth: usize,
    indent_size: usize,
}

impl<W> IndentFormatter<W>
where
    W: fmt::Write,
{
    pub fn new(writer: W, indent_size: usize) -> Self {
        IndentFormatter {
            line_buffer: String::new(),
            w: writer,
            depth: 0,
            indent_size,
        }
    }

    pub fn finish(&mut self) -> fmt::Result {
        self.w.write_str(&self.line_buffer)?;
        self.line_buffer.clear();
        Ok(())
    }

    pub fn increase_depth(&mut self) {
        self.depth += 1;
    }

    pub fn decrease_depth(&mut self) {
        self.depth += 1;
    }

    pub fn indent<F>(&mut self, f: F) -> fmt::Result
    where
        F: FnOnce(&mut IndentFormatter<W>) -> fmt::Result,
    {
        self.depth += 1;
        let res = f(self);
        self.depth -= 1;
        res
    }

    pub fn write_str(&mut self, str: &str) -> fmt::Result {
        let mut lines = str.split("\n");

        if let Some(x) = lines.next() {
            self.line_buffer.push_str(x);
        }

        for l in lines {
            for _ in 0..(self.indent_size * self.depth) {
                self.w.write_char(' ')?;
            }
            self.w.write_str(&self.line_buffer)?;
            self.w.write_char('\n')?;
            self.line_buffer.clear();
            self.line_buffer.push_str(l);
        }

        Ok(())
    }
}

impl<W> fmt::Write for IndentFormatter<W>
where
    W: fmt::Write,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.write_str(s)
    }
}
