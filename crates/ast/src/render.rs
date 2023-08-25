use core::fmt::{self, Write};
use std::{any::Any, cell::Cell, result::Result as StdResult};

use common::{
    interner::Interner,
    number::{Number, NumberId},
    string::{String, StringId},
};

use crate::{
    ast::{ListHead, NodeList},
    Ast, ListId, NodeId,
};

pub type Result<T> = StdResult<T, fmt::Error>;

pub struct Display<'a, R: RenderAst> {
    r: R,
    ctx: RenderCtx<'a>,
}

impl<'a, R: RenderAst> fmt::Display for Display<'a, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.r.render(&self.ctx, f).map_err(|_| fmt::Error)
    }
}

pub struct RenderCtx<'a> {
    pub tree: &'a Ast,
    pub strings: &'a Interner<String, StringId>,
    pub numbers: &'a Interner<Number, NumberId>,
    pub indent: Cell<usize>,
}

impl<'a> RenderCtx<'a> {
    pub fn new(
        tree: &'a Ast,
        strings: &'a Interner<String, StringId>,
        numbers: &'a Interner<Number, NumberId>,
    ) -> Self {
        RenderCtx {
            tree,
            strings,
            numbers,
            indent: Cell::new(0),
        }
    }

    pub fn push_indent(&self) {
        self.indent.set(self.indent.get() + 1);
    }

    pub fn pop_indent(&self) {
        self.indent.set(self.indent.get() - 1);
    }

    pub fn indent<W: Write>(&self, w: &mut W) -> Result<()> {
        for _ in 0..self.indent.get() {
            write!(w, "  ")?
        }
        Ok(())
    }

    pub fn render_struct<'b, W: Write>(
        &'b self,
        name: &str,
        w: &'b mut W,
    ) -> Result<StructRender<'b, W>> {
        writeln!(w, "{}:", name)?;

        self.push_indent();

        Ok(StructRender {
            ctx: self,
            write: w,
        })
    }
}

pub struct StructRender<'a, W> {
    ctx: &'a RenderCtx<'a>,
    write: &'a mut W,
}

impl<'a, W: Write> StructRender<'a, W> {
    pub fn field<R: RenderAst>(self, name: &str, field: &R) -> Result<Self> {
        self.ctx.indent(self.write)?;
        write!(self.write, "> {} = ", name)?;
        field.render(self.ctx, self.write)?;
        Ok(self)
    }

    pub fn field_debug<R: fmt::Debug>(self, name: &str, field: &R) -> Result<Self> {
        self.ctx.indent(self.write)?;
        writeln!(self.write, "> {} = {:?}", name, field)?;
        Ok(self)
    }

    pub fn finish(self) {
        self.ctx.pop_indent();
    }
}

pub trait RenderAst: Sized {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()>;

    fn display<'a>(&'a self, ctx: RenderCtx<'a>) -> Display<'a, &'a Self> {
        Display { r: self, ctx }
    }
}

impl<R: RenderAst> RenderAst for &R {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        (*self).render(ctx, w)
    }
}

impl RenderAst for StringId {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        writeln!(w, "\"{}\"", ctx.strings[*self])
    }
}

impl RenderAst for NumberId {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        writeln!(w, "{}", ctx.numbers[*self].0)
    }
}

impl<T: RenderAst + Any> RenderAst for NodeId<T> {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.tree[*self].render(ctx, w)
    }
}

impl<T: RenderAst + Any> RenderAst for Option<T> {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        if let Some(ref x) = *self {
            write!(w, "+")?;
            x.render(ctx, w)?
        } else {
            writeln!(w, "-")?;
        }
        Ok(())
    }
}

impl<T: RenderAst + Any> RenderAst for ListHead<T> {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            ListHead::Empty => {
                writeln!(w, "empty")
            }
            ListHead::Present(x) => x.render(ctx, w),
        }
    }
}

impl<T: RenderAst + Any> RenderAst for ListId<T> {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        let mut cur = *self;
        ctx.push_indent();
        loop {
            writeln!(w,)?;
            ctx.indent(w)?;
            write!(w, "- ",)?;

            let list = &ctx.tree[cur];
            ctx.tree[list.item].render(ctx, w)?;
            if let Some(x) = list.next {
                cur = x;
            } else {
                ctx.pop_indent();
                return Ok(());
            }
        }
    }
}

impl<T: RenderAst + Any> RenderAst for NodeId<NodeList<T>> {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        let mut cur = *self;
        ctx.push_indent();
        loop {
            writeln!(w,)?;
            ctx.indent(w)?;

            write!(w, "- ",)?;

            let list = &ctx.tree[cur];
            list.data.render(ctx, w)?;
            if let Some(x) = list.next {
                cur = x;
            } else {
                ctx.pop_indent();
                return Ok(());
            }
        }
    }
}
