use core::fmt;
use std::{
    any::Any,
    cell::Cell,
    io::{Result, Write},
};

use common::{interner::Interner, string::String};
use token::{Number, NumberId, StringId};

use crate::{ast::NodeList, Ast, ListId, NodeId};

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

        self.indent.set(self.indent.get() + 1);

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
        self.ctx.indent.set(self.ctx.indent.get() - 1);
    }
}

pub trait RenderAst {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()>;
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
            x.render(ctx, w)?
        }
        Ok(())
    }
}

impl<T: RenderAst + Any> RenderAst for ListId<T> {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        let mut cur = *self;
        loop {
            writeln!(w,)?;
            ctx.indent(w)?;
            write!(w, "- ",)?;

            let list = &ctx.tree[cur];
            ctx.tree[list.item].render(ctx, w)?;
            if let Some(x) = list.next {
                cur = x;
            } else {
                return Ok(());
            }
        }
    }
}

impl<T: RenderAst + Any> RenderAst for NodeId<NodeList<T>> {
    fn render<W: Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        let mut cur = *self;
        loop {
            writeln!(w,)?;
            ctx.indent(w)?;
            write!(w, "- ",)?;

            let list = &ctx.tree[cur];
            list.data.render(ctx, w)?;
            if let Some(x) = list.next {
                cur = x;
            } else {
                return Ok(());
            }
        }
    }
}
