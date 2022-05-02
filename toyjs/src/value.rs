use crate::Ctx;

#[derive(Debug, Clone, Copy)]
pub struct Value<'js> {
    pub(crate) value: vm::Value<'js, 'js>,
    pub(crate) ctx: Ctx<'js>,
}

impl<'js> Value<'js> {
    pub(crate) fn from_vm(ctx: Ctx<'js>, v: vm::Value<'_, 'js>) -> Self {
        let value = ctx.root_value(v);
        Value { value, ctx }
    }

    pub(crate) fn into_vm(self) -> vm::Value<'js, 'js> {
        self.value
    }
}
