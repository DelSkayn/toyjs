#![recursion_limit = "512"]

use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

// This is the entry point for the web app
#[wasm_bindgen]
pub struct ToyJs {
    ctx: toyjs::Context,
}

#[wasm_bindgen]
impl ToyJs {
    pub fn new() -> Self {
        let realm = toyjs::ToyJs::new();
        let ctx = toyjs::Context::new(&realm);
        ToyJs { ctx }
    }

    pub fn eval(&self, source: String) -> String {
        self.ctx
            .with(|ctx| match ctx.eval::<toyjs::String, _>(source) {
                Ok(x) => format!("\x1b[1m{}\x1b[0m", x.as_str()),
                Err(e) => format!("\x1b[1;31m{}\x1b[0m", e),
            })
    }
}
