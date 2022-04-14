/*
#![recursion_limit = "512"]

use std::panic::{self, UnwindSafe};

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

impl Default for ToyJs {
    fn default() -> Self {
        ToyJs::new()
    }
}

pub struct UnwindContext<'a>(&'a toyjs::Context);

impl UnwindSafe for UnwindContext<'_> {}

#[wasm_bindgen]
impl ToyJs {
    pub fn new() -> Self {
        let realm = toyjs::ToyJs::new();
        let ctx = toyjs::Context::new(&realm);
        ToyJs { ctx }
    }

    pub fn eval(&mut self, source: String) -> String {
        let ctx = UnwindContext(&self.ctx);
        let p = panic::catch_unwind(move || {
            ctx.0
                .with(|ctx| match ctx.eval::<toyjs::String, _>(source) {
                    Ok(x) => format!("\x1b[1m{}\x1b[0m", x.as_str()),
                    Err(e) => format!("\x1b[1;31m{}\x1b[0m", e),
                })
        });
        match p {
            Ok(x) => x,
            Err(e) => {
                let realm = toyjs::ToyJs::new();
                let ctx = toyjs::Context::new(&realm);
                self.ctx = ctx;
                if let Some(e) = e.downcast_ref::<String>() {
                    format!("engine panicked, restarted engine {}", e)
                } else if let Some(e) = e.downcast_ref::<&str>() {
                    format!("engine panicked, restarted engine {}", e)
                } else {
                    "engine panicked, restarted engine".to_owned()
                }
            }
        }
    }
}
*/
