use crate::variables::{ScopeId, Symbol, SymbolId, SymbolUseOrder, Variables};

pub struct LoopPoint {
    offset: u32,
}

pub struct LoopStack {
    loop_depth: usize,
    stack: Vec<SymbolId>,
}

impl LoopStack {
    pub fn new() -> Self {
        LoopStack {
            loop_depth: 0,
            stack: Vec::new(),
        }
    }

    pub fn mark_point(&mut self) -> LoopPoint {
        self.loop_depth += 1;
        LoopPoint {
            offset: self.stack.len() as u32,
        }
    }

    pub fn resolve_point(
        &mut self,
        variables: &mut Variables,
        to: SymbolUseOrder,
        at: LoopPoint,
        scope: ScopeId,
    ) {
        self.loop_depth -= 1;
        for i in (at.offset..self.stack.len() as u32).rev() {
            let sym: &mut Symbol = &mut variables.symbols[self.stack[i as usize]];
            if sym.scope < scope {
                sym.last_use = Some(to);
            } else {
                self.stack.swap_remove(i as usize);
            }
        }
    }

    pub fn use_symbol(&mut self, id: SymbolId) {
        if !self.loop_depth != 0 {
            return;
        }
        dbg!(id);
        self.stack.push(id);
    }
}
