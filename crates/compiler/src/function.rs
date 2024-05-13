use std::collections::VecDeque;

use ast::NodeId;

pub struct PendingFunctionList {
    pending: VecDeque<NodeId<ast::Function>>,
    next_function_id: u32,
}

impl PendingFunctionList {
    pub fn new() -> Self {
        PendingFunctionList {
            pending: VecDeque::new(),
            next_function_id: 1,
        }
    }

    pub fn push(&mut self, f: NodeId<ast::Function>) -> bc::FunctionId {
        self.pending.push_back(f);
        let id = self.next_function_id;
        self.next_function_id += 1;
        bc::FunctionId(id)
    }

    pub fn pop_pending(&mut self) -> Option<NodeId<ast::Function>> {
        self.pending.pop_front()
    }
}

pub struct FunctionContext {
    instruction_offset: u32,
}
