use crate::ssa::SsaId;
use std::ops::Deref;

type BlockRef<'alloc> = &'alloc Block<'alloc>;

pub struct Block<'alloc> {
    pub parents: BlockRef<'alloc>,
    pub first: SsaId,
    pub last: SsaId,
    pub next: BlockRef<'alloc>,
    pub jump: BlockRef<'alloc>,
}

pub struct FlowGraph<'alloc> {
    start: Block<'alloc>,
}
