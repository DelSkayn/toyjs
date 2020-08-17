use crate::util::Integer;

shrinkwrap_index!(BlockId);

pub struct Block{
    start: SsaId,
    // end not included in range of instructions.
    end: SsaId,
    jump: BlockId,
}


impl Compiler{
    pub fn generate_basic_blocks(ssa: &Ssa) -> 
}
