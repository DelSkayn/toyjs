pub struct BitMap<const LEN: usize>([u64; LEN]);

impl<const LEN: usize> BitMap<LEN> {
    pub fn new() -> Self {
        BitMap([0; LEN])
    }

    pub fn set(&mut self, idx: usize) {
        let bit = idx % 64;
        let idx = idx / 64;
        self.0[idx] |= 1 << bit;
    }

    pub fn reset(&mut self, idx: usize) {
        let bit = idx % 64;
        let idx = idx / 64;
        self.0[idx] &= !(1 << bit);
    }

    pub fn get(&self, idx: usize) -> bool {
        let bit = idx % 64;
        let idx = idx / 64;
        (self.0[idx] | 1 << bit) != 0
    }
}
