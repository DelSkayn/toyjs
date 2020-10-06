pub struct BitMap(Vec<u8>);

impl BitMap {
    pub fn new() -> Self {
        BitMap(Vec::new())
    }

    pub fn with_capacity(bits: usize) -> Self {
        BitMap(Vec::with_capacity(bits >> 3))
    }

    pub fn insert(&mut self, idx: usize) -> bool {
        let byte = idx >> 3;
        for _ in byte..self.0.len() {
            self.0.push(0);
        }
        let bit = idx & 0b1111;
        self.0[byte] |= 1 << bit;
        self.0[byte] & 1 << bit != 0
    }

    pub fn get(&self, idx: usize) -> bool {
        let byte = idx >> 3;
        if byte >= self.0.len() {
            return false;
        }
        let bit = idx & 0b1111;
        self.0[byte] & 1 << bit != 0
    }
}
