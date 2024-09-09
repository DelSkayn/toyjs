pub trait BitValue: Copy {
    const BITS: usize;
    const ZERO: Self;

    fn get(&self, idx: u8) -> bool;

    fn set(&mut self, idx: u8);

    fn unset(&mut self, idx: u8);
}

impl BitValue for u32 {
    const BITS: usize = std::mem::size_of::<Self>() * 8;
    const ZERO: Self = 0;

    fn get(&self, idx: u8) -> bool {
        self & (1 << (idx as Self)) != 0
    }

    fn set(&mut self, idx: u8) {
        *self |= 1 << (idx as Self)
    }

    fn unset(&mut self, idx: u8) {
        *self &= !(1 << (idx as Self))
    }
}

impl BitValue for usize {
    const BITS: usize = std::mem::size_of::<Self>() * 8;
    const ZERO: Self = 0;

    fn get(&self, idx: u8) -> bool {
        self & (1 << (idx as Self)) != 0
    }

    fn set(&mut self, idx: u8) {
        *self |= 1 << (idx as Self)
    }

    fn unset(&mut self, idx: u8) {
        *self &= !(1 << (idx as Self))
    }
}

pub struct BitMap<T: BitValue, const SIZE: usize> {
    bits: [T; SIZE],
}

impl<T: BitValue, const SIZE: usize> BitMap<T, SIZE> {
    pub const fn new() -> Self {
        Self {
            bits: [T::ZERO; SIZE],
        }
    }

    pub const fn len() -> usize {
        SIZE * T::BITS
    }

    pub fn get(&self, idx: usize) -> bool {
        self.bits[idx / T::BITS].get((idx % T::BITS) as u8)
    }

    pub fn set(&mut self, idx: usize) {
        self.bits[idx / T::BITS].set((idx % T::BITS) as u8)
    }

    pub fn unset(&mut self, idx: usize) {
        self.bits[idx / T::BITS].unset((idx % T::BITS) as u8)
    }
}
