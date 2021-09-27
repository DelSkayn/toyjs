//! Code related to generating variable placement.

pub enum Placement {
    Global,
    Local { register: u8 },
    Captured { slot: u16, depth: u8 },
}
