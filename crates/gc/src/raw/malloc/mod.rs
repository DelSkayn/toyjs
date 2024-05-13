use std::ptr::NonNull;

use common::hashmap::HashSet;

pub struct Arena {
    old: HashSet<NonNull<()>>,
    new: HashSet<NonNull<()>>,
}
