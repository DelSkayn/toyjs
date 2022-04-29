use toyjs_vm as vm;
use vm::{
    gc::{Arena, Gc},
    Realm,
};

fn create_realm<'gc, 'rt, 'cell>(
    arena: &'gc Arena<'rt, 'cell>,
) -> Gc<'gc, 'cell, Realm<'gc, 'cell>> {
    let realm = vm::Realm::new(arena);
    arena.add(realm)
}

fn main() {
    vm::new_cell_owner!(owner);
    let root = vm::gc::Roots::new();
    let mut arena = vm::gc::Arena::new(&root);

    let realm = create_realm(&arena);
    vm::root!(arena, realm);

    arena.collect_full(&owner);

    realm.global(&owner);
}
