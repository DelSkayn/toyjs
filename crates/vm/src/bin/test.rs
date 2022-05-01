use toyjs_vm as vm;
use vm::{
    atom::Atoms,
    gc::{Arena, Gc},
    Realm,
};

fn create_realm<'gc, 'rt, 'cell>(
    atoms: &Atoms,
    arena: &'gc Arena<'rt, 'cell>,
) -> Gc<'gc, 'cell, Realm<'gc, 'cell>> {
    let realm = vm::Realm::new(atoms, arena);
    arena.add(realm)
}

fn main() {
    vm::new_cell_owner!(owner);
    let root = vm::gc::Roots::new();
    let mut arena = vm::gc::Arena::new(&root);
    let atoms = Atoms::new();

    let realm = create_realm(&atoms, &arena);
    vm::root!(arena, realm);

    arena.collect_full(&owner);

    realm.global(&owner);
}
