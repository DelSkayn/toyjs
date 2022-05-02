use toyjs_vm as vm;
use vm::{
    atom::Atoms,
    cell::CellOwner,
    gc::{Arena, Gc},
    Realm,
};

fn create_realm<'gc, 'rt, 'cell>(
    owner: &mut CellOwner<'cell>,
    arena: &'gc Arena<'rt, 'cell>,
    atoms: &Atoms,
) -> Gc<'gc, 'cell, Realm<'gc, 'cell>> {
    let realm = vm::Realm::new(owner, arena, atoms);
    arena.add(realm)
}

fn main() {
    vm::new_cell_owner!(owner);
    let root = vm::gc::Roots::new();
    let mut arena = vm::gc::Arena::new(&root);
    let atoms = Atoms::new();

    let realm = create_realm(&mut owner, &arena, &atoms);
    vm::root!(arena, realm);

    arena.collect_full(&mut owner);

    realm.global(&owner);
}
