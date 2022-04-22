use toyjs_vm as vm;

fn main() {
    vm::new_cell_owner!(owner);
    let root = vm::gc::Roots::new();
    let mut arena = vm::gc::Arena::new(&root);

    let realm = arena.add(vm::Realm::new(&arena));
    vm::root!(&arena, realm);

    arena.collect_full(&owner);

    let global = realm.global(&owner);
    std::mem::drop(global);
}
