use toyjs_vm as vm;

fn main() {
    vm::new_cell_owner!(owner);
    let root = vm::gc::RootSet::new();
    let arena = vm::gc::Arena::new(&root);

    let ptr = arena.allocate(1);
    *ptr.borrow_mut_untraced(&mut owner) += 1;
}
