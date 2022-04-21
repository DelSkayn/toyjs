use toyjs_vm as vm;

fn main() {
    vm::new_cell_owner!(owner);
    let root = vm::gc2::Roots::new();
    let arena = vm::gc2::Arena::new(&root);

    let ptr = arena.add(1);
    *ptr.borrow_mut_untraced(&mut owner) += 1;
}
