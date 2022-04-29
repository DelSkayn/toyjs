use super::*;
use crate::new_cell_owner;

fn alloc_mut<'gc, 'cell>(arena: &'gc mut Arena<'_, 'cell>) -> Gc<'gc, 'cell, u32> {
    arena.add(0 as u32)
}

#[test]
fn allocate() {
    new_cell_owner!(owner);
    let roots = Roots::new();
    let arena = Arena::new(&roots);

    let a = arena.add(1i32);

    *a.borrow_mut_untraced(&mut owner) += 1;

    assert_eq!(*a.borrow(&owner), 2);
}

#[test]
fn collect() {
    new_cell_owner!(owner);
    let roots = Roots::new();
    let mut arena = Arena::new(&roots);

    let a = arena.add(1i32);

    *a.borrow_mut_untraced(&mut owner) += 1;

    assert_eq!(*a.borrow(&owner), 2);
    arena.collect_full(&owner);
}

#[test]
fn root() {
    new_cell_owner!(owner);
    let roots = Roots::new();
    let mut arena = Arena::new(&roots);

    let a = arena.add(1i32);
    root!(arena, a);
    let b = a;

    *b.borrow_mut_untraced(&mut owner) += 1;

    arena.collect_full(&owner);

    assert_eq!(*b.borrow(&owner), 2);
}

#[test]
fn rebind() {
    new_cell_owner!(owner);
    let roots = Roots::new();
    let mut arena = Arena::new(&roots);

    let a = alloc_mut(&mut arena);
    let a = rebind!(&arena, a);
    let b = arena.add(1u32);

    *a.borrow_mut_untraced(&mut owner) += 1;

    assert_eq!(*a.borrow(&owner), *b.borrow(&owner));
}

#[test]
#[should_panic]
fn rebind_test() {
    new_cell_owner!(owner);
    let roots = Roots::new();
    let arena = Arena::new(&roots);

    let roots2 = Roots::new();
    let arena2 = Arena::new(&roots2);

    let a = arena.add(0i32);
    let a = rebind!(&arena2, a);

    mem::drop(arena);
    mem::drop(roots);

    println!("{}", a.borrow(&owner));
}

#[test]
fn double_arena() {
    let roots = Roots::new();
    {
        let arena = Arena::new(&roots);
        arena.add(1);
    }
    {
        let arena = Arena::new(&roots);
        arena.add(1);
    }
}

struct Container<'gc, 'cell> {
    inner: Gc<'gc, 'cell, i32>,
}

unsafe impl<'gc, 'cell> Trace for Container<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, trace: Tracer<'a>) {
        trace.mark(self.inner);
    }
}

unsafe impl<'a, 'gc, 'cell> Rebind<'a> for Container<'gc, 'cell> {
    type Output = Container<'a, 'cell>;
}
