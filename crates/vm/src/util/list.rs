use std::{cell::Cell, marker::PhantomData, mem::offset_of, ops::Deref, pin::Pin};

use common::ptr::{Raw, Ref};

pub struct ListNode {
    // pointers point to the root of pointee.
    // To get the ListNode you need to add the IntrusiveList::OFFSET
    next: Cell<Option<Raw<ListNode>>>,
    prev: Cell<Option<Raw<ListNode>>>,
}

unsafe impl IntrusiveList for ListNode {
    const OFFSET: usize = 0;
}

impl ListNode {
    pub fn new() -> Self {
        ListNode {
            next: Cell::new(None),
            prev: Cell::new(None),
        }
    }

    pub fn is_detached(&self) -> bool {
        self.next.get().is_none()
    }

    pub fn detach(&self) {
        let next = self.next.take();
        let prev = self.prev.take();

        if let Some(next) = next {
            unsafe {
                next.as_borrow().prev.set(prev);
            }
        }

        if let Some(prev) = prev {
            unsafe {
                prev.as_borrow().next.set(next);
            }
        }
    }
}

impl Drop for ListNode {
    fn drop(&mut self) {
        self.detach()
    }
}

pub struct ListEntry<T> {
    node: ListNode,
    value: T,
}

impl<T> Deref for ListEntry<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> ListEntry<T> {
    pub fn new(value: T) -> Self {
        ListEntry {
            node: ListNode::new(),
            value,
        }
    }
}

unsafe impl<T> IntrusiveList for ListEntry<T> {
    const OFFSET: usize = offset_of!(Self, value);
}

pub struct Cursor<'a, T> {
    list: Pin<&'a List<T>>,
    cur: Raw<ListNode>,
}

impl<'a, T: IntrusiveList> Cursor<'a, T> {
    pub fn next(mut self) -> Self {
        unsafe {
            self.cur = self.cur.as_borrow().next.get().expect("Invalid list");
        }

        self
    }

    pub fn prev(mut self) -> Self {
        unsafe {
            self.cur = self.cur.as_borrow().prev.get().expect("Invalid list");
        }
        self
    }

    pub fn is_root(&self) -> bool {
        let root_ptr = Ref::from(&self.list.node).into_raw();
        self.cur == root_ptr
    }

    pub fn get_raw(&self) -> Option<Raw<T>> {
        if !self.is_root() {
            unsafe { Some(self.cur.cast::<u8>().sub(T::OFFSET).cast::<T>()) }
        } else {
            None
        }
    }

    pub fn get(&self) -> Option<&T> {
        unsafe { self.get_raw().map(|x| x.into_ref().into_borrow()) }
    }
}

impl<'a, T> Iterator for Cursor<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

pub struct List<T> {
    node: ListNode,
    _marker: PhantomData<T>,
}

impl<T: IntrusiveList> List<T> {
    pub fn new() -> Self {
        List {
            node: ListNode::new(),
            _marker: PhantomData,
        }
    }

    pub fn initialize(self: Pin<&Self>) {
        let this_node = Ref::from(&self.node).into_raw();

        self.node.next.set(Some(this_node));
        self.node.prev.set(Some(this_node));
    }

    /// Safety: None of the nodes currently in the last may be detached from the list while the cursor is
    /// targeting that specific node.
    pub unsafe fn cursor(self: Pin<&Self>) -> Cursor<T> {
        let this = self.get_ref();
        Cursor {
            list: self,
            cur: Ref::from(&this.node).into_raw(),
        }
    }

    /// Safety: None of the nodes currently in the last may be detached from the list while the cursor is
    /// targeting that specific node.
    pub unsafe fn cursor_next(self: Pin<&Self>) -> Cursor<T> {
        Cursor {
            list: self,
            cur: self.node.next.get().expect("list not initialized"),
        }
    }

    /// Safety: None of the nodes currently in the last may be detached from the list while the cursor is
    /// targeting that specific node.
    pub unsafe fn cursor_prev(self: Pin<&Self>) -> Cursor<T> {
        Cursor {
            list: self,
            cur: self.node.prev.get().expect("list not initialized"),
        }
    }

    pub fn pop_front(&self) -> Option<Raw<T>> {
        let this_node = Ref::from(&self.node).into_raw();

        let next = self.node.next.get().expect("list not initialized");

        if next == this_node {
            return None;
        }

        unsafe { next.as_borrow().detach() };

        let ptr = unsafe { next.cast::<u8>().sub_bytes(T::OFFSET).cast::<T>() };

        Some(ptr)
    }

    pub fn front(&self) -> Option<Raw<T>> {
        let this_node = Ref::from(&self.node).into_raw();

        let next = self.node.next.get().expect("list not initialized");

        if next == this_node {
            return None;
        }

        let ptr = unsafe { next.cast::<u8>().sub_bytes(T::OFFSET).cast::<T>() };

        Some(ptr)
    }

    pub fn pop_back(&self) -> Option<Raw<T>> {
        let this_node = Ref::from(&self.node).into_raw();

        let prev = self.node.prev.get().expect("list not initialized");

        if prev == this_node {
            return None;
        }

        unsafe { prev.as_borrow().detach() };

        let ptr = unsafe { prev.cast::<u8>().sub_bytes(T::OFFSET).cast::<T>() };

        Some(ptr)
    }

    pub fn back(&self) -> Option<Raw<T>> {
        let this_node = Ref::from(&self.node).into_raw();

        let next = self.node.prev.get().expect("list not initialized");

        if next == this_node {
            return None;
        }

        let ptr = unsafe { next.cast::<u8>().sub_bytes(T::OFFSET).cast::<T>() };

        Some(ptr)
    }

    pub fn push_front(&self, v: Pin<&T>) {
        unsafe {
            let v = v.get_ref();
            self.push_front_raw(Ref::from(v).into_raw());
        }
    }

    pub unsafe fn push_front_raw(&self, v: Raw<T>) {
        unsafe {
            let v_node = v.cast::<u8>().add(T::OFFSET).cast::<ListNode>();

            let this_node = Ref::from(&self.node).into_raw();

            let old = self
                .node
                .next
                .replace(Some(v_node))
                .expect("list not initialized");

            let detached = v_node.as_borrow().next.replace(Some(old)).is_none();
            assert!(detached, "list node added twice");
            v_node.as_borrow().prev.set(Some(this_node));

            old.as_borrow().prev.set(Some(v_node));
        }
    }

    pub fn push_back(&self, v: Pin<&T>) {
        unsafe {
            let v = v.get_ref();
            self.push_back_raw(Ref::from(v).into_raw());
        }
    }

    pub unsafe fn push_back_raw(&self, v: Raw<T>) {
        unsafe {
            let v_node = v.cast::<u8>().add(T::OFFSET).cast::<ListNode>();

            let this_node = Ref::from(&self.node).into_raw();

            let old = self
                .node
                .prev
                .replace(Some(v_node))
                .expect("list not initialized");

            let detached = v_node.as_borrow().prev.replace(Some(old)).is_none();
            assert!(detached, "list node added twice");
            v_node.as_borrow().next.set(Some(this_node));

            old.as_borrow().next.set(Some(v_node));
        }
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        self.node.detach()
    }
}

pub unsafe trait IntrusiveList {
    const OFFSET: usize;
}

#[cfg(test)]
mod test {
    use std::pin::{pin, Pin};

    use super::{List, ListEntry};

    fn collect_entries(list: Pin<&List<ListEntry<usize>>>) -> Vec<usize> {
        let mut cursor = unsafe { list.cursor_next() };
        let mut res = Vec::new();
        while let Some(x) = cursor.get() {
            res.push(**x);
            cursor = cursor.next();
        }
        res
    }

    #[test]
    fn stack_list() {
        let a = pin!(ListEntry::new(1));

        {
            let list = pin!(List::new());
            list.as_ref().initialize();

            let entries = collect_entries(list.as_ref());
            assert_eq!(entries.as_slice(), &[]);

            let b = pin!(ListEntry::new(2));
            list.push_front(b.as_ref());

            let entries = collect_entries(list.as_ref());
            assert_eq!(entries.as_slice(), &[2]);

            list.push_front(a.as_ref());

            let entries = collect_entries(list.as_ref());
            assert_eq!(entries.as_slice(), &[1, 2]);

            {
                let c = pin!(ListEntry::new(3));
                list.push_front(c.as_ref());

                let entries = collect_entries(list.as_ref());
                assert_eq!(entries.as_slice(), &[3, 1, 2]);

                let d = pin!(ListEntry::new(4));
                list.push_back(d.as_ref());

                let entries = collect_entries(list.as_ref());
                assert_eq!(entries.as_slice(), &[3, 1, 2, 4]);
            }

            let entries = collect_entries(list.as_ref());
            assert_eq!(entries.as_slice(), &[1, 2]);
        }
    }
}
