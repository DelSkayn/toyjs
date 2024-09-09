use std::{cell::Cell, marker::PhantomData};

use common::ptr::{Mut, Raw, Ref};

pub struct ListNode {
    next: Cell<Option<Raw<u8>>>,
    prev: Cell<Option<Raw<u8>>>,
}

impl ListNode {
    pub fn new() -> Self {
        ListNode {
            next: Cell::new(None),
            prev: Cell::new(None),
        }
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

    pub unsafe fn front<'a>(&'a self) -> Option<Ref<'a, T>> {
        self.node.next.get().map(Raw::cast).map(Raw::into_ref)
    }

    pub unsafe fn back<'a>(&'a self) -> Option<Ref<'a, T>> {
        self.node.next.get().map(Raw::cast).map(Raw::into_ref)
    }

    pub unsafe fn front_mut<'a>(&'a mut self) -> Option<Mut<'a, T>> {
        self.node.next.get().map(Raw::cast).map(Raw::into_mut)
    }

    pub unsafe fn back_mut<'a>(&'a mut self) -> Option<Mut<'a, T>> {
        self.node.next.get().map(Raw::cast).map(Raw::into_mut)
    }

    pub unsafe fn push_front(&self, v: Raw<T>) {
        let v_node = v.cast::<u8>().add(T::OFFSET).cast::<ListNode>();

        let old = self.node.next.replace(Some(v.cast()));
        v_node.as_ref().next.set(old.clone());
        if let Some(old) = old {
            old.add(T::OFFSET)
                .cast::<ListNode>()
                .as_ref()
                .prev
                .set(Some(v.cast()));
        }
    }

    pub unsafe fn push_back(&self, v: Raw<T>) {
        let v_node = v.cast::<u8>().add(T::OFFSET).cast::<ListNode>();

        let old = self.node.prev.replace(Some(v.cast()));
        v_node.as_ref().prev.set(old);
        if let Some(old) = old {
            old.cast::<u8>()
                .add(T::OFFSET)
                .cast::<ListNode>()
                .as_ref()
                .next
                .set(Some(v.cast()));
        }
    }

    pub unsafe fn pop_front(&self) -> Option<Raw<T>> {
        let res = self.node.next.take()?;
        let res_node = res.add(T::OFFSET).cast::<ListNode>();

        let next = res_node.as_ref().next.get();

        self.node.next.set(next);
        if let Some(next) = next {
            next.cast::<u8>()
                .add(T::OFFSET)
                .cast::<ListNode>()
                .as_ref()
                .prev
                .set(None)
        }

        Some(res.cast())
    }

    pub unsafe fn pop_back(&self) -> Option<Raw<T>> {
        let res = self.node.prev.take()?;
        let res_node = res.add(T::OFFSET).cast::<ListNode>();

        let next = res_node.as_ref().prev.get();

        self.node.prev.set(next);
        if let Some(next) = next {
            next.cast::<u8>()
                .add(T::OFFSET)
                .cast::<ListNode>()
                .as_ref()
                .next
                .set(None)
        }

        Some(res.cast())
    }
}

pub unsafe trait IntrusiveList {
    const OFFSET: usize;

    unsafe fn detach(this: &Self)
    where
        Self: Sized,
    {
        let node = Ref::from(this)
            .into_raw()
            .cast::<u8>()
            .add(Self::OFFSET)
            .cast::<ListNode>();

        let next = node.as_ref().next.take();
        let prev = node.as_ref().prev.take();

        if let Some(next) = next {
            next.add(Self::OFFSET)
                .cast::<ListNode>()
                .as_ref()
                .prev
                .set(prev);
        }

        if let Some(prev) = prev {
            prev.add(Self::OFFSET)
                .cast::<ListNode>()
                .as_ref()
                .next
                .set(next);
        }
    }
}
