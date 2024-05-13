use std::{cell::Cell, marker::PhantomData, ptr::NonNull};

pub struct ListNode {
    next: Cell<Option<NonNull<u8>>>,
    prev: Cell<Option<NonNull<u8>>>,
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

    pub unsafe fn push_front(&self, v: NonNull<T>) {
        let v_node = v.cast::<u8>().add(T::OFFSET).cast::<ListNode>();

        let old = self.node.next.replace(Some(v.cast()));
        v_node.as_ref().next.set(old);
        if let Some(old) = old {
            old.add(T::OFFSET)
                .cast::<ListNode>()
                .as_ref()
                .prev
                .set(Some(v.cast()));
        }
    }

    pub unsafe fn push_back(&self, v: NonNull<T>) {
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

    pub unsafe fn pop_front(&self) -> Option<NonNull<T>> {
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

    pub unsafe fn pop_back(&self) -> Option<NonNull<T>> {
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
        let node = NonNull::from(this)
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
