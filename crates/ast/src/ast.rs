use std::{
    any::Any,
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

use common::any_vec::AnyVec;

static TOO_MANY_NODES: &str = "Too many nodes in storage to fit 32 bit id.";

#[cold]
fn panic_no_storage<T>() -> ! {
    panic!(
        "Node of type `{}` not present in storage.",
        std::any::type_name::<T>()
    );
}

#[repr(transparent)]
pub struct NodeId<T> {
    id: NonZeroU32,
    marker: PhantomData<T>,
}

impl<T> Eq for NodeId<T> {}
impl<T> PartialEq for NodeId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T> Copy for NodeId<T> {}
impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

#[repr(transparent)]
pub struct ListId<T> {
    id: NonZeroU32,
    marker: PhantomData<T>,
}

impl<T> Eq for ListId<T> {}
impl<T> PartialEq for ListId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T> Copy for ListId<T> {}
impl<T> Clone for ListId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub struct List<T> {
    pub item: NodeId<T>,
    pub next: Option<ListId<T>>,
}

impl<T> List<T> {
    fn cast<V>(self) -> List<V> {
        // SAFETY: NodeId and ListId are transparent wrappers over u32.
        // Changing the generic type does not change that.
        unsafe { std::mem::transmute(self) }
    }

    fn cast_borrow<V>(&self) -> &List<V> {
        // SAFETY: NodeId and ListId are transparent wrappers over u32.
        // Changing the generic type does not change that.
        unsafe { std::mem::transmute(self) }
    }

    fn cast_borrow_mut<V>(&mut self) -> &mut List<V> {
        // SAFETY
        unsafe { std::mem::transmute(self) }
    }
}

pub struct Ast<Storage: AnyVec> {
    storage: Storage,
    lists: Vec<List<()>>,
}

impl<S: AnyVec> Ast<S> {
    pub fn new(storage: S) -> Self {
        Ast {
            storage,
            lists: Vec::new(),
        }
    }

    pub fn push_node<N: Any>(&mut self, node: N) -> NodeId<N> {
        let Some(len) = self.storage.any_len::<N>() else {
            panic_no_storage::<N>()
        };

        let id = u32::try_from(len + 1).expect(TOO_MANY_NODES);
        // SAFETY: since 1 is added it id will never be zero.
        let id = unsafe { NonZeroU32::new_unchecked(id) };
        if self.storage.any_push(node).is_some() {
            panic_no_storage::<N>();
        }
        NodeId {
            id,
            marker: PhantomData,
        }
    }

    pub fn push_list<N: Any>(&mut self, node: List<N>) -> ListId<N> {
        let len = self.lists.len();

        let id = u32::try_from(len + 1).expect(TOO_MANY_NODES);
        // SAFETY: since 1 is added it id will never be zero.
        let id = unsafe { NonZeroU32::new_unchecked(id) };
        self.lists.push(node.cast());

        ListId {
            id,
            marker: PhantomData,
        }
    }

    pub fn append_list<N: Any>(
        &mut self,
        item: NodeId<N>,
        previous: Option<ListId<N>>,
    ) -> ListId<N> {
        let len = self.lists.len();

        let id = u32::try_from(len + 1).expect(TOO_MANY_NODES);
        // SAFETY: since 1 is added it id will never be zero.
        let id = unsafe { NonZeroU32::new_unchecked(id) };
        self.lists.push(List { item, next: None }.cast());

        let id = ListId {
            id,
            marker: PhantomData,
        };

        if let Some(x) = previous {
            self[x].next = Some(id);
        }

        id
    }
}

impl<R: AnyVec, N: Any> Index<NodeId<N>> for Ast<R> {
    type Output = N;

    #[inline]
    fn index(&self, index: NodeId<N>) -> &Self::Output {
        let id = usize::try_from(index.id.get()).unwrap() - 1;
        let Some(res) = self.storage.any_get(id) else {
            panic_no_storage::<N>()
        };
        res.expect("invalid node id")
    }
}

impl<R: AnyVec, N: Any> IndexMut<NodeId<N>> for Ast<R> {
    #[inline]
    fn index_mut(&mut self, index: NodeId<N>) -> &mut Self::Output {
        let id = usize::try_from(index.id.get()).unwrap() - 1;
        let Some(res) = self.storage.any_get_mut(id) else {
            panic_no_storage::<N>()
        };
        res.expect("invalid node id")
    }
}

impl<R: AnyVec, N: Any> Index<ListId<N>> for Ast<R> {
    type Output = List<N>;

    #[inline]
    fn index(&self, index: ListId<N>) -> &Self::Output {
        let id = usize::try_from(index.id.get()).unwrap() - 1;
        self.lists
            .get(id)
            .map(List::cast_borrow)
            .expect("invalid node id")
    }
}

impl<R: AnyVec, N: Any> IndexMut<ListId<N>> for Ast<R> {
    #[inline]
    fn index_mut(&mut self, index: ListId<N>) -> &mut Self::Output {
        let id = usize::try_from(index.id.get()).unwrap() - 1;
        self.lists
            .get_mut(id)
            .map(List::cast_borrow_mut)
            .expect("invalid node id")
    }
}
