//! The ast datastructure used by the engine.
//!
//! For performance reasons the ast is structured as a flattenend collection of nodes.
//! Nodes are stored in a vector by types and are refered to by `NodeId` an index into the vector.
//! An index is a 32 bit number which is formatted as a non-zero integer. This allows rust to
//! optimize for example `Option<NodeId<T>>` to still only take 4 bytes of size.
//!
//! The tree uses the `AnyVec` and `AnyIndex` traits to store the different types of nodes.
//! The ast is defined with a vector for each type the ast might need to store.
//! If one tries to store a type which was not defined in the storage the implementation will
//! panic. Most of this is just fancy syntatic sugar. Each methods just compiles down to a load of
//! a field and then an operation on that field.
//!
//! A lot of nodes in the ast require lists so the ast supports a generic list type which can be
//! referred to by a list id. The different types of lists nodes are all stored in the same vector.
//!
//! Sometimes using the builtin list node is inefficient because, for example, the list can have
//! empty items. In this case you can use the `NodeList` type to define a list of nodes with a
//! different storage type.

use core::fmt;
use std::{
    any::Any,
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

use common::any_vec::AnyVec;

static TOO_MANY_NODES: &str = "Too many nodes in storage to fit 32 bit id.";

/// Generates the panic for when a type is not in the storage.
#[cold]
fn panic_no_storage<T>() -> ! {
    panic!(
        "Node of type `{}` not present in storage.",
        std::any::type_name::<T>()
    );
}

/// The node id for a specific type.
///
/// Internally represented by `NonZeroU32` so can be efficiently used with options.
#[repr(transparent)]
pub struct NodeId<T> {
    id: NonZeroU32,
    marker: PhantomData<T>,
}

impl<T> fmt::Debug for NodeId<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NodeId")
            .field("id", &self.id)
            .field("marker", &self.marker)
            .finish()
    }
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

impl<T> PartialOrd for NodeId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl<T> Ord for NodeId<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

/// An id of a `List<T>` node.
///
/// Internally represented by `NonZeroU32` so can be efficiently used with options.
#[repr(transparent)]
pub struct ListId<T> {
    id: NonZeroU32,
    marker: PhantomData<T>,
}

impl<T> fmt::Debug for ListId<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ListId")
            .field("id", &self.id)
            .field("marker", &self.marker)
            .finish()
    }
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

/// Basically `Option<ListId<T>>` but specifically meant to indicate wether a list is empty instead of
/// just not present. Use it when a list of nodes can be empty.
pub enum ListHead<T> {
    Empty,
    Present(ListId<T>),
}

impl<T> fmt::Debug for ListHead<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ListHead::Empty => f.debug_tuple("ListHead::Empty").finish(),
            ListHead::Present(ref x) => f.debug_tuple("ListHead::Present").field(x).finish(),
        }
    }
}

impl<T> ListHead<T> {
    pub fn is_empty(&self) -> bool {
        match *self {
            ListHead::Empty => true,
            ListHead::Present(_) => false,
        }
    }

    pub fn or(self, other: Self) -> Self {
        match self {
            Self::Present(x) => Self::Present(x),
            Self::Empty => other,
        }
    }
}

impl<T> From<Option<ListId<T>>> for ListHead<T> {
    fn from(value: Option<ListId<T>>) -> Self {
        match value {
            None => ListHead::Empty,
            Some(x) => ListHead::Present(x),
        }
    }
}

impl<T> From<ListHead<T>> for Option<ListId<T>> {
    fn from(value: ListHead<T>) -> Self {
        match value {
            ListHead::Empty => None,
            ListHead::Present(x) => Some(x),
        }
    }
}

impl<T> Eq for ListHead<T> {}
impl<T> PartialEq for ListHead<T> {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}
impl<T> Copy for ListHead<T> {}
impl<T> Clone for ListHead<T> {
    fn clone(&self) -> Self {
        *self
    }
}

/// A generic list node
pub struct List<T> {
    /// The id of the current item
    pub item: NodeId<T>,
    /// The id of the next item, if it exists.
    pub next: Option<ListId<T>>,
}

/// A generic list node for when the normal list node would be inefficient.
pub struct NodeList<T> {
    pub data: T,
    pub next: Option<NodeId<NodeList<T>>>,
}

/// Shorthand for `NodeId<NodeList<T>>`;
pub type NodeListId<T> = NodeId<NodeList<T>>;

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

/// The generic ast datastructure.
///
/// Can be indexed with the index syntax by both `NodeId` as well as `ListId`;
#[derive(Default)]
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

    /// Pushes a new node into the ast. Returns the id of the new node.
    #[inline]
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

    /// Pushes a new list node into the ast. Returns the id of the new list node.
    #[inline]
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

    /// Utility function for creating lists.
    ///
    /// Adds a new item to a list of nodes.
    /// `prev` should be the id of the previous list node if there is one.
    /// `item` is the id of the item the node should refer to.
    ///
    /// Returns the id of the newly create list node.
    ///
    /// # Usage
    /// ```rust
    /// # use toyjs_ast::{GenAst as Ast};
    /// # let generate_item = || {return 0u32};
    /// # let condition = false;
    /// let mut ast = Ast::new(Vec::<u32>::new());
    /// let mut prev = None;
    /// while condition{
    ///     let item: u32 = generate_item();
    ///     let item = ast.push_node(item);
    ///     prev = Some(ast.append_list(item,prev));
    /// }
    /// ```
    #[inline]
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

    /// Utility function for creating lists both for `NodeList` instead.
    ///
    /// Adds a new item to a list of nodes.
    /// `prev` should be id of the previous node list node if there is one.
    /// `item` is the item the node.
    ///
    /// Returns the id of the newly create list node.
    ///
    /// See `append_list` for usage.
    #[inline]
    pub fn append_node_list<N: Any>(
        &mut self,
        item: N,
        previous: Option<NodeId<NodeList<N>>>,
    ) -> NodeId<NodeList<N>> {
        let id = self.push_node(NodeList {
            data: item,
            next: None,
        });

        if let Some(x) = previous {
            self[x].next = Some(id);
        }

        id
    }

    pub fn free_node<N: Any>(&mut self, node: NodeId<N>) {
        let Some(len) = self.storage.any_len::<N>() else {
            panic_no_storage::<N>();
        };
        assert_eq!(
            len,
            u32::from(node.id).try_into().unwrap(),
            "A node can only be removed if it is the last item"
        );

        self.storage.any_pop::<N>();
    }

    /// Clears out all storage.
    ///
    /// Indexing with any an id after this call will probably panic or return an invalid value.
    pub fn clear(&mut self) {
        self.storage.all_clear();
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
