use std::{any::Any, hash::Hash};

use super::{Node, NodeId};

/// A generic list node
pub struct NodeList<T> {
    /// The id of the current item
    pub item: NodeId<T>,
    /// The id of the next item, if it exists.
    pub next: Option<NodeListId<T>>,
}

impl<T> PartialEq for NodeList<T> {
    fn eq(&self, other: &Self) -> bool {
        self.item == other.item && self.next == other.next
    }
}
impl<T> Eq for NodeList<T> {}
impl<T> Hash for NodeList<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.item.hash(state);
        self.next.hash(state);
    }
}

/// Shorthand for `NodeId<NodeList<T>>`;
pub type NodeListId<T> = NodeId<NodeList<T>>;
pub type ListStorage = NodeList<()>;

unsafe impl<T: Any> Node for NodeList<T> {
    type Storage = ListStorage;

    fn into_storage(self) -> Self::Storage {
        unsafe { std::mem::transmute(self) }
    }

    fn from_storage_ref(storage: &Self::Storage) -> &Self {
        unsafe { std::mem::transmute(storage) }
    }

    fn from_storage_mut(storage: &mut Self::Storage) -> &mut Self {
        unsafe { std::mem::transmute(storage) }
    }
}

/// A generic list node
pub struct OptionNodeList<T> {
    /// The id of the current item
    pub item: Option<NodeId<T>>,
    /// The id of the next item, if it exists.
    pub next: Option<OptionNodeListId<T>>,
}

impl<T> PartialEq for OptionNodeList<T> {
    fn eq(&self, other: &Self) -> bool {
        self.item == other.item && self.next == other.next
    }
}
impl<T> Eq for OptionNodeList<T> {}
impl<T> Hash for OptionNodeList<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.item.hash(state);
        self.next.hash(state);
    }
}

/// Shorthand for `NodeId<NodeList<T>>`;
pub type OptionNodeListId<T> = NodeId<OptionNodeList<T>>;
pub type OptionListStorage = OptionNodeList<()>;

unsafe impl<T: Any> Node for OptionNodeList<T> {
    type Storage = OptionListStorage;

    fn into_storage(self) -> Self::Storage {
        unsafe { std::mem::transmute(self) }
    }

    fn from_storage_ref(storage: &Self::Storage) -> &Self {
        unsafe { std::mem::transmute(storage) }
    }

    fn from_storage_mut(storage: &mut Self::Storage) -> &mut Self {
        unsafe { std::mem::transmute(storage) }
    }
}
