use std::{
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

use common::span::Span;

use crate::{AstNode, Node};

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

pub struct Ast<R> {
    nodes: Vec<AstNode>,
    spans: Vec<Span>,
    marker: PhantomData<R>,
}

impl<R: Node> Ast<R> {
    pub fn new(root: R, span: Span) -> Self {
        Ast {
            nodes: vec![root.into_node()],
            spans: vec![span],
            marker: PhantomData,
        }
    }

    pub fn push_node<N: Node>(&mut self, node: N, span: Span) -> NodeId<N> {
        let id = u32::try_from(self.nodes.len())
            .expect("too many nodes")
            .try_into()
            .unwrap();
        self.nodes.push(node.into_node());
        self.spans.push(span);
        NodeId {
            id,
            marker: PhantomData,
        }
    }

    pub fn root(&self) -> &R {
        R::from_node(&self.nodes[0])
    }

    pub fn root_mut(&mut self) -> &mut R {
        R::from_node_mut(&mut self.nodes[0])
    }

    pub fn span<N: Node>(&self, id: NodeId<N>) -> &Span {
        let idx = usize::try_from(u32::from(id.id)).unwrap();
        &self.spans[idx]
    }

    pub fn span_mut<N: Node>(&mut self, id: NodeId<N>) -> &mut Span {
        let idx = usize::try_from(u32::from(id.id)).unwrap();
        &mut self.spans[idx]
    }
}

impl<R, N: Node> Index<NodeId<N>> for Ast<R> {
    type Output = N;

    fn index(&self, index: NodeId<N>) -> &Self::Output {
        let idx = usize::try_from(u32::from(index.id)).unwrap();
        N::from_node(&self.nodes[idx])
    }
}

impl<R, N: Node> IndexMut<NodeId<N>> for Ast<R> {
    fn index_mut(&mut self, index: NodeId<N>) -> &mut Self::Output {
        let idx = usize::try_from(u32::from(index.id)).unwrap();
        N::from_node_mut(&mut self.nodes[idx])
    }
}
