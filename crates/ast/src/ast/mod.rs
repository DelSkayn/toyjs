//! The ast datastructure used by the engine.

use core::fmt;
use std::{
    any::Any,
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

use common::{
    id,
    id::{Id, IdRangeError},
};

static TOO_MANY_NODES: &str = "Too many nodes in storage to fit 32 bit id.";

/// Generates the panic for when a type is not in the storage.
#[cold]
fn panic_no_storage<T>() -> ! {
    panic!(
        "Node of type `{}` not present in storage.",
        std::any::type_name::<T>()
    );
}

id!(pub struct NodeId<T>);
pub type NodeListId<T> = NodeId<NodeList<T>>;

/// A generic list node
pub struct NodeList<T> {
    /// The id of the current item
    pub item: NodeId<T>,
    /// The id of the next item, if it exists.
    pub next: Option<NodeListId<T>>,
}

/// A generic list node for when the normal list node would be inefficient.
pub struct List<T> {
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

/// A collections of storages of nodes.
pub trait NodeLibrary {
    fn new() -> Self;

    fn get<T: Any>(&self, idx: u32) -> Option<&T>;

    fn get_mut<T: Any>(&mut self, idx: u32) -> Option<&mut T>;

    fn push<T: Any>(&mut self, value: T) -> u32;

    fn clear(&mut self);
}

/// The generic ast datastructure.
///
/// Can be indexed with the index syntax by both `NodeId` as well as `ListId`;
#[derive(Default)]
pub struct Ast<L: NodeLibrary> {
    storage: L,
}

pub struct Ast<L> {
    library: L,
}

impl<L> Ast<L>
where
    L: NodeLibrary,
{
    pub fn new() -> Self {
        Self { library: L::new() }
    }

    pub fn push<T: Any>(&mut self, value: T) -> Result<NodeId<T>, IdRangeError> {
        let idx = self.library.push(value);
        NodeId::from_u32(idx)
    }

    pub fn push_list<T: Any>(
        &mut self,
        head: &mut Option<NodeListId<T>>,
        current: &mut Option<NodeListId<T>>,
        item: NodeId<T>,
    ) -> Result<(), IdRangeError> {
        let list_idx = self.push(NodeList { item, next: None })?;

        if let Some(x) = *current {
            self[x].next = Some(list_idx);
        }

        *current = Some(list_idx);
        *head = head.or(*current);

        Ok(())
    }

    pub fn clear(&mut self) {
        self.library.clear()
    }

    pub fn iter_list<T>(&self, id: Option<NodeListId<T>>) -> ListIter<L, T> {
        ListIter {
            ast: self,
            current: id,
        }
    }

    pub fn next_list<'a, T: 'static>(
        &'a self,
        id: &mut Option<NodeListId<T>>,
    ) -> Option<NodeId<T>> {
        let node = (*id)?;

        *id = self[node].next;
        let v = self[node].item;

        Some(v)
    }

    pub fn next_list_mut<'a, T: 'static>(
        &'a mut self,
        id: &mut Option<NodeListId<T>>,
    ) -> Option<NodeId<T>> {
        let node = (*id)?;

        *id = self[node].next;
        let v = self[node].item;

        Some(v)
    }
}

pub struct ListIter<'a, L, T> {
    ast: &'a Ast<L>,
    current: Option<NodeListId<T>>,
}

impl<'a, L, T: 'static> Iterator for ListIter<'a, L, T>
where
    L: NodeLibrary,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.ast.next_list(&mut self.current)?;
        Some(&self.ast[idx])
    }
}

impl<T: Any, L: NodeLibrary> Index<NodeId<T>> for Ast<L> {
    type Output = T;

    fn index(&self, index: NodeId<T>) -> &Self::Output {
        let Some(x) = self.library.get(index.into_u32()) else {
            panic!("invalid node id for type `{}`", std::any::type_name::<T>());
        };
        x
    }
}

impl<T: Any, L: NodeLibrary> IndexMut<NodeId<T>> for Ast<L> {
    fn index_mut(&mut self, index: NodeId<T>) -> &mut Self::Output {
        let Some(x) = self.library.get_mut(index.into_u32()) else {
            panic!("invalid node id for type `{}`", std::any::type_name::<T>());
        };
        x
    }
}

#[macro_export]
macro_rules! library {
    (
        $(#[$m:meta])*
        $name:ident{ $($field:ident: $container:ident<$( $ty:ty ),* $(,)?>),* $(,)? }
    ) => {

        $(#[$m])*
        pub struct $name{
            $(
                $field: $container<$( $ty ),*>
            ),*
        }

        impl $crate::ast::NodeLibrary for $name{
            fn new() -> Self{
                $name{
                    $($field: $container::new()),*
                }
            }

            fn get<T: std::any::Any>(&self, idx: u32) -> Option<&T>{
                let type_id = std::any::TypeId::of::<T>();
                $(
                    if std::any::TypeId::of::<library!(@last $($ty,)*)>() == type_id{
                        unsafe{
                            return std::mem::transmute::<&$container<$($ty,)*>,&$container<T>>(&self.$field).get(idx)
                        }
                    }

                )*

                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn get_mut<T: std::any::Any>(&mut self, idx: u32) -> Option<&mut T>{
                let type_id = std::any::TypeId::of::<T>();
                $(
                    if std::any::TypeId::of::<library!(@last $($ty,)*)>() == type_id{
                        unsafe{
                            return std::mem::transmute::<&mut $container<$($ty,)*>,&mut $container<T>>(&mut self.$field).get_mut(idx)
                        }
                    }
                )*
                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn push<T: std::any::Any>(&mut self, value: T) -> u32{
                let type_id = std::any::TypeId::of::<T>();
                $(
                    if std::any::TypeId::of::<library!(@last $($ty,)*)>() == type_id{
                        unsafe{
                            let idx = self.$field.len();
                            std::mem::transmute::<&mut $container<$($ty,)*>,&mut $container<T>>(&mut self.$field).push(value);
                            return idx
                        }
                    }
                )*
                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn clear(&mut self){
                $(
                    self.$field.clear();
                )*
            }

        }

    };

    (@last $f:ty, $($t:ty,)+) => {
        library!(@last $($t,)*)
    };
    (@last $f:ty,) => {
        $f
    };
}

#[macro_export]
macro_rules! ast_struct {
    (
        $(#[$m:meta])*
        $vis:vis struct $name:ident {
            $(
                pub $field:ident: $ty:ty
            ),*$(,)?
        }


    ) => {

        #[derive(Clone,Copy,Debug)]
        $vis struct $name {
            $(
                pub $field: $ty,
            )*
            pub span: crate::ast::Span
        }

        impl crate::ast::Spanned for $name
        {
            fn span(&self) -> crate::ast::Span {
                self.span
            }
        }

        #[cfg(feature = "print")]
        impl<L,W> crate::ast::AstDisplay<L,W> for $name
            where L: crate::ast::NodeLibrary,
                  W: ::std::fmt::Write,
        {
            fn fmt(&self, fmt: &mut crate::ast::AstFormatter<L,W>) -> ::std::fmt::Result{
                writeln!(fmt,concat!(stringify!($name)," {{"))?;
                fmt.indent(|fmt|{
                    $(
                        write!(fmt,concat!(stringify!($field),": "))?;
                        crate::ast::AstDisplay::fmt(&self.$field,fmt)?;
                        writeln!(fmt,",")?;
                    )*
                    Ok(())
                })?;
                write!(fmt,"}}")
            }
        }
    };
}

#[macro_export]
macro_rules! ast_enum{
    (
        $(#[$m:meta])*
        $vis:vis enum $name:ident {
            $(
                $variant:ident($ty:ty)
            ),*$(,)?
        }


    ) => {

        #[derive(Clone,Copy,Debug)]
        $vis enum $name {
            $(
                $variant($ty)
            ),*
        }


        impl $crate::ast::AstSpanned for $name
        {
            fn ast_span<L: $crate::ast::NodeLibrary>(&self, ast: &$crate::ast::Ast<L>) -> $crate::ast::Span {
                match *self{
                    $(
                        Self::$variant(x) => {
                            x.ast_span(ast)
                        }
                    )*
                }
            }
        }


        #[cfg(feature = "print")]
        impl<L,W> crate::ast::AstDisplay<L,W> for $name
            where L: crate::ast::NodeLibrary,
                  W: ::std::fmt::Write,
        {
            fn fmt(&self, fmt: &mut crate::ast::AstFormatter<L,W>) -> ::std::fmt::Result{
                match self{
                    $(
                    $name::$variant(x) => {
                        write!(fmt,concat!(stringify!($name),"::",stringify!($variant),"("))?;
                        x.fmt(fmt)?;
                        write!(fmt,")")
                    }
                    )*
                }
            }
        }
    };
}
