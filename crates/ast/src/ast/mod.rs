//! The ast datastructure used by the engine.

use std::{
    any::Any,
    hash::{BuildHasher, Hash},
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use common::{
    id,
    id::{collections::IdSet, IdRangeError},
    number::Number,
    string::String,
    thinvec::ThinVec,
};

mod list;
pub use list::{
    ListStorage, NodeList, NodeListId, OptionListStorage, OptionNodeList, OptionNodeListId,
};

#[cfg(feature = "print")]
mod print;
#[cfg(feature = "print")]
pub(crate) use print::impl_display_debug;
#[cfg(feature = "print")]
pub use print::{AstDisplay, AstFormatter, AstRender};

id!(pub struct NodeId<T>);

impl<T: Node> NodeId<T> {
    pub fn index<L>(self, ast: &Ast<L>) -> &T
    where
        L: NodeLibrary + 'static,
        T: 'static,
    {
        &ast[self]
    }

    pub fn index_mut<L>(self, ast: &mut Ast<L>) -> &mut T
    where
        L: NodeLibrary + 'static,
        T: 'static,
    {
        &mut ast[self]
    }

    pub fn erase(self) -> NodeId<()> {
        NodeId {
            id: self.id,
            _marker: PhantomData,
        }
    }
}

impl NodeId<()> {
    pub fn entype<T>(self) -> NodeId<T> {
        NodeId {
            id: self.id,
            _marker: PhantomData,
        }
    }
}

pub trait NodeStorage<T: Node> {
    fn storage_get(&self, idx: u32) -> Option<&T>;

    fn storage_get_mut(&mut self, idx: u32) -> Option<&mut T>;

    fn storage_push(&mut self, value: T) -> Option<u32>
    where
        T::Storage: Eq + Hash;
}

impl<T: Node> NodeStorage<T> for Vec<T::Storage> {
    fn storage_push(&mut self, value: T) -> Option<u32>
    where
        T::Storage: Eq + Hash,
    {
        let idx = self.len().try_into().ok()?;
        self.push(value.into_storage());
        Some(idx)
    }

    fn storage_get(&self, idx: u32) -> Option<&T> {
        let Some(x) = self.get(idx as usize) else {
            return None;
        };
        Some(T::from_storage_ref(x))
    }

    fn storage_get_mut(&mut self, idx: u32) -> Option<&mut T> {
        let Some(x) = self.get_mut(idx as usize) else {
            return None;
        };
        Some(T::from_storage_mut(x))
    }
}

impl<T: Node> NodeStorage<T> for ThinVec<T::Storage> {
    fn storage_push(&mut self, value: T) -> Option<u32>
    where
        T::Storage: Eq + Hash,
    {
        let idx = self.len().try_into().ok()?;
        self.push(value.into_storage());
        Some(idx)
    }

    fn storage_get(&self, idx: u32) -> Option<&T> {
        let Some(x) = self.get(idx) else {
            return None;
        };
        Some(T::from_storage_ref(x))
    }

    fn storage_get_mut(&mut self, idx: u32) -> Option<&mut T> {
        let Some(x) = self.get_mut(idx) else {
            return None;
        };
        Some(T::from_storage_mut(x))
    }
}

impl<T: Node, S: BuildHasher + Default> NodeStorage<T> for IdSet<u32, T::Storage, S> {
    fn storage_push(&mut self, value: T) -> Option<u32>
    where
        T::Storage: Eq + Hash,
    {
        self.push(value.into_storage()).ok()
    }

    fn storage_get(&self, idx: u32) -> Option<&T> {
        let Some(x) = self.get(idx) else {
            return None;
        };
        Some(T::from_storage_ref(x))
    }

    fn storage_get_mut(&mut self, idx: u32) -> Option<&mut T> {
        let Some(x) = self.get_mut(idx) else {
            return None;
        };
        Some(T::from_storage_mut(x))
    }
}

pub unsafe trait Node: Any {
    type Storage: Any;

    fn into_storage(self) -> Self::Storage;

    fn from_storage_ref(storage: &Self::Storage) -> &Self;

    fn from_storage_mut(storage: &mut Self::Storage) -> &mut Self;
}

unsafe impl Node for String {
    type Storage = Self;

    fn into_storage(self) -> Self::Storage {
        self
    }

    fn from_storage_ref(storage: &Self::Storage) -> &Self {
        storage
    }

    fn from_storage_mut(storage: &mut Self::Storage) -> &mut Self {
        storage
    }
}

unsafe impl Node for Number {
    type Storage = Self;

    fn into_storage(self) -> Self::Storage {
        self
    }

    fn from_storage_ref(storage: &Self::Storage) -> &Self {
        storage
    }

    fn from_storage_mut(storage: &mut Self::Storage) -> &mut Self {
        storage
    }
}

/// A collections of storages of nodes.
pub trait NodeLibrary {
    fn new() -> Self;

    fn get<T: Node>(&self, idx: u32) -> Option<&T>;

    fn get_mut<T: Node>(&mut self, idx: u32) -> Option<&mut T>;

    fn push<T: Node>(&mut self, value: T) -> Option<u32>
    where
        T::Storage: Eq + Hash;

    fn clear(&mut self);
}

/// The generic ast datastructure.
///
/// Can be indexed with the index syntax by both `NodeId` as well as `ListId`;
#[derive(Default)]
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

    pub fn library(&self) -> &L {
        &self.library
    }

    pub fn library_mut(&mut self) -> &mut L {
        &mut self.library
    }

    pub fn push<T: Node>(&mut self, value: T) -> Result<NodeId<T>, IdRangeError>
    where
        T::Storage: Eq + Hash,
    {
        self.library
            .push(value)
            .and_then(NodeId::from_u32)
            .ok_or(IdRangeError)
    }

    pub fn push_list<T: Node>(
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

    pub fn push_option_list<T: Node>(
        &mut self,
        head: &mut Option<OptionNodeListId<T>>,
        current: &mut Option<OptionNodeListId<T>>,
        item: Option<NodeId<T>>,
    ) -> Result<(), IdRangeError> {
        let list_idx = self.push(OptionNodeList { item, next: None })?;

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

impl<'a, L, T: Node> Iterator for ListIter<'a, L, T>
where
    L: NodeLibrary,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.ast.next_list(&mut self.current)?;
        Some(&self.ast[idx])
    }
}

impl<T: Node, L: NodeLibrary> Index<NodeId<T>> for Ast<L> {
    type Output = T;

    fn index(&self, index: NodeId<T>) -> &Self::Output {
        let Some(x) = self.library.get(index.into_u32()) else {
            panic!("invalid node id for type `{}`", std::any::type_name::<T>());
        };
        x
    }
}

impl<T: Node, L: NodeLibrary> IndexMut<NodeId<T>> for Ast<L> {
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
        $name:ident{ $($field:ident: $container:ident<$ty:ty>),* $(,)? }
    ) => {

        $(#[$m])*
        pub struct $name{
            $(
                pub $field: $container<$ty>
            ),*
        }

        impl $crate::ast::NodeLibrary for $name{
            fn new() -> Self{
                $name{
                    $($field: $container::default()),*
                }
            }

            fn get<T: $crate::ast::Node>(&self, idx: u32) -> Option<&T>{
                let type_id = std::any::TypeId::of::<T::Storage>();
                $(
                    if std::any::TypeId::of::<$ty>() == type_id{
                        unsafe{
                            let c = std::mem::transmute::<&$container<$ty>,&$container<T::Storage>>(&self.$field);
                            return <$container<T::Storage> as $crate::ast::NodeStorage<T>>::storage_get(c,idx)
                        }
                    }

                )*

                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn get_mut<T: $crate::ast::Node>(&mut self, idx: u32) -> Option<&mut T>{
                let type_id = std::any::TypeId::of::<T::Storage>();
                $(
                    if std::any::TypeId::of::<$ty>() == type_id{
                        unsafe{
                            let c = std::mem::transmute::<&mut $container<$ty>,&mut $container<T::Storage>>(&mut self.$field);
                            return <$container<T::Storage> as $crate::ast::NodeStorage<T>>::storage_get_mut(c,idx)
                        }
                    }
                )*
                panic!("type '{}' not part of node library",std::any::type_name::<T>());
            }

            fn push<T: $crate::ast::Node>(&mut self, value: T) -> Option<u32>
                where T::Storage: Eq + ::std::hash::Hash
            {
                let type_id = std::any::TypeId::of::<T::Storage>();
                $(
                    if std::any::TypeId::of::<$ty>() == type_id{
                        unsafe{
                            let c = std::mem::transmute::<&mut $container<$ty>,&mut $container<T::Storage>>(&mut self.$field);
                            return <$container<T::Storage> as $crate::ast::NodeStorage<T>>::storage_push(c,value)
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

}

#[macro_export]
macro_rules! ast_struct {
    (
        $(#[$m:meta])*
        $vis:vis struct $name:ident {
            $(
                pub $field:ident: $ty:ty
            ),*$(,)?
        } $(= $storage:ident)?
    ) => {

        #[derive(Clone,Copy,Debug, Eq, PartialEq, Hash)]
        $(#[$m])*
        $vis struct $name {
            $(
                pub $field: $ty,
            )*
            //pub span: crate::ast::Span
        }

        ast_struct!(@impl_id $name, $($storage)?);

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

    (@impl_id $name:ident, $storage:ident) => {
        unsafe impl crate::ast::Node for $name{
            type Storage = $storage;
        }
    };

    (@impl_id $name:ident,) => {
        unsafe impl crate::ast::Node for $name{
            type Storage = Self;

            fn into_storage(self) -> Self::Storage{
                self
            }

            fn from_storage_ref(storage: &Self::Storage) -> &Self{
                storage
            }

            fn from_storage_mut(storage: &mut Self::Storage) -> &mut Self{
                storage
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
                $(#[$vm:meta])*
                $variant:ident $({
                    $($field:ident: $ty:ty),* $(,)?
                })?
            ),*
            $(,)?
        } $(= $storage:ident)?

    ) => {

        #[derive(Clone,Copy,Debug, Eq, PartialEq, Hash)]
        #[repr(u8)]
        $(#[$m])*
        $vis enum $name {
            $(
                $(#[$vm])*
                $variant$({
                    $($field : $ty,)*
                })?
            ),*
        }

        ast_enum!{@impl_id $name, $($storage)?}

        #[cfg(feature = "print")]
        impl<L,W> crate::ast::AstDisplay<L,W> for $name
            where L: crate::ast::NodeLibrary,
                  W: ::std::fmt::Write,
        {
          fn fmt(&self, fmt: &mut crate::ast::AstFormatter<L,W>) -> ::std::fmt::Result{
              match self{
                  $(
                      $name::$variant$({$($field),*})* => {
                          write!(fmt,concat!(stringify!($name),"::",stringify!($variant)))?;
                          $(
                              writeln!(fmt,"{{")?;
                              fmt.indent(|fmt|{
                                  $(
                                      write!(fmt,concat!(stringify!($field),": "))?;
                                      crate::ast::AstDisplay::fmt($field, fmt)?;
                                      writeln!(fmt,",")?;
                                  )*
                                      Ok(())
                              })?;
                              write!(fmt,"}}")?;
                          )*
                      }
                  )*
              }
              Ok(())
          }
        }
    };


    (@impl_id $name:ident, $storage:ident) => {
        impl crate::ast::Node for $name{
            type Storage = $storage;
        }
    };

    (@impl_id $name:ident,) => {
        unsafe impl crate::ast::Node for $name{
            type Storage = Self;

            fn into_storage(self) -> Self::Storage{
                self
            }

            fn from_storage_ref(storage: &Self::Storage) -> &Self{
                storage
            }

            fn from_storage_mut(storage: &mut Self::Storage) -> &mut Self{
                storage
            }
        }
    };
}
