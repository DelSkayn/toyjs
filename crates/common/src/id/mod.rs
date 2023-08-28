use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
    slice::{Iter, IterMut},
};

/// A macro which implements a newtype index.
#[macro_export]
macro_rules! id {
    ($v:vis struct $name:ident($data:ty)) => {
        #[derive(Clone, Copy, Eq, PartialEq, Hash, bytemuck::Pod, bytemuck::Zeroable, Debug)]
        #[repr(transparent)]
        $v struct $name($data);

        impl $crate::id::Id for $name {
        }

        impl TryFrom<usize> for $name {
            type Error = <$data as TryFrom<usize>>::Error;

            #[inline]
            fn try_from(value: usize) -> ::std::result::Result<Self, Self::Error> {
                Ok($name(value.try_into()?))
            }
        }

        impl TryFrom<$name> for usize {
            type Error = <usize as TryFrom<u32>>::Error;

            #[inline]
            fn try_from(value: $name) -> ::std::result::Result<Self, Self::Error> {
                value.0.try_into()
            }
        }
    };
}

pub trait Id: TryFrom<usize> + TryInto<usize> {}

#[derive(Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct KeyedVec<K, T> {
    vec: Vec<T>,
    _marker: PhantomData<K>,
}

impl<K, T> KeyedVec<K, T> {
    pub fn new() -> Self {
        KeyedVec {
            vec: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn shrink_to_fit(&mut self) {
        self.vec.shrink_to_fit()
    }

    pub fn pop(&mut self) -> Option<T> {
        self.vec.pop()
    }

    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.vec.iter()
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        self.vec.iter_mut()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }
}

impl<K: Id, T> KeyedVec<K, T> {
    pub fn push(&mut self, value: T) -> K {
        let Ok(res) = K::try_from(self.len()) else {
            panic!("could not convert index to usize")
        };
        self.vec.push(value);
        res
    }

    pub fn try_push(&mut self, value: T) -> Result<K, T> {
        let Ok(res) = K::try_from(self.len()) else {
            return Err(value);
        };
        self.vec.push(value);
        Ok(res)
    }
}

impl<V, K, T> From<V> for KeyedVec<K, T>
where
    Vec<T>: From<V>,
{
    fn from(v: V) -> Self {
        KeyedVec {
            vec: Vec::from(v),
            _marker: PhantomData,
        }
    }
}

impl<K, T> AsRef<[T]> for KeyedVec<K, T> {
    fn as_ref(&self) -> &[T] {
        self.vec.as_ref()
    }
}

impl<K: Id, T> Index<K> for KeyedVec<K, T> {
    type Output = T;

    fn index(&self, index: K) -> &Self::Output {
        if let Ok(x) = index.try_into() {
            &self.vec[x]
        } else {
            panic!("could not convert index to usize")
        }
    }
}

impl<K: Id, T> IndexMut<K> for KeyedVec<K, T> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        if let Ok(x) = index.try_into() {
            &mut self.vec[x]
        } else {
            panic!("could not convert index to usize")
        }
    }
}
