use std::{
    marker::PhantomData,
    ops::{Index, IndexMut, RangeBounds},
    slice::{Iter, IterMut},
    vec::Drain,
};

/// A macro which implements a newtype index.
#[macro_export]
macro_rules! key {
    ($(#[$m:meta])* $v:vis struct $name:ident($data:ty)) => {

        $(#[ $m ])*
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
    ($(#[$m:meta])* $v:vis struct $name:ident(#[non_zero] $data:ty)) => {
        $(#[ $m ])*
        #[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
        #[repr(transparent)]
        $v struct $name($data);

        impl $crate::id::Id for $name {
        }

        impl TryFrom<usize> for $name {
            type Error = <u32 as TryFrom<usize>>::Error;

            #[inline]
            fn try_from(value: usize) -> ::std::result::Result<Self, Self::Error> {
                let x: u32 = (value + 1).try_into()?;
                Ok($name(unsafe{ <$data>::new_unchecked(x) }))
            }
        }

        impl TryFrom<$name> for usize {
            type Error = <usize as TryFrom<u32>>::Error;

            #[inline]
            fn try_from(value: $name) -> ::std::result::Result<Self, Self::Error> {
                (u32::from(value.0) - 1).try_into()
            }
        }
    }
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

    pub fn drain<R>(&mut self, range: R) -> Drain<'_, T>
    where
        R: RangeBounds<usize>,
    {
        self.vec.drain(range)
    }

    pub fn as_inner(&self) -> &Vec<T> {
        &self.vec
    }

    pub fn as_inner_mut(&mut self) -> &mut Vec<T> {
        &mut self.vec
    }
}

impl<K: Id, T: Clone + Default> KeyedVec<K, T> {
    /// Resizes the vector so it contains value up till the key and then inserts the value.
    pub fn insert_grow_default(&mut self, key: K, value: T) {
        self.insert_grow(key, value, Default::default())
    }
}

impl<K: Id, T: Clone> KeyedVec<K, T> {
    /// Resizes the vector so it contains value up till the key and then inserts the value.
    pub fn insert_grow(&mut self, key: K, value: T, fill: T) {
        let Ok(idx) = key.try_into() else {
            panic!("could not convert key to usize")
        };
        if self.len() <= idx {
            self.vec.resize(idx, fill);
            self.vec.push(value);
        } else {
            self.vec[idx] = value;
        }
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

    pub fn next_id(&mut self) -> K {
        let Ok(x) = K::try_from(self.len()) else {
            panic!("could not convert index to usize")
        };
        x
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

pub struct BinaryVec<K, T>(Vec<(K, T)>);

impl<K: Ord + Copy, V> BinaryVec<K, V> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn insert_after(&mut self, key: K, value: V) {
        if let Some(x) = self.0.last() {
            assert!(x.0 < key);
        }
        self.0.push((key, value));
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        let idx = self.0.binary_search_by_key(key, |x| x.0).ok()?;
        Some(&self.0[idx].1)
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        let idx = self.0.binary_search_by_key(key, |x| x.0).ok()?;
        Some(&mut self.0[idx].1)
    }

    pub fn iter(&self) -> impl Iterator<Item = &(K, V)> {
        self.0.iter()
    }
}

impl<K: Ord + Copy, V> Default for BinaryVec<K, V> {
    fn default() -> Self {
        Self::new()
    }
}
