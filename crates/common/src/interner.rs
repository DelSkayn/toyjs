//! An interner mapping equivalent to a single id.

use core::hash::Hash;
use hashbrown::{Equivalent, HashMap};
use std::ops::Index;

pub struct Interner<K: Hash + Eq, I> {
    map: HashMap<K, I>,
    items: Vec<K>,
}

impl<K: Hash + Eq, I: TryFrom<usize> + TryInto<usize> + Copy> Interner<K, I> {
    /// Create a new iterner
    #[inline]
    pub fn new() -> Self {
        Interner {
            map: HashMap::new(),
            items: Vec::new(),
        }
    }

    /// Intern a value, returning the an id to the value
    #[inline]
    pub fn intern<'a, V>(&mut self, value: &'a V) -> I
    where
        V: Hash + Equivalent<K> + 'a,
        K: From<&'a V>,
    {
        let res = self.map.entry_ref(value).or_insert_with(|| {
            let Ok(len) = self
                .items
                .len()
                .try_into() else{
                    panic!("too many values in interner to fit in bits")
            };
            self.items.push(K::from(value));
            len
        });
        *res
    }

    /// Skip hashing a value returning a new id regardless if the value was already interned
    #[inline]
    pub fn skip_push<V>(&mut self, value: V) -> I
    where
        K: From<V>,
    {
        let Ok(len) = self
            .items
            .len()
            .try_into() else{
                panic!("too many values in interner to fit in bits")
        };
        self.items.push(K::from(value));
        len
    }

    /// Get the value associated with the id.
    #[inline]
    pub fn get(&self, id: I) -> Option<&K> {
        let Ok(idx) = id.try_into() else{
            panic!("failed to convert id to interner index");
        };
        self.items.get(idx)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl<K: Hash + Eq, I: TryFrom<usize> + TryInto<usize> + Copy> Default for Interner<K, I> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, I> Index<I> for Interner<K, I>
where
    K: Hash + Eq,
    I: TryInto<usize>,
{
    type Output = K;

    fn index(&self, index: I) -> &Self::Output {
        let Ok(idx) = index.try_into() else{
            panic!("failed to convert id to interner index");
        };
        &self.items[idx]
    }
}

/// A macro which implements a newtype index.
#[macro_export]
macro_rules! id {
    ($v:vis struct $name:ident($data:ty)) => {
        #[derive(Clone, Copy, Eq, PartialEq, Hash, bytemuck::Pod, bytemuck::Zeroable, Debug)]
        #[repr(transparent)]
        $v struct $name($data);

        impl TryFrom<usize> for $name {
            type Error = <$data as TryFrom<usize>>::Error;

            #[inline]
            fn try_from(value: usize) -> Result<Self, Self::Error> {
                Ok($name(value.try_into()?))
            }
        }

        impl TryFrom<$name> for usize {
            type Error = <usize as TryFrom<u32>>::Error;

            #[inline]
            fn try_from(value: $name) -> Result<Self, Self::Error> {
                value.0.try_into()
            }
        }
    };
}
