use std::{
    cmp::Ordering,
    marker::PhantomData,
    ops::{
        Index, IndexMut, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive,
    },
};

use crate::id::{Id, IdRangeError};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IdVec<I, T> {
    inner: Vec<T>,
    _marker: PhantomData<I>,
}

impl<I, T> Default for IdVec<I, T> {
    fn default() -> Self {
        IdVec::new()
    }
}

impl<I, T> IdVec<I, T> {
    /// Create a new `IdVec`.
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
            _marker: PhantomData,
        }
    }

    /// Create a new `IdVec`.
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: Vec::with_capacity(cap),
            _marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn clear(&mut self) {
        self.inner.clear();
    }
}

impl<I: Id, T> IdVec<I, T> {
    /// Returns the id of the next value that would be pushed.
    /// Will return None if the size exceeded the value the index can hold.
    pub fn next_id(&self) -> Result<I, IdRangeError> {
        I::from_idx(self.inner.len())
    }

    /// Add a new value to the vector returning the id.
    /// Will return None if the size exceeded the value the index can hold.
    pub fn push(&mut self, value: T) -> Result<I, IdRangeError> {
        let idx = I::from_idx(self.inner.len())?;
        self.inner.push(value);
        Ok(idx)
    }

    /// Inserts the value the given index.
    ///
    /// If the current length is less then the index of where this value should be inserted all the
    /// other values will be filled with the given callback.
    pub fn insert_fill<F>(&mut self, at: I, value: T, mut fill: F)
    where
        F: FnMut() -> T,
    {
        let idx = at.idx();
        match idx.cmp(&self.inner.len()) {
            Ordering::Less => {
                self[at] = value;
            }
            Ordering::Equal => {
                self.inner.push(value);
            }
            Ordering::Greater => {
                self.inner.reserve(idx - self.inner.len());
                for _ in self.inner.len()..idx {
                    self.inner.push(fill());
                }
                self.inner.push(value);
            }
        }
    }

    pub fn get(&self, i: I) -> Option<&T> {
        let idx = i.idx();
        if self.inner.len() <= idx {
            return None;
        }
        Some(&self.inner[idx])
    }

    pub fn get_mut(&mut self, i: I) -> Option<&mut T> {
        let idx = i.idx();
        if self.inner.len() <= idx {
            return None;
        }
        Some(&mut self.inner[idx])
    }
}

impl<I: Id, T: Default> IdVec<I, T> {
    /// Inserts the value the given index.
    ///
    /// If the current length is less then the index of where this value should be inserted all the
    /// other values will be filled with the given callback.
    pub fn insert_fill_default(&mut self, at: I, value: T) {
        let idx = at.idx();
        match idx.cmp(&self.inner.len()) {
            Ordering::Less => {
                self[at] = value;
            }
            Ordering::Equal => {
                self.inner.push(value);
            }
            Ordering::Greater => {
                self.inner.reserve(idx - self.inner.len());
                for _ in self.inner.len()..idx {
                    self.inner.push(Default::default());
                }
                self.inner.push(value);
            }
        }
    }
}

impl<I: Id, T> Index<I> for IdVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        self.inner.index(index.idx())
    }
}

impl<I: Id, T> IndexMut<I> for IdVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.inner.index_mut(index.idx())
    }
}

macro_rules! impl_range {
    ($name:ident$(<$gen:ident>)?, $($beg:ident)? [$t:tt] $($end:ident)?) => {
        impl<I: Id,T> Index<$name $(<$gen>)? > for IdVec<I, T> {
            type Output = [T];

            fn index(&self, _index: $name$(<$gen>)?) -> &Self::Output {
                self.inner.index($(_index.$beg.idx())? $t $(_index.$end.idx())?)
            }
        }

        impl<I : Id, T> IndexMut<$name$(<$gen>)?> for IdVec<I, T> {
            fn index_mut(&mut self, _index: $name$(<$gen>)?) -> &mut Self::Output {
                self.inner.index_mut($(_index.$beg.idx())? $t $(_index.$end.idx())?)
            }
        }
    };
}

impl_range!(Range<I>, start[..]end);
impl_range!(RangeFrom<I>, start[..]);
impl_range!(RangeFull, [..]);
impl_range!(RangeTo<I>, [..]end);
impl_range!(RangeToInclusive<I>, [..=]end);

impl<I: Id, T> Index<RangeInclusive<I>> for IdVec<I, T> {
    type Output = [T];
    fn index(&self, _index: RangeInclusive<I>) -> &Self::Output {
        self.inner.index(_index.start().idx()..=_index.end().idx())
    }
}
impl<I: Id, T> IndexMut<RangeInclusive<I>> for IdVec<I, T> {
    fn index_mut(&mut self, _index: RangeInclusive<I>) -> &mut Self::Output {
        self.inner
            .index_mut(_index.start().idx()..=_index.end().idx())
    }
}
