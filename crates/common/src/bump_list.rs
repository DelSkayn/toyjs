use bumpalo::Bump;
use std::{
    cell::{Cell, UnsafeCell},
    cmp::PartialEq,
    fmt,
    iter::Iterator,
    marker::PhantomData,
    mem::{self, ManuallyDrop},
    ptr,
};

pub struct ListItem<'alloc, T> {
    prev: Cell<Option<&'alloc Self>>,
    next: Cell<Option<&'alloc Self>>,
    v: UnsafeCell<ManuallyDrop<T>>,
}

impl<'alloc, T: fmt::Debug> fmt::Debug for ListItem<'alloc, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ListItem")
            .field("next", &self.next)
            .field("v", unsafe { &(**self.v.get()) })
            .finish()
    }
}

impl<'alloc, T: PartialEq> PartialEq for ListItem<'alloc, T> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { (&(*self.v.get())).eq(&(*other.v.get())) }
    }
}

pub struct List<'alloc, T> {
    first: Option<&'alloc ListItem<'alloc, T>>,
    last: Option<&'alloc ListItem<'alloc, T>>,
    alloc: &'alloc Bump,
}

impl<'alloc, T: PartialEq> PartialEq for List<'alloc, T> {
    fn eq(&self, other: &Self) -> bool {
        let mut iter = self.iter();
        let mut other_iter = other.iter();
        while let Some(a) = iter.next() {
            if let Some(b) = other_iter.next() {
                if a != b {
                    return false;
                }
            } else {
                return false;
            }
        }
        other_iter.next().is_none()
    }
}

impl<'alloc, T: fmt::Debug> fmt::Debug for List<'alloc, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("List")
            .field("first", &self.first)
            .field("alloc", &self.alloc)
            .finish()
    }
}

impl<'alloc, T> List<'alloc, T> {
    pub fn new_in(alloc: &'alloc Bump) -> Self {
        List {
            first: None,
            last: None,
            alloc,
        }
    }

    pub fn push(&mut self, value: T) {
        let n = self.alloc.alloc(ListItem {
            prev: Cell::new(None),
            next: Cell::new(None),
            v: UnsafeCell::new(ManuallyDrop::new(value)),
        });
        if let Some(x) = self.last {
            x.next.set(Some(n));
            self.last = Some(n);
            n.prev.set(Some(x));
        } else {
            self.first = Some(n);
            self.last = Some(n);
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if let Some(x) = self.last {
            self.last = x.prev.get();
            if ptr::eq(self.first.unwrap(), x) {
                self.first = None;
            }
            unsafe { Some(ManuallyDrop::take(&mut *x.v.get())) }
        } else {
            None
        }
    }

    pub fn append(&mut self, other: &mut Self) {
        if let Some(x) = self.last {
            let o_first = other.first.take();
            o_first.as_ref().map(|x| x.prev.set(Some(x)));
            x.next.set(o_first);
            self.last = other.last.take();
        } else {
            self.first = other.first.take();
            self.last = other.last.take();
        }
    }

    pub fn iter<'a>(&'a self) -> ListIter<'a, 'alloc, T> {
        ListIter { item: self.first }
    }

    pub fn iter_mut<'a>(&'a mut self) -> ListIterMut<'a, 'alloc, T> {
        ListIterMut {
            item: self.first,
            __marker: PhantomData,
        }
    }
}

impl<'alloc, T> Drop for List<'alloc, T> {
    fn drop(&mut self) {
        if mem::needs_drop::<T>() {
            let mut item = self.first;
            while let Some(x) = item {
                unsafe { ManuallyDrop::drop(&mut *x.v.get()) }
                item = x.next.take();
            }
        }
    }
}

pub struct ListIter<'a, 'alloc, T> {
    item: Option<&'a ListItem<'alloc, T>>,
}

impl<'a, 'alloc, T> Iterator for ListIter<'a, 'alloc, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let item = self.item?;
        let res = unsafe { &*item.v.get() };
        self.item = item.next.get();
        Some(res)
    }
}

pub struct ListIterMut<'a, 'alloc, T> {
    item: Option<&'a ListItem<'alloc, T>>,
    __marker: PhantomData<&'a mut List<'alloc, T>>,
}

impl<'a, 'alloc, T> Iterator for ListIterMut<'a, 'alloc, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        let item = self.item?;
        let res = unsafe { &mut *item.v.get() };
        self.item = item.next.get();
        Some(res)
    }
}
