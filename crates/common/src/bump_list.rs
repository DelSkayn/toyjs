use bumpalo::{boxed::Box, Bump};
use std::iter::Iterator;

#[derive(Debug, Eq, PartialEq)]
enum ListItem<'alloc, T> {
    End(T),
    Chain(T, Box<'alloc, ListItem<'alloc, T>>),
}

#[derive(Debug)]
pub struct List<'alloc, T> {
    head: Option<ListItem<'alloc, T>>,
    alloc: &'alloc Bump,
}

impl<'alloc, T> List<'alloc, T> {
    pub fn new_in(alloc: &'alloc Bump) -> Self {
        List { head: None, alloc }
    }

    pub fn push(&mut self, value: T) {
        if let Some(x) = self.head.take() {
            let r = Box::new_in(x, self.alloc);
            self.head.replace(ListItem::Chain(value, r));
        } else {
            self.head = Some(ListItem::End(value))
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        let v = self.head.take()?;
        match v {
            ListItem::End(x) => Some(x),
            ListItem::Chain(x, rest) => {
                let rest = unsafe { Box::into_raw(rest).read() };
                self.head = Some(rest);
                Some(x)
            }
        }
    }

    pub fn iter<'a>(&'a self) -> ListIter<'a, 'alloc, T> {
        ListIter {
            item: self.head.as_ref(),
        }
    }

    pub fn iter_mut<'a>(&'a mut self) -> ListIterMut<'a, 'alloc, T> {
        ListIterMut {
            item: self.head.as_mut(),
        }
    }
}

pub struct ListIter<'a, 'alloc, T> {
    item: Option<&'a ListItem<'alloc, T>>,
}

impl<'a, 'alloc, T> Iterator for ListIter<'a, 'alloc, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let v = self.item.take()?;
        match *v {
            ListItem::End(ref x) => Some(x),
            ListItem::Chain(ref x, ref rest) => {
                self.item = Some(rest.as_ref());
                Some(x)
            }
        }
    }
}

pub struct ListIterMut<'a, 'alloc, T> {
    item: Option<&'a mut ListItem<'alloc, T>>,
}

impl<'a, 'alloc, T> Iterator for ListIterMut<'a, 'alloc, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        let v = self.item.take()?;
        match *v {
            ListItem::End(ref mut x) => Some(x),
            ListItem::Chain(ref mut x, ref mut rest) => {
                self.item = Some(rest.as_mut());
                Some(x)
            }
        }
    }
}
