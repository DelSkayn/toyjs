use std::any::{Any, TypeId};

pub fn downcast_ref<F: Any, T: Any>(f: &F) -> Option<&T> {
    if TypeId::of::<F>() == TypeId::of::<T>() {
        Some(unsafe { std::mem::transmute::<&F, &T>(f) })
    } else {
        None
    }
}

pub fn downcast_mut<F: Any, T: Any>(f: &mut F) -> Option<&mut T> {
    if TypeId::of::<F>() == TypeId::of::<T>() {
        Some(unsafe { std::mem::transmute::<&mut F, &mut T>(f) })
    } else {
        None
    }
}

pub trait AnyIndex<Idx> {
    fn any_get<T: Any>(&self, idx: Idx) -> Option<Option<&T>>;

    fn any_get_mut<T: Any>(&mut self, idx: Idx) -> Option<Option<&mut T>>;

    fn any_len<T: Any>(&self) -> Option<usize>;
}

impl<T: Any> AnyIndex<usize> for Vec<T> {
    #[inline]
    fn any_get<V: Any>(&self, idx: usize) -> Option<Option<&V>> {
        downcast_ref::<_, Vec<V>>(self).map(|x| x.get(idx))
    }

    #[inline]
    fn any_get_mut<V: Any>(&mut self, idx: usize) -> Option<Option<&mut V>> {
        downcast_mut::<_, Vec<V>>(self).map(|x| x.get_mut(idx))
    }

    fn any_len<V: Any>(&self) -> Option<usize> {
        downcast_ref::<_, Vec<V>>(self).map(|x| x.len())
    }
}

impl<T: Any, const S: usize> AnyIndex<usize> for [T; S] {
    #[inline]
    fn any_get<V: Any>(&self, idx: usize) -> Option<Option<&V>> {
        downcast_ref::<_, [V; S]>(self).map(|x| x.get(idx))
    }

    #[inline]
    fn any_get_mut<V: Any>(&mut self, idx: usize) -> Option<Option<&mut V>> {
        downcast_mut::<_, [V; S]>(self).map(|x| x.get_mut(idx))
    }

    #[inline]
    fn any_len<V: Any>(&self) -> Option<usize> {
        downcast_ref::<_, Vec<V>>(self).map(|x| x.len())
    }
}

pub trait AnyVec: AnyIndex<usize> {
    fn any_push<T: Any>(&mut self, value: T) -> Option<T>;

    fn any_pop<T: Any>(&mut self) -> Option<Option<T>>;
}

impl<T: Any> AnyVec for Vec<T> {
    #[inline]
    fn any_push<V: Any>(&mut self, value: V) -> Option<V> {
        let Some(this) = downcast_mut::<_, Vec<V>>(self) else {
            return Some(value)
        };
        this.push(value);
        None
    }

    #[inline]
    fn any_pop<V: Any>(&mut self) -> Option<Option<V>> {
        downcast_mut::<_, Vec<V>>(self).map(|this| this.pop())
    }
}

macro_rules! impl_tuple {
    ($($n:ident),*$(,)?) => {
        impl_tuple!(@sub $($n,)*);
    };
    (@sub $head:ident,$($rest:ident,)*) => {
        impl_tuple!(@impl $head,$($rest,)*);
        impl_tuple!(@sub $($rest,)*);
    };
    (@sub) => {
        impl_tuple!(@impl );
    };
    (@impl $($n:ident,)*) => {
        #[allow(non_snake_case)]
        impl<Idx: Clone + Copy,$($n: AnyIndex<Idx>,)*> AnyIndex<Idx> for ($($n,)*){
            #[inline]
            fn any_get<V: Any>(&self, idx: Idx) -> Option<Option<&V>> {
                let ($(ref $n,)*) = self;
                $(
                    if let Some(x) = $n.any_get(idx){
                        return Some(x)
                    }
                )*
                let _idx = idx;
                None
            }

            #[inline]
            fn any_get_mut<V: Any>(&mut self, idx: Idx) -> Option<Option<&mut V>> {
                let ($(ref mut $n,)*) = self;
                $(
                    if let Some(x) = $n.any_get_mut(idx){
                        return Some(x)
                    }
                )*
                let _idx = idx;
                None
            }

            #[inline]
            fn any_len<V: Any>(&self) -> Option<usize> {
                let ($(ref $n,)*) = self;
                $(
                    if let Some(x) = $n.any_len::<V>(){
                        return Some(x)
                    }
                )*
                None
            }
        }

        #[allow(non_snake_case)]
        impl<$($n: AnyVec,)*> AnyVec for ($($n,)*){
            #[inline]
            fn any_push<V: Any>(&mut self, value: V) -> Option<V> {
                let ($(ref mut $n,)*) = self;
                $(
                    let Some(value) = $n.any_push(value) else {
                        return None
                    };
                )*
                Some(value)
            }

            #[inline]
            fn any_pop<V: Any>(&mut self) -> Option<Option<V>> {
                let ($(ref mut $n,)*) = self;
                $(
                    if let Some(x) = $n.any_pop(){
                        return Some(x)
                    }
                )*
                None
            }
        }
    };
}
impl_tuple!(A, B, C, D, E, F, G, H);
