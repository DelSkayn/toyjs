#[cfg(not(feature = "usize_index"))]
pub type Index = u32;

#[cfg(feature = "usize_index")]
pub type Index = usize;

#[macro_export]
macro_rules! newtype_index{
    ($(#[$attrs:meta])* $v:vis struct $name:ident) => {

        $(#[$attrs])*
        #[derive(Clone,Copy, Eq, PartialEq, Hash, Debug)]
        $v struct $name($crate::index::Index);

        impl From<usize> for $name{
            fn from(v: usize) -> Self{
                use std::convert::TryInto;
                Self(v.try_into().unwrap())
            }
        }

        impl From<u32> for $name{
            fn from(v: u32) -> Self{
                Self(v as $crate::index::Index)
            }
        }

        impl From<$name> for usize{
            fn from(v: $name) -> Self{
                v.0 as usize
            }
        }

        impl From<$name> for u32{
            fn from(v: $name) -> Self{
                use std::convert::TryInto;
                v.0.try_into().unwrap()
            }
        }
    }
}

#[macro_export]
macro_rules! newtype_vec{
    (struct $name:ident
        $(<$($lt:lifetime,)*$($gen:ident,)*>)*
        [$idx:ident]
        -> $out:ident$(<$($out_lt:lifetime,)*$($out_gen:ident,)*>)*)=>
     {
        newtype_vec!(
            @name ($name)
            @lifetimes ($($($lt)*)*)
            @generics ($($($gen)*)*)
            @member (.0)
            @index ($idx)
            @out ($out)
            @out_lifetimes ($($($out_lt)*)*)
            @out_generics ($($($out_gen)*)*)

            );

    };

    (struct $name:ident$(<$($lt:lifetime,)*$($gen:ident,)*>)*
        $(. $member:ident)+
        [$idx:ident]
        -> $out:ident$(<$($out_lt:lifetime,)*$($out_gen:ident,)*>)*) => {
        newtype_vec!(
            @name ($name)
            @lifetimes ($($($lt)*)*)
            @generics ($($($gen)*)*)
            @member ($(. $member)*)
            @index ($idx)
            @out ($out)
            @out_lifetimes ($($($out_lt)*)*)
            @out_generics ($($($out_gen)*)*)
            );
    };

    (@name ($name:ident)
     @lifetimes ($($lt:lifetime)*)
     @generics ($($gen:ident)*)
     @member ($($member:tt)*)
     @index ($index:ident)
     @out ($out:ident)
     @out_lifetimes ($($out_lt:lifetime)*)
     @out_generics ($($out_gen:ident)*)
     ) => {
        impl <$($lt,)* $($gen,)*> $name <$($lt,)* $($gen,)*>{

            pub fn push(&mut self, value: $out <$($out_lt,)* $($out_gen,)*>) -> $index {
                let res = $index::from(self.len());
                self$($member)*.push(value);
                res
            }

            pub fn len(&self) -> usize{
                self$($member)*.len()
            }

            pub fn iter(&self) -> impl std::iter::Iterator<Item = &$out<$($out_lt,)* $($out_gen)*>>{
                self$($member)*.iter()
            }

            pub fn iter_mut(&mut self) -> impl std::iter::Iterator<Item = &mut $out<$($out_lt,)* $($out_gen)*>>{
                self$($member)*.iter_mut()
            }

            pub fn clear(&mut self) {
                self$($member)*.clear();
            }
        }

        impl <$($lt,)* $($gen,)*> std::ops::Index<$index> for $name<$($lt,)* $($gen,)*>{
            type Output = $out<$($out_lt,)* $($out_gen)*>;
            fn index(&self, idx: $index) -> &$out<$($out_lt,)* $($out_gen)*> {
                &self$($member)*[usize::from(idx)]
            }
        }

        impl <$($lt,)* $($gen,)*> std::ops::IndexMut<$index> for $name<$($lt,)* $($gen,)*>{
            fn index_mut(&mut self, idx: $index) -> &mut $out<$($out_lt,)* $($out_gen)*> {
                &mut self$($member)*[usize::from(idx)]
            }
        }


    };
}

#[macro_export]
macro_rules! newtype_slice {
    (struct $name:ident
        $(<$($lt:lifetime,)*$($gen:ident,)*>)*
        [$idx:ident]
        -> $out:ident$(<$($out_lt:lifetime,)*$($out_gen:ident,)*>)*)=>
     {
        newtype_slice!(
            @name ($name)
            @lifetimes ($($($lt)*)*)
            @generics ($($($gen)*)*)
            @member (.0)
            @index ($idx)
            @out ($out)
            @out_lifetimes ($($($out_lt)*)*)
            @out_generics ($($($out_gen)*)*)

            );

    };

    (struct $name:ident$(<$($lt:lifetime,)*$($gen:ident,)*>)*
        $(. $member:ident)+
        [$idx:ident]
        -> $out:ident$(<$($out_lt:lifetime,)*$($out_gen:ident,)*>)*) => {
        newtype_slice!(
            @name ($name)
            @lifetimes ($($($lt)*)*)
            @generics ($($($gen)*)*)
            @member ($(. $member)*)
            @index ($idx)
            @out ($out)
            @out_lifetimes ($($($out_lt)*)*)
            @out_generics ($($($out_gen)*)*)
            );
    };

    (@name ($name:ident)
     @lifetimes ($($lt:lifetime)*)
     @generics ($($gen:ident)*)
     @member ($($member:tt)*)
     @index ($index:ident)
     @out ($out:ident)
     @out_lifetimes ($($out_lt:lifetime)*)
     @out_generics ($($out_gen:ident)*)
     ) => {
        impl <$($lt,)* $($gen,)*> $name <$($lt,)* $($gen,)*>{
            pub fn len(&self) -> usize{
                self$($member)*.len()
            }

            pub fn iter(&self) -> impl std::iter::Iterator<Item = &$out<$($out_lt,)* $($out_gen)*>>{
                self$($member)*.iter()
            }

            pub fn iter_mut(&mut self) -> impl std::iter::Iterator<Item = &mut $out<$($out_lt,)* $($out_gen)*>>{
                self$($member)*.iter_mut()
            }
        }

        impl <$($lt,)* $($gen,)*> std::ops::Index<$index> for $name<$($lt,)* $($gen,)*>{
            type Output = $out<$($out_lt,)* $($out_gen)*>;
            fn index(&self, idx: $index) -> &$out<$($out_lt,)* $($out_gen)*> {
                &self$($member)*[usize::from(idx)]
            }
        }

        impl <$($lt,)* $($gen,)*> std::ops::IndexMut<$index> for $name<$($lt,)* $($gen,)*>{
            fn index_mut(&mut self, idx: $index) -> &mut $out<$($out_lt,)* $($out_gen)*> {
                &mut self$($member)*[usize::from(idx)]
            }
        }


    };
}
