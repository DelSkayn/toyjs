#![allow(non_upper_case_globals)]
use super::{Atom, Atoms};

macro_rules! define_atom{
    ($($name:ident),*) => {
        pub(super) fn init_constants(atoms: &mut Atoms){
            define_atom!(@init atoms => ($($name),*));
        }

        define_atom!(@cons 0 => ($($name),*));

        #[cfg(test)]
        mod test {
            use super::{Atoms};

            #[test]
            fn constants() {
                let atoms = Atoms::new();
                $(
                    assert_eq!(atoms.atomize_string(stringify!($name)),super::$name);
                )*
            }
        }

    };
    (@cons $v:expr => ($head:ident,$($rem:tt)*)) => {
        pub const $head: Atom = Atom::from_string_idx($v);
        define_atom!(@cons $v + 1 => ($($rem)*));
    };
    (@cons $v:expr => ($head:ident))=> {
        pub const $head: Atom = Atom::from_string_idx($v);
    };
    (@init $v:expr => ($($name:ident),*)) => {
        $($v.atomize_string(stringify!($name));)*
    };
}

define_atom!(
    prototype,
    message,
    cause,
    name,
    constructor,
    toString,
    valueOf,
    Object,
    Error,
    SyntaxError,
    TypeError,
    length,
    globalThis,
    getPrototypeOf,
    is,
    isExtensible,
    assign,
    enumerable,
    configurable,
    value,
    writable,
    get,
    set,
    create
);
