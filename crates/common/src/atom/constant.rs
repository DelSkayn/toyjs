#![allow(non_upper_case_globals)]
use super::{Atom, Atoms};

macro_rules! define_atom{
    ($($name:ident),*) => {
        pub(super) fn init_constants(atoms: &mut Atoms){
            atoms.atomize_string("false");
            atoms.atomize_string("true");
            define_atom!(@init atoms => ($($name),*));
        }

        pub const r#false: Atom = Atom::from_string_idx(0);
        pub const r#true: Atom = Atom::from_string_idx(1);

        define_atom!(@cons 2 => ($($name),*));

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
    create,
    defineProperties,
    defineProperty,
    freeze,
    seal,
    isFrozen,
    isSealed,
    Array,
    eval,
    String
);
