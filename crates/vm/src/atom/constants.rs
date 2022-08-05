#![allow(non_snake_case)]
use super::Atom;

macro_rules! define_atom{
    ($($name:ident),*) => {
        pub static STRINGS: &'static [&'static str] = &[
            "",
            "false",
            "true",
            $(stringify!($name),)*
        ];

        pub const fn empty<'a>() -> Atom<'static,'a>{
            Atom::from_constant_id(0)
        }

        pub const fn r#false<'a>() -> Atom<'static,'a>{
            Atom::from_constant_id(1)
        }

        pub const fn r#true<'a>() -> Atom<'static,'a>{
            Atom::from_constant_id(2)
        }

        define_atom!(@cons 3 => ($($name),*));

    };
    (@cons $v:expr => ($head:ident,$($rem:tt)*)) => {
        pub const fn $head<'a>() -> Atom<'static,'a>{
            Atom::from_constant_id($v)
        }
        define_atom!(@cons $v + 1 => ($($rem)*));
    };
    (@cons $v:expr => ($head:ident))=> {
        pub const fn $head<'a>() -> Atom<'static,'a>{
            Atom::from_constant_id($v)
        }
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
    String,
    Number,
    EPSILON,
    MAX_VALUE,
    MIN_VALUE,
    NaN,
    POSITIVE_INFINITY,
    NEGATIVE_INFINITY,
    MAX_SAFE_INTEGER,
    MIN_SAFE_INTEGER,
    Boolean
);
