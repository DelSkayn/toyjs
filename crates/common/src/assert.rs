/// An assertion used in places where the assert would introduce minimal overhead,
/// For instance, size checks, alignment checks etc.
///
/// t stands for toyjs
#[cfg(feature = "no_assert")]
#[macro_export]
macro_rules! tassert {
    ($a:expr$(,$($t:tt)*)?) => {};
}

/// An assertion used in places where the assert would introduce minimal overhead,
/// For instance, size checks, alignment checks etc.
///
/// t stands for toyjs
#[cfg(not(feature = "no_assert"))]
#[macro_export]
macro_rules! tassert {
    ($a:expr$(,$($t:tt)*)?) => {
        assert!($a $(,$($t)*)?)
    };
}

/// An assertion used in places where the assert would introduce minimal overhead,
/// For instance, size checks, alignment checks etc.
///
/// t stands for toyjs
#[cfg(feature = "no_assert")]
macro_rules! tassert_eq {
    ($a:expr,$b:expr,$($t:tt)*) => {};
}

/// An assertion used in places where the assert would introduce minimal overhead,
/// For instance, size checks, alignment checks etc.
///
/// t stands for toyjs
#[cfg(not(feature = "no_assert"))]
#[macro_export]
macro_rules! tassert_eq {
    ($a:expr,$b:expr$(,$($t:tt)*)?) => {
        assert_eq!($a,$b$(, $($t)*)?)
    };
}

/// Assertion for slow checks, which might require more work or are in critical paths.
#[cfg(feature = "slow_assert")]
macro_rules! sassert {
    ($a:expr,$($t:tt)*) => {
        assert!($a,$($t)*)
    };
}

/// Assertion for slow checks, which might require more work or are in critical paths.
#[cfg(not(feature = "slow_assert"))]
#[macro_export]
macro_rules! sassert {
    ($a:expr$(,$($t:tt)*)?) => {};
}

/// An assertion used in places where the assert would introduce minimal overhead,
/// For instance, size checks, alignment checks etc.
///
/// t stands for toyjs
#[cfg(feature = "slow_assert")]
macro_rules! sassert_eq {
    ($a:expr,$b:expr,$($t:tt)*) => {
        assert_eq!($a,$b,$($t)*)
    };
}

/// An assertion used in places where the assert would introduce minimal overhead,
/// For instance, size checks, alignment checks etc.
///
/// t stands for toyjs
#[cfg(not(feature = "slow_assert"))]
#[macro_export]
macro_rules! sassert_eq {
    ($a:expr,$b:expr,$($t:tt)*) => {};
}

/// An assertion used in places where the assert would introduce minimal overhead,
/// For instance, size checks, alignment checks etc.
///
/// t stands for toyjs
#[cfg(feature = "slow_assert")]
macro_rules! sassert_ne {
    ($a:expr,$b:expr,$($t:tt)*) => {
        assert_ne!($a,$b,$($t)*)
    };
}

/// An assertion used in places where the assert would introduce minimal overhead,
/// For instance, size checks, alignment checks etc.
///
/// t stands for toyjs
#[cfg(not(feature = "slow_assert"))]
#[macro_export]
macro_rules! sassert_ne {
    ($a:expr,$b:expr$(,$($t:tt)*)?) => {};
}
