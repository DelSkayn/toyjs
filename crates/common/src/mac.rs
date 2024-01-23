/// A macro for running assertions which should be disabled if performance is required.
#[macro_export]
macro_rules! slow_assert {
    ($($t:tt)*) => {
        #[cfg(feature = "slow_assert")]
        {
            assert!($($t)*)
        }
    };
}

/// A macro for running assertions which should be disabled if performance is required.
#[macro_export]
macro_rules! slow_assert_ne {
    ($($t:tt)*) => {
        #[cfg(feature = "slow_assert")]
        {
            assert_ne!($($t)*)
        }
    };
}

/// A macro for running assertions which should be disabled if performance is required.
#[macro_export]
macro_rules! slow_assert_eq {
    ($($t:tt)*) => {
        #[cfg(feature = "slow_assert")]
        {
            assert_eq!($($t)*)
        }
    };
}

#[macro_export]
macro_rules! print_backtrace {
    () => {
        eprintln!("{}", ::std::backtrace::Backtrace::force_capture());
    };
}
