/// A macro for running assertions which should be disabled if performance is required.
#[macro_export]
macro_rules! slow_assert {
    ($assert:expr) => {
        #[cfg(feature = "slow_assert")]
        {
            assert!($assert)
        }
    };
}

/// A macro for running assertions which should be disabled if performance is required.
#[macro_export]
macro_rules! slow_assert_ne {
    ($a:expr, $b:expr) => {
        #[cfg(feature = "slow_assert")]
        {
            assert_ne!($a, $b)
        }
    };
}

/// A macro for running assertions which should be disabled if performance is required.
#[macro_export]
macro_rules! slow_assert_eq {
    ($a:expr, $b:expr) => {
        #[cfg(feature = "slow_assert")]
        {
            assert_eq!($a, $b)
        }
    };
}
