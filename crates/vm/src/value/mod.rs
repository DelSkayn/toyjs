//! The javascript value implementation.

#[cfg(all(not(feature = "tagged-union"), target_pointer_width = "64"))]
mod nan_tagged;
#[cfg(all(not(feature = "tagged-union"), target_pointer_width = "64"))]
pub use nan_tagged::Value;

#[cfg(any(feature = "tagged-union", not(target_pointer_width = "64")))]
mod tagged_union;
#[cfg(any(feature = "tagged-union", not(target_pointer_width = "64")))]
pub use tagged_union::Value;

//mod tagged_union;
//pub use tagged_union::Value;

#[cfg(test)]
mod test {
    use super::Value;

    macro_rules! test_int {
        ($v:expr) => {
            let a = Some($v as i32);
            let b = Value::from($v).into_int();
            assert_eq!(a, b, stringify!($v));
        };
    }

    #[test]
    fn convert_i32() {
        test_int!(-1);
        test_int!(1);
        test_int!(0);
        test_int!(i32::MAX);
        test_int!(i32::MIN);
    }

    macro_rules! test_float {
        ($v:expr) => {
            let a = Some(($v as f64).to_bits());
            let b = Value::ensure_float($v).into_float().map(f64::to_bits);
            assert_eq!(a, b, stringify!($v));
        };
    }

    #[test]
    fn convert_f64() {
        test_float!(-1.0);
        test_float!(1.0);
        test_float!(0.0);
        test_float!(f64::MAX);
        test_float!(f64::MIN);
        test_float!(f64::INFINITY);
        test_float!(f64::NEG_INFINITY);
        test_float!(f64::NAN);
        test_float!(f64::MIN_POSITIVE);
    }
}
