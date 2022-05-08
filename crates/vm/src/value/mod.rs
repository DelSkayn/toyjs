//! The javascript value implementation.

#[cfg(all(not(feature = "tagged-union"), target_pointer_width = "64"))]
mod nan_tagged;
#[cfg(all(not(feature = "tagged-union"), target_pointer_width = "64"))]
pub use nan_tagged::Value;

#[cfg(any(feature = "tagged-union", not(target_pointer_width = "64")))]
mod tagged_union;
#[cfg(any(feature = "tagged-union", not(target_pointer_width = "64")))]
pub use tagged_union::Value;

#[cfg(test)]
mod test {
    use super::Value;

    #[test]
    fn convert_i32() {
        fn test_value(v: i32) {
            assert_eq!(Some(v), Value::from(v).into_int());
        }

        test_value(-1);
        test_value(1);
        test_value(0);
        test_value(i32::MAX);
        test_value(i32::MIN);
    }

    #[test]
    fn convert_f64() {
        fn test_value(v: f64) {
            assert_eq!(
                Some(v.to_bits()),
                Value::from(v).into_float().map(f64::to_bits)
            );
        }

        test_value(-1.0);
        test_value(1.0);
        test_value(0.0);
        test_value(f64::MAX);
        test_value(f64::MIN);
        test_value(f64::INFINITY);
        test_value(f64::NEG_INFINITY);
        test_value(f64::NAN);
        test_value(f64::MIN_POSITIVE);
    }
}
