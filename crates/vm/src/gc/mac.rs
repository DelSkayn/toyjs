macro_rules! root_owned {
    ($arena:expr, $place:expr, $value:expr) => {
        let __tmp = unsafe { $crate::gc::Trace::free_root($value) };
        $crate::gc::Arena::root_owned($arena, $place, __tmp);
    };
}

pub(crate) use root_owned;
