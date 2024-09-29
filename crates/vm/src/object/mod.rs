use crate::gc::{Error, Free, Gc, Marker, OptionGc, RootState, Rooted, Trace};

mod props;
mod shape;
pub use shape::{GcShape, PropertyMap, Shape};

pub type GcObject<R> = Gc<R, Object<R>>;
pub type OptionGcObject<R> = OptionGc<R, Object<R>>;
pub struct Object<R: RootState> {
    /// The objects hidden class.
    shape: GcShape<R>,
    /// The prototype for this object.
    /// Prototypes are either an object or null,
    /// If this is none then the prototype is null.
    prototype: OptionGcObject<R>,
    // TODO: The values of the object indexed with a integer.
    slots: (),
}

unsafe impl<R: RootState> Trace for Object<R> {
    type Free<'a> = Object<Free<'a>>;
    type Rooted = Object<Rooted>;

    const NEEDS_TRACE: bool = true;

    fn trace(&self, marker: &Marker) -> Result<(), Error> {
        marker.mark(&self.shape)?;
        marker.mark_option(&self.prototype)?;
        Ok(())
    }
}
