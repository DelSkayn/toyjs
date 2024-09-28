use crate::gc::{Error, Free, Gc, Marker, RootState, Rooted, Trace};

mod props;
mod shape;
pub use shape::{GcShape, PropertyMap, Shape};

pub type GcObject<R> = Gc<R, Object<R>>;
pub struct Object<R: RootState> {
    // The objects hidden class.
    shape: GcShape<R>,
    // The prototype for this object.
    prototype: Option<GcObject<R>>,
    // TODO: The values of the object indexed with a integer.
    slots: (),
}

unsafe impl<R: RootState> Trace for Object<R> {
    type Free<'a> = Object<Free<'a>>;
    type Rooted = Object<Rooted>;

    const NEEDS_TRACE: bool = true;

    fn trace(&self, marker: &Marker) -> Result<(), Error> {
        marker.mark(&self.shape)?;
        if let Some(x) = self.prototype.as_ref() {
            marker.mark(x)?;
        }
        Ok(())
    }
}
