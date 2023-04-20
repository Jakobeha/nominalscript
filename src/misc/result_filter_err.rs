/// Filter an error in an empty result, e.g. to remove "already exists" I/O errors
pub trait ResultFilterErr<E> {
    /// Filter an error in an empty result, e.g. to remove "already exists" I/O errors
    fn filter_err(self, f: impl FnOnce(&mut E) -> bool) -> Self;
}

impl<E> ResultFilterErr<E> for Result<(), E> {
    #[inline]
    fn filter_err(self, f: impl FnOnce(&mut E) -> bool) -> Self {
        match self {
            Ok(()) => Ok(()),
            Err(mut e) => {
                if f(&mut e) {
                    Err(e)
                } else {
                    Ok(())
                }
            }
        }
    }
}