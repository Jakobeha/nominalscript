/// Get either value of a result where both branches are the same
pub trait ResultEitherExt<T> {
    /// Get either value of a result where both branches are the same
    fn either(self) -> T;
}

impl<T> ResultEitherExt<T> for Result<T, T> {
    fn either(self) -> T {
        match self {
            Ok(x) => x,
            Err(x) => x
        }
    }
}