use std::fmt::{Debug, Display};
use std::hash::Hash;

/// Trait for a semantic value which is distinguished by a name; a declaration or field.
///
/// 2 semantic values in the same scope can't have the same name
pub trait HasName<'tree> {
    /// The name type
    type Name: Debug + Display + Eq + Hash + ?Sized;
    /// Get the name
    fn name(&self) -> &'tree Self::Name;
}

#[macro_export]
macro_rules! impl_has_name {
    (<$a:lifetime> $Name:ident for $Ty:ty) => {
impl<$a> $crate::semantic::storage::HasName<$a> for $Ty {
    type Name = $Name;

    fn name(&self) -> &$a Self::Name {
        self.ident.name
    }
}
    }
}