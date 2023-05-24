use std::hash::Hash;

/// Trait for a semantic value which is distinguished by a name; a declaration or field.
/// 2 semantic values in the same scope can't have the same name
pub trait HasName<'tree>: HasAnn<'tree> {
    /// The name type
    type Name: Eq + Hash;
    /// Get the name
    fn name(&self) -> &'tree Self::Name;
}

macro_rules! impl_has_name {
    ($(($($a:lifetime),*))? $Name:ty for $Ty:ty) => {
impl$(<$($a),*>)? HasName for $Ty {
    type Name = $Name;

    fn name(&self) -> &Self::Name {
        &self.name
    }
}
    }
}
pub(crate) use impl_has_name;
use crate::semantic::ann::HasAnn;