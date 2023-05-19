/// Trait for a value which has an automatically-accessible key
pub trait HasName<Name: Copy> {
    fn name(&self) -> &Name;
}

#[macro_export]
macro_rules! impl_has_name {
    ($(($($a:lifetime),*))? $Name:ty for $Ty:ty) => {
impl$(<$($a),*>)? HasName<$Name> for $Ty {
    fn name(&self) -> &$Name {
        &self.name
    }
}
    }
}