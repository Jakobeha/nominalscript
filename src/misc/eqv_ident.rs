use std::hash::{Hash, Hasher};

/// Compare and hash by equivalence (semantic equality)
pub struct ByEqv<T>(pub T);

/// Compare and hash by identity (intrinsic equality)
pub struct ByIdentical<T>(pub T);

/// Type with 2 forms of equality: semantic (equivalence) and intrinsic (identity)
pub trait HasEqvIdent {
    /// Compare by equivalence (semantic equality)
    fn eqv(&self, other: &Self) -> bool;
    /// Compare by identity (intrinsic equality)
    fn identical(&self, other: &Self) -> bool;
    /// Hash by equivalence (semantic equality)
    fn hash_eqv<H: Hasher>(&self, state: &mut H);
    /// Hash by identity (intrinsic equality)
    fn hash_identical<H: Hasher>(&self, state: &mut H);
}

#[macro_export]
macro_rules! impl_has_eqv_ident_struct_body {
    ({ $($eqv_field:ident),* $(; $($ident_field:ident),* $(; $($eqv_field2:ident),* $(; $($ident_field2:ident),*)?)?)? }) => {
        fn eqv(&self, other: &Self) -> bool {
            $( self.$eqv_field == other.$eqv_field && )*
            $($($( self.$eqv_field2.eqv(&other.$eqv_field2) && )*)?)?
            true
        }

        fn identical(&self, other: &Self) -> bool {
            $( self.$eqv_field == other.$eqv_field && )*
            $($( self.$ident_field == other.$ident_field && )*
            $($( self.$eqv_field2.identical(&other.$eqv_field2) && )*
            $($( self.$ident_field2.identical(&other.$ident_field2) && )*)?)?)?
            true
        }

        fn hash_eqv<H: std::hash::Hasher>(&self, state: &mut H) {
            $( self.$eqv_field.hash(state); )*
            $($($( self.$eqv_field2.hash_eqv(state); )*)?)?
        }

        fn hash_identical<H: std::hash::Hasher>(&self, state: &mut H) {
            $( self.$eqv_field.hash(state); )*
            $($( self.$ident_field.hash(state); )*
            $($( self.$eqv_field2.hash_identical(state); )*
            $($( self.$ident_field2.hash_identical(state); )*)?)?)?
        }
    }
}

#[macro_export]
macro_rules! impl_has_eqv_ident_struct {
    ($Name:ident < $A:ident > $($tt:tt)+) => {
        impl<$A> $crate::misc::eqv_ident::HasEqvIdent for $Name<$A> {
            impl_has_eqv_ident_struct_body!($($tt)+);
        }
    };
    ($Name:ident < $a:lifetime > $($tt:tt)+) => {
        impl<$a> $crate::misc::eqv_ident::HasEqvIdent for $Name<$a> {
            impl_has_eqv_ident_struct_body!($($tt)+);
        }
    };
    ($Name:ident $($tt:tt)+) => {
        impl $crate::misc::eqv_ident::HasEqvIdent for $Name {
            impl_has_eqv_ident_struct_body!($($tt)+);
        }
    };
}

impl<T: HasEqvIdent> PartialEq for ByEqv<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eqv(&other.0)
    }
}

impl<T: HasEqvIdent> Eq for ByEqv<T> {}

impl<T: HasEqvIdent> Hash for ByEqv<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash_eqv(state)
    }
}

impl<T: HasEqvIdent> PartialEq for ByIdentical<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.identical(&other.0)
    }
}

impl<T: HasEqvIdent> Eq for ByIdentical<T> {}

impl<T: HasEqvIdent> Hash for ByIdentical<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash_identical(state)
    }
}