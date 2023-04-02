macro_rules! impl_by_map {
    (<$Lhs:ident : $Bound1:path, $Rhs:ident : $Bound2:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<$Lhs: $Bound1, $Rhs: $Bound2> $Trait $tt for only $Functor);
)+
    };
    (<$Bound1:path, $Bound2:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<Lhs: $Bound1, Rhs: $Bound2> $Trait $tt for only $Functor);
)+
    };
    (<$Bound:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<Lhs: $Bound, Rhs: $Bound> $Trait $tt for only $Functor);
)+
    };
    ($Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<Lhs, Rhs> $Trait $tt for only $Functor);
)+
    };
    (<$Lhs:ident $(: $Bound1:path)?, $Rhs:ident $(: $Bound2:path)?> $Trait:ident (fn $method:ident($($tt:tt)*) $(with $clone:ident)?) for only $Functor:ident) => {
impl_by_map!(<$Lhs $(: $Bound1)?, $Rhs $(: $Bound2)?> $Trait (fn $method($($tt)*) $(with $clone)? by map) for only $Functor);
    };
    (<$Lhs:ident $(: $Bound1:path)?, $Rhs:ident $(: $Bound2:path)?> $Trait:ident (fn $method:ident(self $(, $arg:ident: $arg_ty:ty)*) $(with $clone:ident)? by $map:ident) for only $Functor:ident) => {
impl<$Lhs $(: $Bound1)?, $Rhs: $Trait<$Lhs> $(+ $Bound2)?> $Trait<$Functor<$Lhs>> for $Functor<$Rhs> {
    fn $method(self $(, $arg: $arg_ty)*) -> $Functor<$Lhs> {
        self $(.$clone())? .$map::<$Lhs>(|rhs| rhs.$method($($arg),*))
    }
}
    };
    (<$Lhs:ident $(: $Bound1:path)?, $Rhs:ident $(: $Bound2:path)?> $Trait:ident (fn $method:ident(&self $(, $arg:ident: $arg_ty:ty)*) $(with $clone:ident)? by $map:ident) for only $Functor:ident) => {
impl<$Lhs $(: $Bound1)?, $Rhs: $Trait<$Lhs> $(+ $Bound2)?> $Trait<$Functor<$Lhs>> for $Functor<$Rhs> {
    fn $method(&self $(, $arg: $arg_ty)*) -> $Functor<$Lhs> {
        self $(.$clone())? .$map::<$Lhs>(|rhs| rhs.$method($($arg),*))
    }
}
    }
}
pub(crate) use impl_by_map;