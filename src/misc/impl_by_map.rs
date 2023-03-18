macro_rules! impl_by_map {
    ($Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!($Trait $tt for only $Functor);
)+
    };
    ($Trait:ident (fn $method:ident($($tt:tt)*) $(with $clone:ident)?) for only $Functor:ident) => {
impl_by_map!($Trait (fn $method($($tt)*) $(with $clone)? by map) for only $Functor);
    };
    ($Trait:ident (fn $method:ident(self $(, $arg:ident: $arg_ty:ty)*) $(with $clone:ident)? by $map:ident) for only $Functor:ident) => {
impl<Lhs, Rhs: $Trait<Lhs>> $Trait<$Functor<Lhs>> for $Functor<Rhs> {
    fn $method(self $(, $arg: $arg_ty)*) -> $Functor<Lhs> {
        self $(.$clone())? .$map::<Lhs>(|rhs| rhs.$method($($arg),*))
    }
}
    };
    ($Trait:ident (fn $method:ident(&self $(, $arg:ident: $arg_ty:ty)*) $(with $clone:ident)? by $map:ident) for only $Functor:ident) => {
impl<Lhs, Rhs: $Trait<Lhs>> $Trait<$Functor<Lhs>> for $Functor<Rhs> {
    fn $method(&self $(, $arg: $arg_ty)*) -> $Functor<Lhs> {
        self $(.$clone())? .$map::<Lhs>(|rhs| rhs.$method($($arg),*))
    }
}
    }
}
pub(crate) use impl_by_map;