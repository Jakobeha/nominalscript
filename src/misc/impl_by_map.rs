macro_rules! impl_by_map {
    (<$a:lifetime, $Lhs:ident : $Bound1:path | $Bound1a:path, $Rhs:ident : $Bound2:path | $Bound2a:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<$a, $Lhs: $Bound1 | $Bound1a, $Rhs: $Bound2 | $Bound2a> $Trait $tt for only $Functor);
)+
    };
    (<$a:lifetime, $Lhs:ident : $Bound1:path, $Rhs:ident : $Bound2:path | $Bound2a:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<$a, $Lhs: $Bound1, $Rhs: $Bound2 | $Bound2a> $Trait $tt for only $Functor);
)+
    };
    (<$a:lifetime, $Lhs:ident : $Bound1:path, $Rhs:ident : $Bound2:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<$a, $Lhs: $Bound1, $Rhs: $Bound2> $Trait $tt for only $Functor);
)+
    };
    (<$a:lifetime, $Bound1:path, $Bound2:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<$a, Lhs: $Bound1, Rhs: $Bound2> $Trait $tt for only $Functor);
)+
    };
    (<$a:lifetime, $Bound:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<$a, Lhs: $Bound, Rhs: $Bound> $Trait $tt for only $Functor);
)+
    };
    (<$a:lifetime> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<$a, Lhs, Rhs> $Trait $tt for only $Functor);
)+
    };
    (<$Lhs:ident : $Bound1:path | $Bound1a:path, $Rhs:ident : $Bound2:path | $Bound2a:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<$Lhs: $Bound1 | $Bound1a, $Rhs: $Bound2 | $Bound2a> $Trait $tt for only $Functor);
)+
    };
    (<$Lhs:ident : $Bound1:path, $Rhs:ident : $Bound2:path | $Bound2a:path> $Trait:ident $tt:tt for $($Functor:ident),+) => {
$(
impl_by_map!(<$Lhs: $Bound1, $Rhs: $Bound2 | $Bound2a> $Trait $tt for only $Functor);
)+
    };
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
    (<$a:lifetime, $Lhs:ident $(: $Bound1:path $(| $Bound1s:path)*)?, $Rhs:ident $(: $Bound2:path $(| $Bound2s:path)*)?> $Trait:ident (fn $method:ident($($tt:tt)*) $(with $clone:ident)?) for only $Functor:ident) => {
impl_by_map!(<$a, $Lhs $(0 $(+ $Bound1s)*: $Bound1)?, $Rhs $(: $Bound2 $(+ $Bound2s)*)?> $Trait (fn $method($($tt)*) $(with $clone)? by map) for only $Functor);
    };
    (<$a:lifetime, $Lhs:ident $(: $Bound1:path $(| $Bound1s:path)*)?, $Rhs:ident $(: $Bound2:path $(| $Bound2s:path)*)?> $Trait:ident (fn $method:ident(self $(, $arg:ident: $arg_ty:ty)*) $(with $clone:ident)? by $map:ident) for only $Functor:ident) => {
impl<$a, $Lhs $(: $Bound1 $(+ $Bound1s)*)?, $Rhs: $Trait<$a, $Lhs> $(+ $Bound2 $(+ $Bound2s)*)?> $Trait<$a, $Functor<$a, $Lhs>> for $Functor<$a, $Rhs> {
    fn $method(self $(, $arg: $arg_ty)*) -> $Functor<$a, $Lhs> {
        self $(.$clone())? .$map::<$Lhs>(|rhs| rhs.$method($($arg),*))
    }
}
    };
    (<$a:lifetime, $Lhs:ident $(: $Bound1:path $(| $Bound1s:path)*)?, $Rhs:ident $(: $Bound2:path $(| $Bound2s:path)*)?> $Trait:ident (fn $method:ident(&self $(, $arg:ident: $arg_ty:ty)*) $(with $clone:ident)? by $map:ident) for only $Functor:ident) => {
impl<$a, $Lhs $(: $Bound1 $(+ $Bound1s)*)?, $Rhs: $Trait<$a, $Lhs> $(+ $Bound2 $(+ $Bound2s)*)?> $Trait<$a, $Functor<$a, $Lhs>> for $Functor<$a, $Rhs> {
    fn $method(&self $(, $arg: $arg_ty)*) -> $Functor<$a, $Lhs> {
        self $(.$clone())? .$map::<$Lhs>(|rhs| rhs.$method($($arg),*))
    }
}
    };
    (<$a:lifetime, $Lhs:ident $(: $Bound1:path $(| $Bound1s:path)*)?, $Rhs:ident $(: $Bound2:path $(| $Bound2s:path)*)?> $Trait:ident (type $Assoc:ident) for only $Functor:ident) => {
impl<$a, $Lhs $(: $Bound1 $(+ $Bound1s)*)?, $Rhs: $Trait<$a, $Assoc = $Lhs> $(+ $Bound2 $(+ $Bound2s)*)?> $Trait for $Functor<$a, $Rhs> {
    type $Assoc = $Functor<$a, $Lhs>;
}
    };
    (<$Lhs:ident $(: $Bound1:path $(| $Bound1s:path)*)?, $Rhs:ident $(: $Bound2:path $(| $Bound2s:path)*)?> $Trait:ident (fn $method:ident($($tt:tt)*) $(with $clone:ident)?) for only $Functor:ident) => {
impl_by_map!(<$Lhs $(0 $(+ $Bound1s)*: $Bound1)?, $Rhs $(: $Bound2 $(+ $Bound2s)*)?> $Trait (fn $method($($tt)*) $(with $clone)? by map) for only $Functor);
    };
    (<$Lhs:ident $(: $Bound1:path $(| $Bound1s:path)*)?, $Rhs:ident $(: $Bound2:path $(| $Bound2s:path)*)?> $Trait:ident (fn $method:ident(self $(, $arg:ident: $arg_ty:ty)*) $(with $clone:ident)? by $map:ident) for only $Functor:ident) => {
impl<$Lhs $(: $Bound1 $(+ $Bound1s)*)?, $Rhs: $Trait<$Lhs> $(+ $Bound2 $(+ $Bound2s)*)?> $Trait<$Functor<$Lhs>> for $Functor<$Rhs> {
    fn $method(self $(, $arg: $arg_ty)*) -> $Functor<$Lhs> {
        self $(.$clone())? .$map::<$Lhs>(|rhs| rhs.$method($($arg),*))
    }
}
    };
    (<$Lhs:ident $(: $Bound1:path $(| $Bound1s:path)*)?, $Rhs:ident $(: $Bound2:path $(| $Bound2s:path)*)?> $Trait:ident (fn $method:ident(&self $(, $arg:ident: $arg_ty:ty)*) $(with $clone:ident)? by $map:ident) for only $Functor:ident) => {
impl<$Lhs $(: $Bound1 $(+ $Bound1s)*)?, $Rhs: $Trait<$Lhs> $(+ $Bound2 $(+ $Bound2s)*)?> $Trait<$Functor<$Lhs>> for $Functor<$Rhs> {
    fn $method(&self $(, $arg: $arg_ty)*) -> $Functor<$Lhs> {
        self $(.$clone())? .$map::<$Lhs>(|rhs| rhs.$method($($arg),*))
    }
}
    };
    (<$Lhs:ident $(: $Bound1:path $(| $Bound1s:path)*)?, $Rhs:ident $(: $Bound2:path $(| $Bound2s:path)*)?> $Trait:ident (type $Assoc:ident) for only $Functor:ident) => {
impl<$Lhs $(: $Bound1 $(+ $Bound1s)*)?, $Rhs: $Trait<$Assoc = $Lhs> $(+ $Bound2 $(+ $Bound2s)*)?> $Trait for $Functor<$Rhs> {
    type $Assoc = $Functor<$Lhs>;
}
    };
}
pub(crate) use impl_by_map;