/// Safely downcast a trait with a lifetime, by transmuting to the 'static version, downcasting, and
/// transmuting the lifetime back. The operation is safe because we statically know that the
/// lifetimes are the same, and otherwise this is equivalent to downcasting the static version.
/// We must wrap this in a macro in order to enforce that both types only differ in lifetime.
macro_rules! downcast_ref_with_lifetime {
    (($a:lifetime, $T:ident => $U:ident) $e:expr) => {
        // SAFETY: Transmuting is the same except for lifetime changes, the lifetime remains valid
        // and is the same in the downcast
        unsafe {
            let e = std::mem::transmute::<&$T<$a>, &$T<'static>>($e);
            let e = (e as &dyn std::any::Any).downcast_ref::<$U<'static>>();
            e.map(|e| std::mem::transmute::<&$U<'static>, &$U<$a>>(e))
        }
    };
    (($a:lifetime, $T:ident<$A0:ty> => $U:ident) $e:expr) => {
        // SAFETY: Transmuting is the same except for lifetime changes, the lifetime remains valid
        // and is the same in the downcast
        unsafe {
            let e = std::mem::transmute::<&$T<$a, $A0>, &$T<'static, $A0>>($e);
            let e = (e as &dyn std::any::Any).downcast_ref::<$U<'static>>();
            e.map(|e| std::mem::transmute::<&$U<'static>, &$U<$a>>(e))
        }
    }
}
pub(crate) use downcast_ref_with_lifetime;