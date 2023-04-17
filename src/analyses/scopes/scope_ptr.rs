// TODO: Move this into a new crate active_rc
use std::cell::{Cell, UnsafeCell};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::rc::{Rc, Weak};

use crate::analyses::scopes::Scope;

/// Strong and (mutably) dereferencable pointer to a scope.
///
/// Only one [ActiveScopePtr] or [ActiveScopeRef] of the same pointee can exist at a time;
/// trying to create many causes a panic. Thus the pointer is safe to mutably dereference.
#[derive(Debug, Clone)]
pub struct ActiveScopePtr<'tree>(Rc<ScopePointee<'tree>>);

/// Strong and (mutably) dereferencable borrowed pointer to a scope.
///
/// Only one [ActiveScopePtr] or [ActiveScopeRef] of the same pointee can exist at a time;
/// trying to create many causes a panic. Thus the reference is safe to mutably dereference.
#[derive(Debug, Clone)]
pub struct ActiveScopeRef<'a, 'tree>(&'a Rc<ScopePointee<'tree>>);

/// Strong but not dereferencable pointer to a scope
#[derive(Debug, Clone)]
pub struct InactiveScopePtr<'tree>(Rc<ScopePointee<'tree>>);

/// Weak pointer to a scope
#[derive(Debug, Clone)]
pub struct WeakScopePtr<'tree>(Weak<ScopePointee<'tree>>);

/// Raw pointer to scope which can be dereferenced and re-assigned a lifetime if we know that the scope exists
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RawScopePtr(NonNull<ScopePointee<'static>>);

/// Scope pointer data
#[derive(Debug)]
struct ScopePointee<'tree> {
    /// The pointed-to scope. The interior value can only be accessed safelu by [ActiveScopePtr].
    scope: UnsafeCell<Scope<'tree>>,
    /// Whether there is an active pointer or reference
    is_active: Cell<bool>
}

impl<'tree> ActiveScopePtr<'tree> {
    /// Create a new scope and pointer
    pub(super) fn new(parent: Option<&InactiveScopePtr<'tree>>) -> Self {
        ActiveScopePtr(Rc::new(ScopePointee::new(Scope::new(parent), true)))
    }

    /// Convert this into an inactive scope pointer. Now you can create another active pointer.
    pub fn deactivate(self) -> InactiveScopePtr<'tree> {
        debug_assert!(self.0.is_active.get(), "ActiveScopePtr exists so the pointed value should be marked active");
        // SAFETY: we're destroying this active pointer (by converting it into an inactive one)
        self.0.is_active.set(false);
        // We want to move `self.0` to return `InactiveScopePtr(self.0)` without calling `drop`,
        // and the only way to do this seems to be by unsafely copying the inner value, then
        // calling `forget`.
        // SAFETY: We immediately forget the value so there is no real aliasing.
        let this_0 = unsafe { std::ptr::read(&self.0 as *const Rc<ScopePointee<'tree>>) };
        // Drop sets is_active to false, but we already did this, so we call mem::forget
        // We don't have to worry about the Rc's drop being ignored because we are transferring it,
        // only the ActiveScopePtr wrapper is getting dropped
        std::mem::forget(self);
        InactiveScopePtr(this_0)
    }

    /// Clone this as an inactive scope pointer.
    /// You cannot clone an active pointer because two active pointers can't exist at the same time.
    pub fn inactive_clone(&self) -> InactiveScopePtr<'tree> {
        InactiveScopePtr(self.0.clone())
    }

    /// Temporarily pretend this is an inactive pointer.
    /// This is safe, because they both have the same repr, and active pointers only have more
    /// capabilities. However, trying to activate this as an inactive pointer will **panic**.
    pub fn inactive_ref(&self) -> &InactiveScopePtr<'tree> {
        // SAFETY: Same repr and the active flag is still set
        unsafe { &*(self as *const ActiveScopePtr<'tree> as *const InactiveScopePtr<'tree>) }
    }

    /// Downgrade to a weak scope pointer
    pub fn downgrade(&self) -> WeakScopePtr<'tree> {
        WeakScopePtr(Rc::downgrade(&self.0))
    }

    //noinspection DuplicatedCode
    /// Erase the lifetime and forget that this is reference counted.
    pub(crate) fn as_raw(&self) -> RawScopePtr {
        // SAFETY: The pointer returned from as_ptr is trivially not null
        RawScopePtr(unsafe { NonNull::new_unchecked(self.as_ptr().cast::<ScopePointee<'static>>().cast_mut()) })
    }

    fn as_ptr(&self) -> *const ScopePointee<'tree> {
        Rc::as_ptr(&self.0)
    }
}

// There's no point in implementing equality between [ActiveScopePtr] because two equql pointers
// can't coexist

impl<'tree> PartialEq<InactiveScopePtr<'tree>> for ActiveScopePtr<'tree> {
    fn eq(&self, other: &InactiveScopePtr<'tree>) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<'tree> PartialEq<WeakScopePtr<'tree>> for ActiveScopePtr<'tree> {
    fn eq(&self, other: &WeakScopePtr<'tree>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> PartialEq<RawScopePtr> for ActiveScopePtr<'tree> {
    fn eq(&self, other: &RawScopePtr) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> Hash for ActiveScopePtr<'tree> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
    }
}

impl<'tree> Deref for ActiveScopePtr<'tree> {
    type Target = Scope<'tree>;

    fn deref(&self) -> &Self::Target {
        debug_assert!(self.0.is_active.get(), "ActiveScopePtr exists so the pointed value should be marked active");
        // SAFETY: We're the only active pointer, so we can safely dereference the interior value
        unsafe { &*self.0.scope.get() }
    }
}

impl<'tree> DerefMut for ActiveScopePtr<'tree> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        debug_assert!(self.0.is_active.get(), "ActiveScopePtr exists so the pointed value should be marked active");
        // SAFETY: We're the only active pointer, so we can safely dereference the interior value
        unsafe { &mut *self.0.scope.get() }
    }
}

impl<'tree> Drop for ActiveScopePtr<'tree> {
    fn drop(&mut self) {
        debug_assert!(self.0.is_active.get(), "ActiveScopePtr exists so the pointed value should be marked active");
        // SAFETY: we're destroying this active pointer (by dropping)
        self.0.is_active.set(false);
    }
}

impl<'a, 'tree> ActiveScopeRef<'a, 'tree> {
    /// Downgrade to a weak scope pointer
    pub fn downgrade(&self) -> WeakScopePtr<'tree> {
        WeakScopePtr(Rc::downgrade(self.0))
    }

    //noinspection DuplicatedCode
    /// Erase the lifetime and forget that this is reference counted.
    pub(crate) fn as_raw(&self) -> RawScopePtr {
        // SAFETY: The pointer returned from as_ptr is trivially not null
        RawScopePtr(unsafe { NonNull::new_unchecked(self.as_ptr().cast::<ScopePointee<'static>>().cast_mut()) })
    }

    fn as_ptr(&self) -> *const ScopePointee<'tree> {
        Rc::as_ptr(&self.0)
    }
}

impl<'a, 'tree> PartialEq<InactiveScopePtr<'tree>> for ActiveScopeRef<'a, 'tree> {
    fn eq(&self, other: &InactiveScopePtr<'tree>) -> bool {
        Rc::ptr_eq(self.0, &other.0)
    }
}

impl<'a, 'tree> PartialEq<WeakScopePtr<'tree>> for ActiveScopeRef<'a, 'tree> {
    fn eq(&self, other: &WeakScopePtr<'tree>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'a, 'tree> PartialEq<RawScopePtr> for ActiveScopeRef<'a, 'tree> {
    fn eq(&self, other: &RawScopePtr) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'a, 'tree> Hash for ActiveScopeRef<'a, 'tree> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
    }
}

impl<'a, 'tree> Deref for ActiveScopeRef<'a, 'tree> {
    type Target = Scope<'tree>;

    fn deref(&self) -> &Self::Target {
        debug_assert!(self.0.is_active.get(), "ActiveScopeRef exists so the pointed value should be marked active");
        // SAFETY: We're the only active pointer, so we can safely dereference the interior value
        unsafe { &*self.0.scope.get() }
    }
}

impl<'a, 'tree> DerefMut for ActiveScopeRef<'a, 'tree> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        debug_assert!(self.0.is_active.get(), "ActiveScopeRef exists so the pointed value should be marked active");
        // SAFETY: We're the only active pointer, so we can safely dereference the interior value
        unsafe { &mut *self.0.scope.get() }
    }
}

impl<'a, 'tree> Drop for ActiveScopeRef<'a, 'tree> {
    fn drop(&mut self) {
        debug_assert!(self.0.is_active.get(), "ActiveScopeRef exists so the pointed value should be marked active");
        // SAFETY: we're destroying this active pointer (by dropping)
        self.0.is_active.set(false);
    }
}

impl<'tree> InactiveScopePtr<'tree> {
    /// Create a new scope and pointer
    pub(super) fn new(parent: Option<&InactiveScopePtr<'tree>>) -> Self {
        InactiveScopePtr(Rc::new(ScopePointee::new(Scope::new(parent), false)))
    }

    /// Convert this into an active scope pointer. **Panics** if there is another active pointer.
    pub fn activate(self) -> ActiveScopePtr<'tree> {
        if self.0.is_active.replace(true) {
            panic!("Tried to activate an already active scope pointer");
        }
        // SAFETY: We just checked and there are no other active pointers
        //   We also marked that now there is another active pointer.
        //   So we can create and return the active pointer
        ActiveScopePtr(self.0)
    }

    /// Temporarily treat this as an active scope pointer. **Panics** if there is another active pointer.
    ///
    /// This is functionally the same as temporarily taking ownership of `self`, calling [Self::activate]
    /// doing some operations on the active pointer, and then calling [Self::deactivate].
    pub fn activate_ref(&self) -> ActiveScopeRef<'_, 'tree> {
        if self.0.is_active.replace(true) {
            panic!("Tried to activate an already active scope pointer");
        }
        // SAFETY: We just checked and there are no other active pointers
        //   We also marked that now there is another active pointer.
        //   So we can create and return the active reference
        ActiveScopeRef(&self.0)
    }

    /// Downgrade to a weak scope pointer
    pub fn downgrade(&self) -> WeakScopePtr<'tree> {
        WeakScopePtr(Rc::downgrade(&self.0))
    }

    //noinspection DuplicatedCode
    /// Erase the lifetime and forget that this is reference counted.
    pub(crate) fn as_raw(&self) -> RawScopePtr {
        // SAFETY: The pointer returned from as_ptr is trivially not null
        RawScopePtr(unsafe { NonNull::new_unchecked(self.as_ptr().cast::<ScopePointee<'static>>().cast_mut()) })
    }

    fn as_ptr(&self) -> *const ScopePointee<'tree> {
        Rc::as_ptr(&self.0)
    }
}

impl<'tree> PartialEq<ActiveScopePtr<'tree>> for InactiveScopePtr<'tree> {
    fn eq(&self, other: &ActiveScopePtr<'tree>) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<'tree> PartialEq<InactiveScopePtr<'tree>> for InactiveScopePtr<'tree> {
    fn eq(&self, other: &InactiveScopePtr<'tree>) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<'tree> Eq for InactiveScopePtr<'tree> {}

impl<'tree> PartialEq<WeakScopePtr<'tree>> for InactiveScopePtr<'tree> {
    fn eq(&self, other: &WeakScopePtr<'tree>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> PartialEq<RawScopePtr> for InactiveScopePtr<'tree> {
    fn eq(&self, other: &RawScopePtr) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> Hash for InactiveScopePtr<'tree> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
    }
}

impl<'tree> WeakScopePtr<'tree> {
    /// Creates an uninialized scope pointer which will always `upgrade` to `None`
    pub fn new() -> Self {
        WeakScopePtr(Weak::new())
    }

    /// If the scope this points to is still alive (has at least once [InactiveScopePtr] or [ActiveScopePtr]),
    /// this will return `Some`. Otherwise `None.
    pub fn upgrade(&self) -> Option<InactiveScopePtr<'tree>> {
        self.0.upgrade().map(InactiveScopePtr)
    }

    //noinspection DuplicatedCode
    /// Erase the lifetime and forget that this is reference counted.
    pub(crate) fn as_raw(&self) -> RawScopePtr {
        // SAFETY: The pointer returned from as_ptr is trivially not null
        RawScopePtr(unsafe { NonNull::new_unchecked(self.as_ptr().cast::<ScopePointee<'static>>().cast_mut()) })
    }

    fn as_ptr(&self) -> *const ScopePointee<'tree> {
        Weak::as_ptr(&self.0)
    }
}

impl<'tree> PartialEq<ActiveScopePtr<'tree>> for WeakScopePtr<'tree> {
    fn eq(&self, other: &ActiveScopePtr<'tree>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> PartialEq<InactiveScopePtr<'tree>> for WeakScopePtr<'tree> {
    fn eq(&self, other: &InactiveScopePtr<'tree>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> PartialEq<WeakScopePtr<'tree>> for WeakScopePtr<'tree> {
    fn eq(&self, other: &WeakScopePtr<'tree>) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}

impl<'tree> Eq for WeakScopePtr<'tree> {}

impl<'tree> PartialEq<RawScopePtr> for WeakScopePtr<'tree> {
    fn eq(&self, other: &RawScopePtr) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> Hash for WeakScopePtr<'tree> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
    }
}

impl RawScopePtr {
    /// Re-assign a lifetime and increment the reference count.
    ///
    /// SAFETY: You must statically know that the scope being pointed to is alive.
    /// This creates a new reference to that scope, incrementing the reference count.
    ///
    /// Note that if you call [as_raw] and then drop the pointer, it decrements the count, and so
    /// if there are no active references, the scope will be dropped calling `upgrade` will cause UB
    pub(crate) unsafe fn upgrade<'tree>(self) -> InactiveScopePtr<'tree> {
        // SAFETY: Rc::into_raw is equivalent to Rc::as_ptr and then mem::forget.
        // We don't call forget (letting the reference from as_raw be dropped)
        // but increment the reference count beforehand (since Rc::from_raw won't).
        // So this is equivalent to Rc::from_raw and Rc::into_raw.
        // And the pin is ok because Rc::pin trivially calls Pin::new_unchecked
        // (Rc is intrinsically Pinned since it's a pointer)
        let ptr = self.0.as_ptr().cast_const().cast::<ScopePointee<'tree>>();
        Rc::increment_strong_count(ptr);
        InactiveScopePtr(Rc::from_raw(ptr))
    }

    fn as_ptr<'tree>(&self) -> *const ScopePointee<'tree> {
        self.0.as_ptr().cast_const().cast::<ScopePointee<'tree>>()
    }
}

impl<'tree> PartialEq<ActiveScopePtr<'tree>> for RawScopePtr {
    fn eq(&self, other: &ActiveScopePtr<'tree>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> PartialEq<InactiveScopePtr<'tree>> for RawScopePtr {
    fn eq(&self, other: &InactiveScopePtr<'tree>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> PartialEq<WeakScopePtr<'tree>> for RawScopePtr {
    fn eq(&self, other: &WeakScopePtr<'tree>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'tree> ScopePointee<'tree> {
    fn new(scope: Scope<'tree>, is_active: bool) -> Self {
        ScopePointee {
            scope: UnsafeCell::new(scope),
            is_active: Cell::new(is_active),
        }
    }
}