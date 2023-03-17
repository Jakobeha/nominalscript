use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::pin::Pin;
use std::ptr::addr_of;
use std::rc::{Rc, Weak};

use crate::analyses::scopes::{Scope, TypeScope, ValueScope};
use crate::ast::typed_nodes::{AstImportStatement, AstParameter};
use crate::diagnostics::FileLogger;

/// Raw pointer to scope which can be dereferenced and re-assigned a lifetime if we know the scope exists
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub(crate) struct RawScopePtr(*const Scope<'static>);

/// Pointer to a scope
#[derive(Debug, Clone)]
pub struct ScopePtr<'tree>(Pin<Rc<Scope<'tree>>>);

/// Weak pointer to a scope
#[derive(Debug, Clone)]
pub struct WeakScopePtr<'tree>(Weak<Scope<'tree>>);

/// Mutable reference to a scope behind a pointer
#[derive(Debug)]
pub struct ScopeMut<'a, 'tree>(Pin<&'a mut Scope<'tree>>);

impl<'tree> ScopePtr<'tree> {
    /// Create a new scope
    pub(super) fn new(parent: Option<&ScopePtr<'tree>>) -> Self {
        ScopePtr(Rc::pin(Scope::new(parent)))
    }

    /// Get a mutable reference to this scope, if this is the only active
    /// (not weak, not raw) scope pointer.
    ///
    /// *Panics* if there are other active pointers to this scope,
    /// and creating an active pointer while this is alive will *panic*
    pub fn borrow_mut(&mut self) -> ScopeMut<'_, 'tree> {
        // SAFETY: We're un-pinning and then re-pinning,
        //     and manipulation of the pinned value doesn't change its location
        let rc = unsafe { ScopePtr::unpinned_mut(self) };
        if Rc::strong_count(rc) != 1 {
            panic!("Cannot borrow_mut a scope with multiple active pointers (has {})", Rc::strong_count(rc));
        }
        debug_assert!(!rc.is_being_mutated, "rc has one reference but is being mutated before borrow_mut call, how?");
        rc.is_being_mutated = true;
        // SAFETY: There are no active Rc pointers, only Weak and raw pointers, and these ensure
        // that the value doesn't have a mutable refernence when they are upgraded.
        // Furthermore, all Weak pointers to Scope are of the same type (raw pointers don't matter)
        let mut_ = unsafe { rc_get_mut_unchecked(rc) };
        // SAFETY: See first comment
        ScopeMut(unsafe { Pin::new_unchecked(mut_) })
    }

    /// Erase the lifetime and forget that this is reference counted
    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn as_raw(this: &Self) -> RawScopePtr {
        RawScopePtr(Self::as_ptr(this) as *const Scope<'static>)
    }

    /// Downgrade to a weak scope pointer
    pub fn downgrade(&self) -> WeakScopePtr<'tree> {
        // SAFETY: We're not breaking the pin
        WeakScopePtr(unsafe { Rc::downgrade(ScopePtr::unpinned(self)) })
    }

    #[allow(clippy::wrong_self_convention)]
    fn as_ptr(this: &Self) -> *const Scope<'tree> {
        // SAFETY: We're not mutating the Rc but creating an (unpinned) pointer
        unsafe { Rc::as_ptr(Self::unpinned(this)) }
    }

    /// SAFETY: You must ensure that the inner reference remains pinned
    unsafe fn unpinned(this: &Self) -> &Rc<Scope<'tree>> {
        // SAFETY: Pin<Rc<_>> = Rc<_> but pinned
        std::mem::transmute::<&Pin<Rc<Scope<'tree>>>, &Rc<Scope<'tree>>>(&this.0)
    }

    /// SAFETY: You must ensure that the inner reference remains pinned
    unsafe fn unpinned_mut(this: &mut Self) -> &mut Rc<Scope<'tree>> {
        // SAFETY: Pin<Rc<_>> = Rc<_> but pinned
        std::mem::transmute::<&mut Pin<Rc<Scope<'tree>>>, &mut Rc<Scope<'tree>>>(&mut this.0)
    }
}

impl<'tree> PartialEq<ScopePtr<'tree>> for ScopePtr<'tree> {
    fn eq(&self, other: &ScopePtr<'tree>) -> bool {
        // SAFETY: We're not breaking the pin
        unsafe { Rc::ptr_eq(ScopePtr::unpinned(self), ScopePtr::unpinned(other)) }
    }
}

impl<'tree> Eq for ScopePtr<'tree> {}

impl<'tree> Hash for ScopePtr<'tree> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ScopePtr::as_ptr(self).hash(state)
    }
}

impl<'tree> Deref for ScopePtr<'tree> {
    type Target = Scope<'tree>;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<'tree> WeakScopePtr<'tree> {
    /// Creates an uninialized scope pointer which will always `upgrade` to `None`
    pub fn new() -> Self {
        // SAFETY: This is a null pointer, so Pin is irrelevant
        WeakScopePtr(Weak::new())
    }

    /// If the scope this points to is still alive (has at least once [ScopePtr]),
    /// this will return `Some`. Otherwise `None.
    ///
    /// *Panics* if the scope is currently being mutated
    pub fn upgrade(&self) -> Option<ScopePtr<'tree>> {
        // SAFETY: We'ren't breaking the pin
        let weak = &self.0;
        if weak.strong_count() == 0 {
            return None
        }
        // SAFETY: We just checked that the strong count is non-zero so we can read the pointer
        unsafe {
            let ptr = weak.as_ptr();
            // We can't create the Rc if it's being mutated, so we read the raw address
            if addr_of!((*ptr).is_being_mutated).read() {
                panic!("Cannot upgrade a scope while it is being mutated")
            }
        }
        // Since we just checked that the strong count is non-zero we can unwrap
        let rc = weak.upgrade().unwrap();
        // SAFETY: We just unpinned the data
        Some(ScopePtr(unsafe { Pin::new_unchecked(rc) }))
    }

    #[allow(clippy::wrong_self_convention)]
    fn as_ptr(this: &Self) -> *const Scope<'tree> {
        Weak::as_ptr(&this.0)
    }
}

impl<'tree> PartialEq<WeakScopePtr<'tree>> for WeakScopePtr<'tree> {
    fn eq(&self, other: &WeakScopePtr<'tree>) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}

impl<'tree> Eq for WeakScopePtr<'tree> {}

impl<'tree> Hash for WeakScopePtr<'tree> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        WeakScopePtr::as_ptr(self).hash(state)
    }
}

impl<'tree> PartialEq<WeakScopePtr<'tree>> for ScopePtr<'tree> {
    fn eq(&self, other: &WeakScopePtr<'tree>) -> bool {
        ScopePtr::as_ptr(self) == WeakScopePtr::as_ptr(other)
    }
}

impl<'tree> PartialEq<ScopePtr<'tree>> for WeakScopePtr<'tree> {
    fn eq(&self, other: &ScopePtr<'tree>) -> bool {
        WeakScopePtr::as_ptr(self) == ScopePtr::as_ptr(other)
    }
}

impl RawScopePtr {
    /// Null pointer
    pub const fn null() -> RawScopePtr {
        RawScopePtr(std::ptr::null())
    }

    /// Is this the null pointer?
    pub fn is_null(&self) -> bool {
        self.0.is_null()
    }

    /// Re-assign a lifetime and increment the reference count.
    ///
    /// SAFETY: You must statically know that the scope being pointed to is alive.
    /// This creates a new reference to that scope, incrementing the reference count.
    ///
    /// Note that if you call [as_raw] and then drop the pointer, it decrements the count, and so
    /// if there are no active references, the scope will be dropped calling `upgrade` will cause UB
    ///
    /// Returns `None` (not UB) if this is null.
    ///
    /// *Panics* (not UB) if the scope is currently being mutated
    pub(crate) unsafe fn upgrade<'tree>(self) -> Option<ScopePtr<'tree>> {
        if self.is_null() {
            return None
        }
        unsafe {
            // SAFETY: Rc::into_raw is equivalent to Rc::as_ptr and then mem::forget.
            // We don't call forget (letting the reference from as_raw be dropped)
            // but increment the reference count beforehand (since Rc::from_raw won't).
            // So this is equivalent to Rc::from_raw and Rc::into_raw.
            // And the pin is ok because Rc::pin trivially calls Pin::new_unchecked
            // (Rc is intrinsically Pinned since it's a pointer)
            let ptr = self.0 as *const Scope<'tree>;
            // We can't create the Rc if it's being mutated, so we read the raw address
            if addr_of!((*ptr).is_being_mutated).read() {
                panic!("Cannot upgrade a scope while it is being mutated")
            }
            Rc::increment_strong_count(ptr);
            Some(ScopePtr(Pin::new_unchecked(Rc::from_raw(ptr))))
        }
    }
}

impl<'a, 'tree> Deref for ScopeMut<'a, 'tree> {
    type Target = Scope<'tree>;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<'a, 'tree> Drop for ScopeMut<'a, 'tree> {
    fn drop(&mut self) {
        debug_assert!(self.0.is_being_mutated, "ScopeMut is being dropped without being borrowed");
        // SAFETY: We'ren't changing the pinned data's location
        unsafe { Pin::into_inner_unchecked(self.0.as_mut()) }.is_being_mutated = false;
    }
}

// We CANNOT provide DerefMut for ScopeMut, because then you could deref and call e.g. mem::swap.
// Instead we must manually delegate all of Scope's mutable fields / methods on ScopeMut.
// Fortunately, there are only a few
// SAFETY for every single one of these functions: We'ren't changing the pinned data's location
impl<'a, 'tree> ScopeMut<'a, 'tree> {
    pub fn values_mut(&mut self) -> &mut ValueScope<'tree> {
        &mut unsafe { Self::unpinned_mut(self) }.values
    }

    pub fn types_mut(&mut self) -> &mut TypeScope<'tree> {
        &mut unsafe { Self::unpinned_mut(self) }.types
    }

    pub fn set_params(&mut self, params: impl Iterator<Item=Rc<AstParameter<'tree>>>) {
        unsafe { Self::unpinned_mut(self) }.set_params(params)
    }

    pub fn add_imported(&mut self, import_stmt: AstImportStatement<'tree>, e: &mut FileLogger<'_>) {
        unsafe { Self::unpinned_mut(self) }.add_imported(import_stmt, e)
    }

    /// SAFETY: You must ensure that the inner reference remains pinned
    unsafe fn unpinned_mut(this: &mut Self) -> &mut Scope<'tree> {
        // SAFETY: Pin<&mut Scope<'tree>> = &mut &mut Scope<'tree> but pinned
        //     (note that the second &mut is important!)
        std::mem::transmute::<&mut Pin<&mut Scope<'tree>>, &mut &mut Scope<'tree>>(&mut this.0)
    }
}

/// [Rc::get_mut_unchecked] but stable
unsafe fn rc_get_mut_unchecked<T>(this: &mut Rc<T>) -> &mut T {
    // We're careful to *not* create a reference covering the "count" fields, as
    // this would conflict with accesses to the reference counts (e.g. by `Weak`).
    &mut *Rc::as_ptr(this).cast_mut()
}