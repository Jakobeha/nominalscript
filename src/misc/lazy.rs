use std::rc::Rc;
use derive_more::{Deref, Display, Error};
use parking_lot::lock_api::RwLockReadGuard;
use parking_lot::{MappedRwLockReadGuard, RwLock};
use replace_with::replace_with_and_return;

#[derive(Debug, Display, Error)]
pub enum LazyError {
    #[display(fmt = "cycle detected")]
    CycleDetected,
    #[display(fmt = "force panicked")]
    ForcePanicked,
}

pub struct Lazy<T, F: FnOnce() -> Result<T, LazyError>> {
    state: RwLock<LazyState<T, F>>
}

pub trait LazyTrait<T> {
    fn get(&self) -> Result<LazyDeref<'_, T>, LazyError>;
}

#[derive(Debug, Deref)]
#[deref(forward)]
pub struct LazyDeref<'a, T>(MappedRwLockReadGuard<'a, T>);

pub type RcLazy<T> = Rc<dyn LazyTrait<T>>;

enum LazyState<T, F: FnOnce() -> Result<T, LazyError>> {
    Value { value: T },
    IsBeingForced,
    ForcePanicked,
    Thunk { thunk: F },
}

impl<T, F: FnOnce() -> Result<T, LazyError>> Lazy<T, F> {
    pub fn new(thunk: F) -> Lazy<T, F> {
        Lazy { state: RwLock::new(LazyState::Thunk { thunk }) }
    }

    pub fn immediate(value: T) -> Lazy<T, F> {
        Lazy { state: RwLock::new(LazyState::Value { value }) }
    }

    pub fn map<'a, U: 'a, G: FnOnce(T) -> U + 'a>(self, transform: G) -> Lazy<U, impl FnOnce() -> Result<U, LazyError> + 'a> {
        Lazy::new(move || self.get().map(|x| transform(&*x)))
    }

    pub fn and_then<'a, U: 'a, G: FnOnce(T) -> Result<U, LazyError> + 'a>(self, transform: G) -> Lazy<U, impl FnOnce() -> Result<U, LazyError> + 'a> {
        Lazy::new(move || self.get().and_then(|x| transform(&*x)))
    }

    pub fn map_ref<'a, U: 'a, G: FnOnce(T) -> U + 'a>(&'a self, transform: G) -> Lazy<U, impl FnOnce() -> Result<U, LazyError> + 'a> {
        Lazy::new(move || self.get().map(|x| transform(&*x)))
    }

    pub fn and_then_ref<'a, U: 'a, G: FnOnce(T) -> Result<U, LazyError> + 'a>(&'a self, transform: G) -> Lazy<U, impl FnOnce() -> Result<U, LazyError> + 'a> {
        Lazy::new(move || self.get().and_then(|x| transform(&*x)))
    }
}

impl<T, F: FnOnce() -> Result<T, LazyError>> LazyTrait<T> for Lazy<T, F> {
    fn get(&self) -> Result<LazyDeref<'_, T>, LazyError> {
        let mut error = None;
        if let Some(result) = RwLockReadGuard::try_map(self.state.read(), |state| match state {
            LazyState::Value { value } => Some(value),
            LazyState::IsBeingForced => {
                error = Some(LazyError::CycleDetected);
                None
            }
            LazyState::ForcePanicked => {
                error = Some(LazyError::ForcePanicked);
                None
            }
            // Need to get a write lock to force
            LazyState::Thunk { .. } => None
        }) {
            return Ok(LazyDeref(result));
        } else if let Some(error) = error {
            return Err(error);
        }
        // Make sure we don't try to force the thunk while we're holding the lock
        // Also it's possible that in between releasing the read lock and aquiring the write lock
        // that the value has or being forced: this is handled appropriately as force() just
        // leaves the state unless it's a thunk
        self.force()?;
        // We can't just use force to return the value because we want it to be the same type,
        // which is a mapped read lock value (stable deref >> deref with branches)
        self.get()
    }
}

impl<T, F: FnOnce() -> Result<T, LazyError>> Lazy<T, F> {
    /// Immediately sets this to resolved.
    /// If the lazy was already forced returns the old value, otherwise does not force and returns `None`.
    /// If the lazy was in a cycle or panic it will no longer be and return `None`.
    pub fn replace(&self, value: T) -> Option<T> {
        replace_with_and_return(&mut *self.state.write(), LazyState::ForcePanicked, |lock| {
            let old_value = match lock {
                LazyState::Value { value: old_value } => Some(old_value),
                _ => None
            };
            (old_value, LazyState::Value { value })
        })
    }

    fn force(&self) -> bool {
        let Some(thunk) = replace_with_and_return(&mut *self.state.write(), LazyState::ForcePanicked, |lock| {
            let LazyState::Thunk { thunk } = lock else {
                return (None, lock);
            };
            (Some(thunk), LazyState::IsBeingForced)
        }) else {
            // Already forced or is being forced
            return false
        };
        // Importantly we have the lock released when we force,
        // so that we get cycle-detected and not a deadlock
        let value = thunk();
        *self.state.write() = match value {
            Ok(value) => LazyState::Value { value },
            // Will cause subsequent calls to get to return an "cycle detected",
            // which is what we intend even though the cycle as passed
            Err(LazyError::CycleDetected) => LazyState::IsBeingForced,
            Err(LazyError::ForcePanicked) => LazyState::ForcePanicked,
        };
        true
    }
}