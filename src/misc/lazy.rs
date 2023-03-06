use std::rc::Rc;
use derive_more::{Display, Error};
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

pub struct LazyDeref<'a, T>(MappedRwLockReadGuard<'a, RwLock<LazyState<T, F>>, T>)

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

    pub fn get(&self) -> Result<LazyDeref<'a, T>, LazyError> {
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
            return Ok(result);
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

    pub fn map<U, G: FnOnce(T) -> U>(&self, transform: G) -> Lazy<U, impl FnOnce() -> Result<U, LazyError> + '_> {
        Lazy::new(|| self.get().map(|x| transform(&*x)))
    }

    pub fn and_then<U, G: FnOnce(T) -> Result<U, LazyError>>(&self, transform: G) -> Lazy<U, impl FnOnce() -> Result<U, LazyError> + '_> {
        Lazy::new(|| self.get().and_then(|x| transform(&*x)))
    }
}

// impl<'a, T> Lazy<T, Box<dyn FnOnce() -> Result<T, LazyError> + 'a>>
impl<'a, T> DynLazy<'a, T> {
    fn into_static(self) -> Result<DynLazy<'static, T>, LazyError> {
        Ok(DynLazy::immediate(self.get()?))
    }
}