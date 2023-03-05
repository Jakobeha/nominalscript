use std::ops::Deref;
use parking_lot::{RwLock};
use derive_more::{Display, Error};
use parking_lot::lock_api::RwLockReadGuard;
use replace_with::replace_with_and_return;

#[derive(Debug, Display, Error)]
pub enum LazyError {
    #[display(fmt = "cycle detected")]
    CycleDetected,
    #[display(fmt = "force panicked")]
    ForcePanicked,
}

pub struct Lazy<T, Origin> {
    state: RwLock<LazyState<T, Origin>>
}

enum LazyState<T, Origin> {
    Value { value: T },
    IsBeingForced,
    ForcePanicked,
    Thunk {
        origin: Origin,
        compute: fn(Origin) -> Result<T, LazyError>
    },
}

impl<T, Origin> Lazy<T, Origin> {
    pub fn new(origin: Origin, compute: fn(Origin) -> Result<T, LazyError>) -> Lazy<T, Origin> {
        Lazy { state: RwLock::new(LazyState::Thunk { origin, compute }) }
    }

    pub fn immediate(value: T) -> Lazy<T, Origin> {
        Lazy { state: RwLock::new(LazyState::Value { value }) }
    }

    pub fn get(&self) -> Result<impl Deref<Target=T> + Send + Sync, LazyError> {
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
            let LazyState::Thunk { origin, compute } = lock else {
                return (None, lock);
            };
            (Some(|| compute(origin)), LazyState::IsBeingForced)
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

    pub fn and_then<U, AuxOrigin>(
        self,
        aux_origin: AuxOrigin,
        new_compute: fn(T, AuxOrigin) -> Result<U, LazyError>
    ) -> Lazy<U, (Origin, fn(Origin) -> Result<T, LazyError>, AuxOrigin)> {
        Lazy::new((self.state, self.compute, aux_origin), |(origin, compute, aux_origin)| {
            let value = compute(origin)?;
            new_compute(value, aux_origin)
        })
    }
}

// impl<'a, T> Lazy<T, Box<dyn FnOnce() -> Result<T, LazyError> + 'a>>
impl<'a, T> DynLazy<'a, T> {
    fn into_static(self) -> Result<DynLazy<'static, T>, LazyError> {
        Ok(DynLazy::immediate(self.get()?))
    }
}