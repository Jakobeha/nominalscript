use std::sync::{Mutex, MutexGuard, TryLockError};

/// Mutex which doesn't return poisoned lock
pub struct NiceMutex<T>(Mutex<T>);

impl<T> From<Mutex<T>> for NiceMutex<T> {
    fn from(m: Mutex<T>) -> NiceMutex<T> {
        NiceMutex(m)
    }
}

impl<T> Into<Mutex<T>> for NiceMutex<T> {
    fn into(self) -> Mutex<T> {
        self.0
    }
}

impl<T> NiceMutex<T> {
    pub fn new(t: T) -> NiceMutex<T> {
        NiceMutex(Mutex::new(t))
    }

    pub fn lock(&self) -> MutexGuard<'_, T> {
        match self.0.lock() {
            Ok(lock) => lock,
            Err(poisoned) => poisoned.into_inner()
        }
    }

    pub fn try_lock(&self) -> Option<MutexGuard<'_, T>> {
        match self.0.try_lock() {
            Ok(lock) => Some(lock),
            Err(TryLockError::Poisoned(poisoned)) => Some(poisoned.into_inner()),
            Err(TryLockError::WouldBlock) => None
        }
    }
}