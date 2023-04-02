use std::rc::Rc;

/// [Rc::unwrap_or_clone] but stable
pub fn rc_unwrap_or_clone<T: Clone>(rc: Rc<T>) -> T {
    Rc::try_unwrap(rc).unwrap_or_else(|rc| rc.as_ref().clone())
}