/// Iterator chain macro
mod chain;
/// Iterator for [elsa::FrozenMap], since it has no builtin iteration for some reason
mod frozen_map_iter;
/// Generate impl which just calls `map` on the inner value
mod impl_by_map;
/// Helpers for iterators which are empty if a boolean is `false`
mod iter_if;
/// Concat path macro
mod mk_path;
/// Mutex which doesn't return poisoned lock
mod nice_mutex;
/// `OnceCell::with_option`
mod once_cell_with_option;
/// Lets you safely create an [Iterator] using a [std::cell::RefCell] reference, and the while the
/// iterator is alive the reference will be borrowed.
mod ref_iterator;
/// `Vec::find_remove`
mod vec_find_remove;

//noinspection RsUnusedImport (IntelliJ bug)
pub(crate) use chain::chain;
pub(crate) use frozen_map_iter::FrozenMapIter;
//noinspection RsUnusedImport (IntelliJ bug)
pub(crate) use impl_by_map::impl_by_map;
pub(crate) use iter_if::*;
//noinspection RsUnusedImport (IntelliJ bug)
pub(crate) use mk_path::mk_path;
pub use nice_mutex::NiceMutex;
pub(crate) use once_cell_with_option::*;
pub(crate) use ref_iterator::RefIterator;
pub(crate) use vec_find_remove::VecFilter;