mod nice_mutex;
/// Iterator chain macro
mod chain;
/// Iterator for [elsa::FrozenMap], since it has no builtin iteration for some reason
mod frozen_map_iter;
/// Concat path macro
mod mk_path;
/// Generate impl which just calls `map` on the inner value
mod impl_by_map;
/// `OnceCell::with_option`
mod once_cell_with_option;
/// Returns the element if the condition is met, otherwise empty
mod once_if;
/// `Vec::find_remove`
mod vec_find_remove;

pub use nice_mutex::NiceMutex;
//noinspection RsUnusedImport (IntelliJ bug)
pub(crate) use chain::chain;
pub(crate) use frozen_map_iter::FrozenMapIter;
//noinspection RsUnusedImport (IntelliJ bug)
pub(crate) use mk_path::mk_path;
//noinspection RsUnusedImport (IntelliJ bug)
pub(crate) use impl_by_map::impl_by_map;
pub(crate) use once_cell_with_option::*;
//noinspection RsUnusedImport (IntelliJ bug)
pub(crate) use once_if::once_if;
pub(crate) use vec_find_remove::VecFilter;