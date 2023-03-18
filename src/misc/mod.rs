mod nice_mutex;
/// Iterator chain macro
mod chain;
/// Iterator for [elsa::FrozenMap], since it has no builtin iteration for some reason
mod frozen_map_iter;
/// Concat path macro
mod mk_path;
/// Generate impl which just calls `map` on the inner value
mod impl_by_map;
/// [HashMap::insert_entry] but stable and doesn't return anything
mod insert_entry;
/// `OnceCell::with_option`
mod once_cell_with_option;
/// Owned or borrowed value
mod oob;

pub use nice_mutex::NiceMutex;
pub(crate) use chain::chain;
pub(crate) use frozen_map_iter::FrozenMapIter;
pub(crate) use mk_path::mk_path;
pub(crate) use impl_by_map::impl_by_map;
pub(crate) use insert_entry::*;
pub(crate) use once_cell_with_option::*;
pub use oob::Oob;