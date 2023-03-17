mod nice_mutex;
/// Roaring bitmaps for ids (particularly node ids but any u64 value is ok)
pub mod id_maps;
/// Iterator chain macro
mod chain;
/// Concat path macro
mod mk_path;
/// Generate impl which just calls `map` on the inner value
mod impl_by_map;
/// `OnceCell::with_option`
mod once_cell_with_option;

pub use nice_mutex::NiceMutex;
pub(crate) use chain::chain;
pub(crate) use mk_path::mk_path;
pub(crate) use impl_by_map::impl_by_map;
pub(crate) use once_cell_with_option::*;