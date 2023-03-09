/// Wrapper for a lazily-computed value which can also detect and catch cycles
pub mod lazy;
/// Roaring bitmaps for ids (particularly node ids but any u64 value is ok)
pub mod id_maps;
/// Iterator chain macro
mod chain;
/// Concat path macro
mod path;
/// Generate impl which just calls `map` on the inner value
mod impl_by_map;
/// `OnceCell::with_option`
mod once_cell_with_option;

pub(crate) use chain::chain;
pub(crate) use path::path;
pub(crate) use impl_by_map::impl_by_map;
pub(crate) use once_cell_with_option::*;