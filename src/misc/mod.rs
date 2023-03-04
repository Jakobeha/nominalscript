/// Wrapper for a lazily-computed value which can also detect and catch cycles
pub mod lazy;
/// Roaring bitmaps for ids (particularly node ids but any u64 value is ok)
pub mod id_maps;
mod chain;

pub(crate) use chain::chain;