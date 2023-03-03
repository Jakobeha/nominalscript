use std::fmt::{Debug, Display};
use std::hash::Hash;
pub use btree_many_to_one::*;
pub use many_to_one::*;
pub use map::*;
pub use one_to_many::*;
pub(super) use roaring_map::*;
pub(super) use roaring_bidimap::*;
pub use set::*;

mod btree_many_to_one;
mod many_to_one;
mod map;
mod one_to_many;
mod roaring_map;
mod roaring_bidimap;
mod set;

pub trait Id: From<u64> + Into<u64> + Copy + Clone + PartialEq + Eq + Hash + Debug + Display {}