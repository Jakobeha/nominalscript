#![allow(unused)]

/// Iterator chain macro
mod chain;
/// Safely downcast types with lifetimes
mod downcast_with_lifetime;
/// Type with 2 forms of equality: semantic (equivalence) and intrinsic (identity)
mod eqv_ident;
/// Display with context like indentation
mod fmt_with_ctx;
/// Generate impl which just calls `map` on the inner value
mod impl_by_map;
/// Helpers for iterators which are empty if a boolean is `false`
mod iter_if;
/// Returns `true` if both iterators return equivalent values (including the same amount)
mod iter_eq;
/// Concat path macro
mod mk_path;
/// Mutex which doesn't return poisoned lock
mod nice_mutex;
/// `OnceCell::with_option`
mod once_cell_with_option;
/// `patricia_tree` with path keys
mod path_patricia_map;
/// [Rc::unwrap_or_clone] but stable
mod rc_unwrap_or_clone;
/// Lets you safely create an [Iterator] using a [std::cell::RefCell] reference, and the while the
/// iterator is alive the reference will be borrowed.
mod ref_iterator;
/// Filter an error in an empty result, e.g. to remove "already exists" I/O errors
mod result_filter_err;
/// Get either value of a result where both branches are the same
mod result_either;
/// `Vec::extend_no_dup`
mod vec_extend_no_dup;
/// `Vec::find_remove`
mod vec_find_remove;
/// Offset a utf-8 error
mod utf8_error_offset_by;

pub(crate) use chain::*;
pub(crate) use downcast_with_lifetime::*;
pub use eqv_ident::*;
pub use fmt_with_ctx::*;
pub(crate) use impl_by_map::*;
pub(crate) use iter_if::*;
pub(crate) use iter_eq::*;
pub(crate) use mk_path::*;
pub use nice_mutex::*;
pub(crate) use once_cell_with_option::*;
pub(crate) use path_patricia_map::*;
pub(crate) use rc_unwrap_or_clone::*;
pub(crate) use ref_iterator::*;
pub(crate) use result_filter_err::*;
pub(crate) use result_either::*;
pub(crate) use vec_extend_no_dup::*;
pub(crate) use vec_find_remove::*;
pub(crate) use utf8_error_offset_by::*;