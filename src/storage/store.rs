pub mod root_set;

/// Single structure which allocates all of the semantic data.
///
/// This is the equivalent to [rustc_arena::declare_arena](https://doc.rust-lang.org/stable/nightly-rustc/rustc_arena/macro.declare_arena.html).
pub struct Store {
    // TODO
}

/// Contains the store of the given type.
///
/// This is equivalent to [rustc_arena::ArenaAllocatable](https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_arena/lib.rs.html#571).
pub trait HasStore<'tree, T> {
    /// Get the store of the given type.
    fn inner_store(&self) -> &'tree T;
}

