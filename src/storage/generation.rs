/// The current generation of incremental compilation. Each time we incrementally recompile, we
/// increment this; each time we do a clean build, we reset this to 0.
///
/// After a semantic phase, we know that all semantic data from the previous phase will no longer
/// be mutated until we incrementally recompile. We take advantage of this as an optimization and,
/// in each [Id] of a semantic node in the done phase, we store the raw pointer to its node data
/// along with the current generation. This allows us to skip comparing or hashing and looking up
/// the annotation, until the generation increments, at which point
///
/// The incremental compiler does a sort of garbage collection after each **incremental removal**
/// (when we remove deleted and modified sources before compiling inserted and modified sources).
/// We *trace* each annotation then delete all annotation map storage which no longer belongs to any
/// of them. This is *not* generational and does not rely on the generation number; it's just
/// something which happens each generation.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Generation(usize);

impl Generation {
    /// A special generation which is less than even the first generation
    pub const fn outdated() -> Self {
        Self(0)
    }

    /// The first generation
    pub const fn new() -> Self {
        Self(1)
    }

    /// Whether this is the always-outdated generation
    pub fn is_always_outdated(&self) -> bool {
        self.0 == 0
    }

    /// Increment the generation
    pub fn increment(&mut self) {
        self.0 += 1;
    }
}
