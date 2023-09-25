use std::path::Path;

use yak_sitter::Node;

/// Annotation span vertex or range endpoint = file-path and byte offset.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Point<'tree> {
    /// Path to the file.
    pub path: Option<&'tree Path>,
    /// Byte offset from the start of the file.
    pub byte: usize
}

/// Byte range at a path = annotation range at a single path.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RangeAtPath<'tree> {
    /// Path to the file.
    pub path: Option<&'tree Path>,
    /// Byte offset from the start of the file to the start of the range.
    pub start_byte: usize,
    /// Byte offset from the start of the file to the end of the range.
    pub end_byte: usize
}

pub trait NodePointExt<'tree> {
    /// Point at the start (lower bound) of a node's range
    fn start_point(&self) -> Point<'tree>;
    /// Point at the end (upper bound) of a node's range
    fn end_point(&self) -> Point<'tree>;
    /// Points within the node's range
    fn range_at_path(&self) -> RangeAtPath<'tree>;
    /// Whether the node's range contains the given point
    fn contains_point(&self, point: &Point<'tree>) -> bool;
    /// Whether the node's range intersects the given line segment
    fn intersects_range_at_path(&self, range: &RangeAtPath<'tree>) -> bool;
    /// Whether the node's range touches or intersects the given line segment
    fn touches_or_intersects_range_at_path(&self, range: &RangeAtPath<'tree>) -> bool;
}

impl<'tree> NodePointExt<'tree> for Node<'tree> {
    #[inline]
    fn start_point(&self) -> Point<'tree> {
        Point {
            path: self.path(),
            byte: self.start_byte()
        }
    }

    #[inline]
    fn end_point(&self) -> Point<'tree> {
        Point {
            path: self.path(),
            byte: self.end_byte()
        }
    }

    #[inline]
    fn range_at_path(&self) -> RangeAtPath<'tree> {
        RangeAtPath {
            path: self.path(),
            start_byte: self.start_byte(),
            end_byte: self.end_byte()
        }
    }

    #[inline]
    fn contains_point(&self, point: &Point<'tree>) -> bool {
        self.path() == point.path &&
            point.byte >= self.start_byte() &&
            point.byte < self.end_byte()
    }

    #[inline]
    fn intersects_range_at_path(&self, range: &RangeAtPath<'tree>) -> bool {
        self.path() == range.path &&
            range.start_byte < self.end_byte() &&
            self.start_byte() < range.end_byte
    }

    #[inline]
    fn touches_or_intersects_range_at_path(&self, range: &RangeAtPath<'tree>) -> bool {
        self.path() == range.path &&
            range.start_byte <= self.end_byte() &&
            self.start_byte() <= range.end_byte
    }
}
