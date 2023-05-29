use std::path::Path;

use yak_sitter::Node;

/// Annotation span vertex or range endpoint = file-path and byte offset.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Point<'tree> {
    /// Path to the file
    pub path: Option<&'tree Path>,
    /// Byte offset from the start of the file
    pub byte: usize
}

pub trait NodePointExt<'tree> {
    /// Point at the start (lower bound) of a node's range
    fn start_point(&self) -> Point<'tree>;
    /// Point at the end (upper bound) of a node's range
    fn end_point(&self) -> Point<'tree>;
    /// Points within the node's range
    #[inline]
    fn point_range(&self) -> std::ops::Range<Point<'tree>> {
        self.start_point()..self.end_point()
    }
    /// Whether the node's range contains the given point
    #[inline]
    fn contains_point(&self, point: Point<'tree>) -> bool {
        self.start_point() <= point && point < self.end_point()
    }
    /// Whether the node's range intersects the given range
    #[inline]
    fn intersects_range(&self, range: std::ops::Range<Point<'tree>>) -> bool {
        range.start < self.end_point() && self.start_point() < range.end
    }
    /// Whether the node's range touches or intersects the given range
    #[inline]
    fn touches_or_intersects_range(&self, range: std::ops::Range<Point<'tree>>) -> bool {
        range.start <= self.end_point() && self.start_point() <= range.end
    }
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
}
