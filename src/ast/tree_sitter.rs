use std::cell::RefCell;
use std::path::Path;
use std::fs;
use std::iter::{once, Once};
use std::str::Utf8Error;
use derive_more::{Display, From, Error};
use std::fmt::Write;
use std::hash::{Hash, Hasher};
use std::ops::{Range, RangeBounds};
use crate::misc::id_maps::{Id, IdSet};

#[derive(Debug)]
pub struct TSTree {
    byte_text: Vec<u8>,
    tree: tree_sitter::Tree,
    cached_data: CachedTreeData,
    marked_nodes: RefCell<IdSet<TSNodeId>>
}

#[derive(Debug)]
struct CachedTreeData {}

#[derive(Debug, Clone, Copy)]
pub struct TSNode<'tree> {
    node: tree_sitter::Node<'tree>,
    tree: &'tree TSTree,
}

#[derive(Debug, Clone, Copy, Display, PartialEq, Eq, Hash)]
pub struct TSNodeId(usize);

#[derive(Debug, Clone)]
pub struct TSCursor<'tree> {
    cursor: tree_sitter::TreeCursor<'tree>,
    tree: &'tree TSTree,
}

#[derive(Debug)]
pub struct TSQueryCursor {
    query_cursor: tree_sitter::QueryCursor
}

#[derive(Debug)]
pub struct TSQueryMatches<'query, 'tree: 'query> {
    query_matches: tree_sitter::QueryMatches<'query, 'tree, &'query TSTree>,
    tree: &'tree TSTree,
    query: &'query TSQuery
}

#[derive(Debug)]
pub struct TSQueryMatch<'query, 'tree> {
    query_match: tree_sitter::QueryMatch<'query, 'tree>,
    tree: &'tree TSTree,
    query: &'query TSQuery
}

#[derive(Debug)]
pub struct TSQueryCaptures<'query, 'tree> {
    query_captures: tree_sitter::QueryCaptures<'query, 'tree, &'query TSTree>,
    tree: &'tree TSTree,
    query: &'query TSQuery
}

#[derive(Debug, Clone, Copy)]
pub struct TSQueryCapture<'query, 'tree> {
    pub node: TSNode<'tree>,
    pub name: &'query str,
}

pub struct TSParser(tree_sitter::Parser);

pub type TSLanguage = tree_sitter::Language;
pub type TSLanguageError = tree_sitter::LanguageError;
pub type TSQuery = tree_sitter::Query;
pub type TSQueryProperty = tree_sitter::QueryProperty;
pub type TSRange = tree_sitter::Range;
pub type TSIncludedRangesError = tree_sitter::IncludedRangesError;
pub type TSPoint = tree_sitter::Point;

#[derive(Debug, Display, From, Error)]
pub enum TreeCreateError {
    IO(std::io::Error),
    LoadLanguage(tree_sitter::LanguageError),
    ParsingFailed,
    #[display(fmt = "Invalid UTF-8 at byte index {}-{}", actual_index, "error.error_len()")]
    NotUtf8 { actual_index: usize, error: Utf8Error }
}

pub type TSSubTree = TSTree;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraversalState {
    Start,
    Down,
    Right,
    Up,
    End
}

#[derive(Debug, Clone)]
pub struct PreorderTraversal<'tree> {
    cursor: TSCursor<'tree>,
    last_state: TraversalState
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraversalItem<'tree> {
    node: TSNode<'tree>,
    field_name: Option<&'static str>,
    last_state: TraversalState
}

impl TSParser {
    pub fn new(language: tree_sitter::Language) -> Result<Self, TSLanguageError> {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(language)?;
        Ok(Self(parser))
    }

    pub fn set_language(&mut self, language: TSLanguage) -> Result<(), TSLanguageError> {
        self.0.set_language(language)
    }

    pub fn set_included_ranges(&mut self, ranges: &[TSRange]) -> Result<(), TSIncludedRangesError> {
        self.0.set_included_ranges(ranges)
    }

    pub fn parse_file(&mut self, path: &Path) -> Result<TSTree, TreeCreateError> {
        self.parse_bytes(fs::read(path)?)
    }

    pub fn parse_string(&mut self, text: String) -> Result<TSTree, TreeCreateError> {
        self.parse_bytes(text.into_bytes())
    }

    pub fn parse_bytes(&mut self, byte_text: Vec<u8>) -> Result<TSTree, TreeCreateError> {
        let tree = self.0.parse(&byte_text, None).ok_or(TreeCreateError::ParsingFailed)?;
        TSTree::new(tree, byte_text)
    }
}

impl TSTree {
    fn new(tree: tree_sitter::Tree, byte_text: Vec<u8>) -> Result<Self, TreeCreateError> {
        Self::validate_utf8(&tree, &byte_text)?;
        let cached_data = CachedTreeData::new(&tree);
        Ok(Self { byte_text, tree, cached_data, marked_nodes: RefCell::new(IdSet::new()) })
    }

    fn validate_utf8(tree: &tree_sitter::Tree, byte_text: &[u8]) -> Result<(), TreeCreateError> {
        if let Err(error) = std::str::from_utf8(byte_text) {
            return Err(TreeCreateError::NotUtf8 {
                actual_index: error.valid_up_to(),
                error
            })
        }
        let mut cursor = tree.walk();
        let mut went_up = false;
        while (!went_up && cursor.goto_first_child()) ||
            cursor.goto_next_sibling() ||
            { went_up = true; cursor.goto_parent() } {
            let node = cursor.node();
            let range = node.byte_range();
            if let Err(error) = std::str::from_utf8(&byte_text[range]) {
                return Err(TreeCreateError::NotUtf8 {
                    actual_index: node.start_byte() + error.valid_up_to(),
                    error
                })
            }
        }
        Ok(())
    }

    pub fn text(&self) -> &str {
        // SAFETY: we ran validate_utf8 before constructing so the text is valid UTF-8
        unsafe { std::str::from_utf8_unchecked(&self.byte_text) }
    }

    pub fn root_node(&self) -> TSNode<'_> {
        TSNode::new(self.tree.root_node(), self)
    }

    pub fn walk(&self) -> TSCursor<'_> {
        TSCursor::new(self.tree.walk(), self)
    }
}

impl<'tree> tree_sitter::TextProvider<'tree> for &'tree TSTree {
    type I = Once<&'tree [u8]>;

    fn text(&mut self, node: tree_sitter::Node<'_>) -> Self::I {
        once(&self.byte_text[node.byte_range()])
    }
}

impl<'tree> TSNode<'tree> {
    fn new(node: tree_sitter::Node<'tree>, tree: &'tree TSTree) -> Self {
        Self { node, tree }
    }

    pub fn id(&self) -> TSNodeId {
        TSNodeId::of_ts(self.node)
    }

    pub fn kind(&self) -> &'static str {
        self.node.kind()
    }

    pub fn is_named(&self) -> bool {
        self.node.is_named()
    }

    pub fn start_byte(&self) -> usize {
        self.node.start_byte()
    }

    pub fn end_byte(&self) -> usize {
        self.node.end_byte()
    }

    pub fn start_point(&self) -> TSPoint {
        self.node.start_point()
    }

    pub fn end_point(&self) -> TSPoint {
        self.node.end_point()
    }

    pub fn byte_range(&self) -> Range<usize> {
        self.node.byte_range()
    }

    pub fn range(&self) -> TSRange {
        self.node.range()
    }

    fn byte_text(&self) -> &[u8] {
        &self.tree.byte_text[self.byte_range()]
    }

    pub fn text(&self) -> &str {
        // SAFETY: we ran validate_utf8 before constructing so all nodes are valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.byte_text()) }
    }

    pub fn all_children(&self, cursor: &mut TSCursor<'tree>) -> impl Iterator<Item = TSNode<'tree>> {
        self.node.children(&mut cursor.cursor).map(move |node| TSNode::new(node, self.tree))
    }

    pub fn named_children(&self, cursor: &mut TSCursor<'tree>) -> impl Iterator<Item = TSNode<'tree>> {
        self.node.named_children(&mut cursor.cursor).map(move |node| TSNode::new(node, self.tree))
    }

    pub fn any_child_count(&self) -> usize {
        self.node.child_count()
    }

    pub fn named_child_count(&self) -> usize {
        self.node.named_child_count()
    }

    pub fn parent(&self) -> Option<TSNode<'tree>> {
        self.node.parent().map(|node| TSNode::new(node, self.tree))
    }

    pub fn next_any_sibling(&self) -> Option<TSNode<'tree>> {
        self.node.next_sibling().map(|node| TSNode::new(node, self.tree))
    }

    pub fn next_named_sibling(&self) -> Option<TSNode<'tree>> {
        self.node.next_named_sibling().map(|node| TSNode::new(node, self.tree))
    }

    pub fn prev_any_sibling(&self) -> Option<TSNode<'tree>> {
        self.node.prev_sibling().map(|node| TSNode::new(node, self.tree))
    }

    pub fn prev_named_sibling(&self) -> Option<TSNode<'tree>> {
        self.node.prev_named_sibling().map(|node| TSNode::new(node, self.tree))
    }

    pub fn any_child(&self, i: usize) -> Option<TSNode<'tree>> {
        self.node.child(i).map(|node| TSNode::new(node, self.tree))
    }

    pub fn named_child(&self, i: usize) -> Option<TSNode<'tree>> {
        self.node.named_child(i).map(|node| TSNode::new(node, self.tree))
    }

    pub fn last_any_child(&self) -> Option<TSNode<'tree>> {
        // .child is already bounds-checked
        self.node.child(self.any_child_count().wrapping_sub(1)).map(|node| TSNode::new(node, self.tree))
    }

    pub fn last_named_child(&self) -> Option<TSNode<'tree>> {
        self.node.named_child(self.named_child_count().wrapping_sub(1)).map(|node| TSNode::new(node, self.tree))
    }

    pub fn child_of_kind(&self, kind: &'static str, cursor: &mut TSCursor<'tree>) -> Option<TSNode<'tree>> {
        self.node.named_children(&mut cursor.cursor)
            .find(|node| node.kind() == kind)
            .map(|node| TSNode::new(node, self.tree))
    }

    pub fn children_of_kind(&self, kind: &'static str, cursor: &mut TSCursor<'tree>) -> impl Iterator<Item = TSNode<'tree>> {
        self.node.named_children(&mut cursor.cursor)
            .filter(|node| node.kind() == kind)
            .map(|node| TSNode::new(node, self.tree))
    }

    pub fn field_child(&self, field_name: &str) -> Option<TSNode<'tree>> {
        self.node.child_by_field_name(field_name).map(|node| TSNode::new(node, self.tree))
    }

    pub fn field_name(&self, cursor: &mut TSCursor<'tree>) -> Option<&str> {
        self.parent().and_then(|parent| {
            cursor.goto(parent);
            cursor.goto_first_child();
            while TSNodeId::of_ts(cursor.cursor.node()) != self.id() {
                if !cursor.goto_next_sibling() {
                    panic!("node not found in parent's children")
                }
            }
            cursor.current_field_name()
        })
    }

    pub fn walk(&self) -> TSCursor<'tree> {
        TSCursor::new(self.node.walk(), self.tree)
    }

    /// NOTE: If tree-sitter has a better way to convert a node into a sub-tree that would be great...
    ///   we cannot and don't really want to store a reference to a node here, and currently
    ///   we need to re-parse the node's text to get a sub-tree.
    pub fn into_subtree(self, parser: &mut TSParser) -> Result<TSSubTree, TreeCreateError> {
        parser.parse_bytes(self.byte_text().into_vec())
    }

    /// *Panics* if already marked
    pub fn mark(&self) {
        let is_marked = self.tree.marked_nodes.borrow_mut().insert(self.id());
        if is_marked {
            panic!("node already marked")
        }
    }

    /// Returns `false` if already marked
    pub fn mark_if_not(&self) -> bool {
        self.tree.marked_nodes.borrow_mut().insert(self.id())
    }

    pub fn is_marked(&self) -> bool {
        self.tree.marked_nodes.borrow().contains(self.id())
    }

    /// *Panics* if a child is already marked
    pub fn mark_children_except(&self, cursor: &mut TSCursor<'tree>, except: &TSNode<'tree>) {
        let mut marked_nodes = self.tree.marked_nodes.borrow_mut();
        for child in self.all_children(cursor) {
            if child.id() != except.id() {
                let is_marked = marked_nodes.insert(child.id());
                if is_marked {
                    panic!("node already marked")
                }
            }
        }
    }
}

impl<'tree> PartialEq<TSNode<'tree>> for TSNode<'tree> {
    fn eq(&self, other: &TSNode<'tree>) -> bool {
        self.id() == other.id()
    }
}

impl<'tree> Eq for TSNode<'tree> {}

impl<'tree> Hash for TSNode<'tree> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl<'tree> TSCursor<'tree> {
    fn new(cursor: tree_sitter::TreeCursor<'tree>, tree: &'tree TSTree) -> Self {
        Self { cursor, tree }
    }

    pub fn node(&self) -> TSNode<'tree> {
        TSNode::new(self.cursor.node(), self.tree)
    }

    pub fn field_name(&self) -> Option<&str> {
        self.cursor.field_name()
    }

    pub fn goto(&mut self, node: TSNode<'tree>) {
        self.cursor.goto(node.node)
    }

    pub fn goto_first_child(&mut self) -> bool {
        self.cursor.goto_first_child()
    }

    pub fn goto_next_sibling(&mut self) -> bool {
        self.cursor.goto_next_sibling()
    }

    pub fn goto_parent(&mut self) -> bool {
        self.cursor.goto_parent()
    }

    pub fn goto_preorder(&mut self, last_state: TraversalState) -> TraversalState {
        if !last_state.is_up() && self.goto_first_child() {
            TraversalState::Down
        } else if self.goto_next_sibling() {
            TraversalState::Right
        } else if self.goto_parent() {
            TraversalState::Up
        } else {
            TraversalState::End
        }
    }
}

impl TSQueryCursor {
    pub fn new() -> Self {
        Self { query_cursor: tree_sitter::QueryCursor::new() }
    }

    pub fn matches<'query, 'tree: 'query>(&mut self, query: &'query TSQuery, node: TSNode<'tree>) -> TSQueryMatches<'query, 'tree> {
        TSQueryMatches {
            query_matches: self.query_cursor.matches(&query.query, node.node, node.tree),
            tree: node.tree,
            query
        }
    }

    pub fn captures<'query, 'tree: 'query>(&mut self, query: &'query TSQuery, node: TSNode<'tree>) -> TSQueryCaptures<'query, 'tree> {
        TSQueryCaptures {
            query_captures: self.query_cursor.captures(&query, node.node, node.tree),
            tree: node.tree,
            query
        }
    }

    pub fn set_match_limit(&mut self, limit: u32) {
        self.query_cursor.set_match_limit(limit)
    }

    pub fn match_limit(&self) -> u32 {
        self.query_cursor.match_limit()
    }

    pub fn did_exceed_match_limit(&self) -> bool {
        self.query_cursor.did_exceed_match_limit()
    }

    pub fn set_byte_range(&mut self, range: Range<usize>) {
        self.query_cursor.set_byte_range(range);
    }

    pub fn set_point_range(&mut self, range: Range<TSPoint>) {
        self.query_cursor.set_point_range(range);
    }
}

impl<'query, 'tree: 'query> Iterator for TSQueryMatches<'query, 'tree> {
    type Item = TSQueryMatch<'query, 'tree>;

    fn next(&mut self) -> Option<Self::Item> {
        self.query_matches.next().map(|query_match| TSQueryMatch {
            query_match,
            tree: self.tree,
            query: self.query
        })
    }
}

impl<'query, 'tree: 'query> Iterator for TSQueryCaptures<'query, 'tree> {
    type Item = TSQueryCapture<'query, 'tree>;

    fn next(&mut self) -> Option<Self::Item> {
        self.query_captures.next().map(|(query_match, index)|
            TSQueryCapture::new(query_match.captures[index], self.tree, self.query))
    }
}

impl<'query, 'tree> TSQueryMatch<'query, 'tree> {
    pub fn iter_captures(&self) -> impl Iterator<Item = TSQueryCapture<'query, 'tree>> {
        self.query_match.captures.iter().map(|&query_capture|
            TSQueryCapture::new(query_capture, self.tree, self.query))
    }

    pub fn capture(&self, index: usize) -> Option<TSQueryCapture<'query, 'tree>> {
        self.query_match.captures.get(index).map(|&query_capture|
            TSQueryCapture::new(query_capture, self.tree, self.query))
    }

    pub fn capture_named(&self, name: &str) -> Option<TSQueryCapture<'query, 'tree>> {
        self.iter_captures().find(|capture| capture.name == name)
    }

    pub fn captures_named(&self, name: &str) -> impl Iterator<Item = TSQueryCapture<'query, 'tree>> {
        self.iter_captures().filter(|capture| capture.name == name)
    }

    pub fn capture_count(&self) -> usize {
        self.query_match.captures.len()
    }

    pub fn pattern_index(&self) -> usize {
        self.query_match.pattern_index
    }

    pub fn id(&self) -> u32 {
        self.query_match.id()
    }

    pub fn remove(&mut self) {
        self.query_match.remove()
    }

    pub fn nodes_for_capture_index(&self, capture_index: u32) -> impl Iterator<Item = TSNode<'tree>> {
        self.query_match
            .nodes_for_capture_index(capture_index)
            .map(move |node| TSNode::new(node, self.tree))
    }
}

impl<'query, 'tree> TSQueryCapture<'query, 'tree> {
    fn new(query_capture: tree_sitter::QueryCapture, tree: &'tree TSTree, query: &'query TSQuery) -> Self {
        Self {
            node: TSNode::new(query_capture.node, tree),
            name: query.capture_names()[query_capture.index]
        }
    }
}

impl TSNodeId {
    pub const INVALID: Self = Self(usize::MAX);

    fn of_ts(node: tree_sitter::Node<'_>) -> Self {
        TSNodeId(node.id())
    }
}

impl From<u64> for TSNodeId {
    fn from(value: u64) -> Self {
        TSNodeId(value as usize)
    }
}

impl Into<u64> for TSNodeId {
    fn into(self) -> u64 {
        self.0 as u64
    }
}

impl Id for TSNodeId {}

impl CachedTreeData {
    fn new(_tree: &tree_sitter::Tree) -> Self {
        CachedTreeData {}
    }
}

impl TraversalState {
    pub fn is_up(&self) -> bool {
        match self {
            TraversalState::Up => true,
            _ => false
        }
    }

    pub fn is_end(&self) -> bool {
        match self {
            TraversalState::End => true,
            _ => false
        }
    }
}

impl<'tree> PreorderTraversal<'tree> {
    pub fn with_cursor(cursor: TSCursor<'tree>) -> Self {
        Self {
            cursor,
            last_state: TraversalState::Start,
        }
    }

    pub fn of_tree(tree: &'tree TSTree) -> Self {
        Self::with_cursor(tree.walk())
    }

    pub fn of_node(node: TSNode<'tree>) -> Self {
        Self::with_cursor(node.walk())
    }

    pub fn peek(&self) -> TraversalItem<'tree> {
        TraversalItem {
            node: self.cursor.node(),
            field_name: self.cursor.field_name(),
            last_state: self.last_state
        }
    }

    pub fn goto_next(&mut self) -> bool {
        if self.last_state.is_end() {
            false
        } else {
            self.last_state = self.cursor.goto_preorder(self.last_state);
            true
        }
    }
}

impl<'tree> Iterator for PreorderTraversal<'tree> {
    type Item = TraversalItem<'tree>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.last_state.is_end() {
            None
        } else {
            let item = self.peek();
            self.last_state = self.cursor.goto_preorder(self.last_state);
            Some(item)
        }
    }
}