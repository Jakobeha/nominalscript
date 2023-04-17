use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Display;
use std::path::Path;
use std::fs;
use std::iter::{once, Once};
use std::str::Utf8Error;
use derive_more::{Display, From, Error};
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::ptr::NonNull;

#[derive(Debug)]
pub struct TSTree {
    byte_text: Vec<u8>,
    tree: tree_sitter::Tree,
    cached_data: CachedTreeData,
    marked_nodes: RefCell<HashSet<TSNodeId>>
}

#[derive(Debug)]
struct CachedTreeData {}

#[derive(Debug, Clone, Copy)]
pub struct TSNode<'tree> {
    node: tree_sitter::Node<'tree>,
    tree: &'tree TSTree,
}

/// Raw pointer equivalent of [TSNode]
#[derive(Debug, Clone, Copy)]
pub struct TSNodePtr {
    node_data: TSNodeData,
    tree: NonNull<TSTree>
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
/// Taken straight from [tree_sitter::ffi::TSNode], they must both have the same size
struct TSNodeData {
    context: [u32; 4usize],
    id: *const (),
    tree: *const (),
}

#[derive(Debug, Clone, Copy, Display, PartialEq, Eq, Hash)]
pub struct TSNodeId(usize);

#[derive(Clone)]
pub struct TSCursor<'tree> {
    cursor: tree_sitter::TreeCursor<'tree>,
    tree: &'tree TSTree,
}

pub struct TSQueryCursor {
    query_cursor: tree_sitter::QueryCursor
}

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
    #[display(fmt = "Invalid UTF-8 at byte index {}-{}", actual_index, "error.error_len().unwrap_or(0)")]
    NotUtf8 { actual_index: usize, error: Utf8Error }
}

/// Allows you to store TSNode separately from the tree
#[derive(Debug, Clone)]
pub struct SubTree {
    pub text: String,
    pub range: TSRange,
    /// Node which can be dereferenced in case the tree is still alive,
    /// otherwise it is dangling
    pub root: Option<TSNodePtr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraversalState {
    Start,
    Down,
    Right,
    Up,
    End
}

#[derive(Clone)]
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
    #[inline]
    pub fn new(language: tree_sitter::Language) -> Result<Self, TSLanguageError> {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(language)?;
        Ok(Self(parser))
    }

    #[inline]
    pub fn set_language(&mut self, language: TSLanguage) -> Result<(), TSLanguageError> {
        self.0.set_language(language)
    }

    #[inline]
    pub fn set_included_ranges(&mut self, ranges: &[TSRange]) -> Result<(), TSIncludedRangesError> {
        self.0.set_included_ranges(ranges)
    }

    #[inline]
    pub fn parse_file(&mut self, path: &Path) -> Result<TSTree, TreeCreateError> {
        self.parse_bytes(fs::read(path)?)
    }

    #[inline]
    pub fn parse_string(&mut self, text: String) -> Result<TSTree, TreeCreateError> {
        self.parse_bytes(text.into_bytes())
    }

    #[inline]
    pub fn parse_bytes(&mut self, byte_text: Vec<u8>) -> Result<TSTree, TreeCreateError> {
        let tree = self.0.parse(&byte_text, None).ok_or(TreeCreateError::ParsingFailed)?;
        TSTree::new(tree, byte_text)
    }
}

impl TSTree {
    #[inline]
    fn new(tree: tree_sitter::Tree, byte_text: Vec<u8>) -> Result<Self, TreeCreateError> {
        Self::validate_utf8(&tree, &byte_text)?;
        let cached_data = CachedTreeData::new(&tree);
        Ok(Self { byte_text, tree, cached_data, marked_nodes: RefCell::new(HashSet::new()) })
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

    #[inline]
    pub fn text(&self) -> &str {
        // SAFETY: we ran validate_utf8 before constructing so the text is valid UTF-8
        unsafe { std::str::from_utf8_unchecked(&self.byte_text) }
    }

    #[inline]
    pub fn root_node(&self) -> TSNode<'_> {
        TSNode::new(self.tree.root_node(), self)
    }

    #[inline]
    pub fn walk(&self) -> TSCursor<'_> {
        TSCursor::new(self.tree.walk(), self)
    }
}

impl<'tree> tree_sitter::TextProvider<'tree> for &'tree TSTree {
    type I = Once<&'tree [u8]>;

    #[inline]
    fn text(&mut self, node: tree_sitter::Node<'_>) -> Self::I {
        once(&self.byte_text[node.byte_range()])
    }
}

impl<'tree> TSNode<'tree> {
    #[inline]
    fn new(node: tree_sitter::Node<'tree>, tree: &'tree TSTree) -> Self {
        Self { node, tree }
    }

    #[inline]
    pub fn id(&self) -> TSNodeId {
        TSNodeId::of_ts(self.node)
    }

    #[inline]
    pub fn kind(&self) -> &'static str {
        self.node.kind()
    }

    #[inline]
    pub fn is_named(&self) -> bool {
        self.node.is_named()
    }

    #[inline]
    pub fn start_byte(&self) -> usize {
        self.node.start_byte()
    }

    #[inline]
    pub fn end_byte(&self) -> usize {
        self.node.end_byte()
    }

    #[inline]
    pub fn start_point(&self) -> TSPoint {
        self.node.start_position()
    }

    #[inline]
    pub fn end_point(&self) -> TSPoint {
        self.node.end_position()
    }

    #[inline]
    pub fn byte_range(&self) -> Range<usize> {
        self.node.byte_range()
    }

    #[inline]
    pub fn range(&self) -> TSRange {
        self.node.range()
    }

    #[inline]
    fn byte_text(&self) -> &[u8] {
        &self.tree.byte_text[self.byte_range()]
    }

    #[inline]
    pub fn text(&self) -> &str {
        // SAFETY: we ran validate_utf8 before constructing so all nodes are valid UTF-8
        unsafe { std::str::from_utf8_unchecked(self.byte_text()) }
    }

    #[inline]
    pub fn all_children<'a>(&'a self, cursor: &'a mut TSCursor<'tree>) -> impl Iterator<Item = TSNode<'tree>> + 'a {
        self.node.children(&mut cursor.cursor).map(move |node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn named_children<'a>(&'a self, cursor: &'a mut TSCursor<'tree>) -> impl Iterator<Item = TSNode<'tree>> + 'a {
        self.node.named_children(&mut cursor.cursor).map(move |node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn any_child_count(&self) -> usize {
        self.node.child_count()
    }

    #[inline]
    pub fn named_child_count(&self) -> usize {
        self.node.named_child_count()
    }

    #[inline]
    pub fn parent(&self) -> Option<TSNode<'tree>> {
        self.node.parent().map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn next_any_sibling(&self) -> Option<TSNode<'tree>> {
        self.node.next_sibling().map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn next_named_sibling(&self) -> Option<TSNode<'tree>> {
        self.node.next_named_sibling().map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn prev_any_sibling(&self) -> Option<TSNode<'tree>> {
        self.node.prev_sibling().map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn prev_named_sibling(&self) -> Option<TSNode<'tree>> {
        self.node.prev_named_sibling().map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn any_child(&self, i: usize) -> Option<TSNode<'tree>> {
        self.node.child(i).map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn named_child(&self, i: usize) -> Option<TSNode<'tree>> {
        self.node.named_child(i).map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn last_any_child(&self) -> Option<TSNode<'tree>> {
        // .child is already bounds-checked
        self.node.child(self.any_child_count().wrapping_sub(1)).map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn last_named_child(&self) -> Option<TSNode<'tree>> {
        self.node.named_child(self.named_child_count().wrapping_sub(1)).map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn child_of_kind(&self, kind: &'static str, cursor: &mut TSCursor<'tree>) -> Option<TSNode<'tree>> {
        self.node.named_children(&mut cursor.cursor)
            .find(|node| node.kind() == kind)
            .map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn children_of_kind<'a>(&'a self, kind: &'static str, cursor: &'a mut TSCursor<'tree>) -> impl Iterator<Item = TSNode<'tree>> + 'a {
        self.node.named_children(&mut cursor.cursor)
            .filter(move |node| node.kind() == kind)
            .map(|node| TSNode::new(node, self.tree))
    }

    #[inline]
    pub fn field_child(&self, field_name: &str) -> Option<TSNode<'tree>> {
        self.node.child_by_field_name(field_name).map(|node| TSNode::new(node, self.tree))
    }

    pub fn field_name<'a>(&self, cursor: &'a mut TSCursor<'tree>) -> Option<&'a str> {
        self.parent().and_then(|parent| {
            cursor.goto(parent);
            cursor.goto_first_child();
            while TSNodeId::of_ts(cursor.cursor.node()) != self.id() {
                if !cursor.goto_next_sibling() {
                    panic!("node not found in parent's children")
                }
            }
            cursor.field_name()
        })
    }

    #[inline]
    pub fn walk(&self) -> TSCursor<'tree> {
        TSCursor::new(self.node.walk(), self.tree)
    }

    #[inline]
    pub fn to_ptr(&self) -> TSNodePtr {
        TSNodePtr {
            node_data: TSNodeData::from(self.node),
            tree: NonNull::from(self.tree),
        }
    }

    #[inline]
    pub fn to_subtree(&self) -> SubTree {
        SubTree {
            text: self.text().to_string(),
            range: self.range(),
            root: Some(self.to_ptr()),
        }
    }

    /// *Panics* if already marked
    #[inline]
    pub fn mark(&self) {
        let is_marked = self.tree.marked_nodes.borrow_mut().insert(self.id());
        if is_marked {
            panic!("node already marked")
        }
    }

    /// Returns `false` if already marked
    #[inline]
    pub fn mark_if_not(&self) -> bool {
        self.tree.marked_nodes.borrow_mut().insert(self.id())
    }

    #[inline]
    pub fn is_marked(&self) -> bool {
        self.tree.marked_nodes.borrow().contains(&self.id())
    }

    /// *Panics* if a child is already marked
    pub fn mark_children_except(&self, except: TSNode<'tree>, cursor: &mut TSCursor<'tree>) {
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
    #[inline]
    fn eq(&self, other: &TSNode<'tree>) -> bool {
        self.id() == other.id()
    }
}

impl<'tree> Eq for TSNode<'tree> {}

impl<'tree> Hash for TSNode<'tree> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl TSNodePtr {
    /// SAFETY: You must ensure that the tree the node came from is alive
    #[inline]
    pub unsafe fn to_node(&self) -> TSNode {
        TSNode {
            node: self.node_data.to_node(),
            tree: self.tree.as_ref(),
        }
    }
}

impl TSNodeData {
    /// SAFETY: You must ensure that the tree the node came from is alive
    #[inline]
    pub unsafe fn to_node<'tree>(self) -> tree_sitter::Node<'tree> {
        // SAFETY: tree_sitter::Node is POD (no Drop, Copy),
        // and sizes are compile_time checked to be the same
        std::mem::transmute(self)
    }
}

impl<'tree> From<tree_sitter::Node<'tree>> for TSNodeData {
    #[inline]
    fn from(node: tree_sitter::Node) -> Self {
        // SAFETY: We are storing this as opaquely, tree_sitter::Node is POD (no Drop, Copy),
        // and sizes are compile_time checked to be the same
        unsafe { std::mem::transmute::<tree_sitter::Node<'_>, TSNodeData>(node) }
    }
}

impl<'tree> TSCursor<'tree> {
    #[inline]
    fn new(cursor: tree_sitter::TreeCursor<'tree>, tree: &'tree TSTree) -> Self {
        Self { cursor, tree }
    }

    #[inline]
    pub fn node(&self) -> TSNode<'tree> {
        TSNode::new(self.cursor.node(), self.tree)
    }

    #[inline]
    pub fn field_name(&self) -> Option<&'static str> {
        self.cursor.field_name()
    }

    #[inline]
    pub fn goto(&mut self, node: TSNode<'tree>) {
        if self.cursor.node() != node.node {
            self.cursor.reset(node.node)
        }
    }

    #[inline]
    pub fn goto_first_child(&mut self) -> bool {
        self.cursor.goto_first_child()
    }

    #[inline]
    pub fn goto_next_sibling(&mut self) -> bool {
        self.cursor.goto_next_sibling()
    }

    #[inline]
    pub fn goto_parent(&mut self) -> bool {
        self.cursor.goto_parent()
    }

    #[inline]
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
    #[inline]
    pub fn new() -> Self {
        Self { query_cursor: tree_sitter::QueryCursor::new() }
    }

    #[inline]
    pub fn matches<'query, 'tree: 'query>(&'query mut self, query: &'query TSQuery, node: TSNode<'tree>) -> TSQueryMatches<'query, 'tree> {
        TSQueryMatches {
            query_matches: self.query_cursor.matches(&query, node.node, node.tree),
            tree: node.tree,
            query
        }
    }

    #[inline]
    pub fn captures<'query, 'tree: 'query>(&'query mut self, query: &'query TSQuery, node: TSNode<'tree>) -> TSQueryCaptures<'query, 'tree> {
        TSQueryCaptures {
            query_captures: self.query_cursor.captures(&query, node.node, node.tree),
            tree: node.tree,
            query
        }
    }

    #[inline]
    pub fn set_match_limit(&mut self, limit: u32) {
        self.query_cursor.set_match_limit(limit)
    }

    #[inline]
    pub fn match_limit(&self) -> u32 {
        self.query_cursor.match_limit()
    }

    #[inline]
    pub fn did_exceed_match_limit(&self) -> bool {
        self.query_cursor.did_exceed_match_limit()
    }

    #[inline]
    pub fn set_byte_range(&mut self, range: Range<usize>) {
        self.query_cursor.set_byte_range(range);
    }

    #[inline]
    pub fn set_point_range(&mut self, range: Range<TSPoint>) {
        self.query_cursor.set_point_range(range);
    }
}

impl<'query, 'tree: 'query> Iterator for TSQueryMatches<'query, 'tree> {
    type Item = TSQueryMatch<'query, 'tree>;

    #[inline]
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

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.query_captures.next().map(|(query_match, index)|
            TSQueryCapture::new(query_match.captures[index], self.tree, self.query))
    }
}

impl<'query, 'tree> TSQueryMatch<'query, 'tree> {
    #[inline]
    pub fn iter_captures(&self) -> impl Iterator<Item = TSQueryCapture<'query, 'tree>> {
        self.query_match.captures.iter().map(|&query_capture|
            TSQueryCapture::new(query_capture, self.tree, self.query))
    }

    #[inline]
    pub fn capture(&self, index: usize) -> Option<TSQueryCapture<'query, 'tree>> {
        self.query_match.captures.get(index).map(|&query_capture|
            TSQueryCapture::new(query_capture, self.tree, self.query))
    }

    #[inline]
    pub fn capture_named(&self, name: &str) -> Option<TSQueryCapture<'query, 'tree>> {
        self.iter_captures().find(|capture| capture.name == name)
    }

    #[inline]
    pub fn captures_named<'a>(&'a self, name: &'a str) -> impl Iterator<Item = TSQueryCapture<'query, 'tree>> + 'a {
        self.iter_captures().filter(move |capture| capture.name == name)
    }

    #[inline]
    pub fn capture_count(&self) -> usize {
        self.query_match.captures.len()
    }

    #[inline]
    pub fn pattern_index(&self) -> usize {
        self.query_match.pattern_index
    }

    #[inline]
    pub fn id(&self) -> u32 {
        self.query_match.id()
    }

    #[inline]
    pub fn remove(self) {
        self.query_match.remove()
    }

    #[inline]
    pub fn nodes_for_capture_index(&self, capture_index: u32) -> impl Iterator<Item = TSNode<'tree>> + '_ {
        self.query_match
            .nodes_for_capture_index(capture_index)
            .map(move |node| TSNode::new(node, self.tree))
    }
}

impl<'query, 'tree> TSQueryCapture<'query, 'tree> {
    #[inline]
    fn new(query_capture: tree_sitter::QueryCapture<'tree>, tree: &'tree TSTree, query: &'query TSQuery) -> Self {
        Self {
            node: TSNode::new(query_capture.node, tree),
            name: &query.capture_names()[query_capture.index as usize]
        }
    }
}

impl TSNodeId {
    pub const INVALID: Self = Self(usize::MAX);

    #[inline]
    fn of_ts(node: tree_sitter::Node<'_>) -> Self {
        TSNodeId(node.id())
    }
}

impl From<u64> for TSNodeId {
    #[inline]
    fn from(value: u64) -> Self {
        TSNodeId(value as usize)
    }
}

impl Into<u64> for TSNodeId {
    #[inline]
    fn into(self) -> u64 {
        self.0 as u64
    }
}

impl CachedTreeData {
    #[inline]
    fn new(_tree: &tree_sitter::Tree) -> Self {
        CachedTreeData {}
    }
}

impl Clone for TreeCreateError {
    #[inline]
    fn clone(&self) -> Self {
        match self {
            TreeCreateError::IO(e) => TreeCreateError::IO(std::io::Error::from(e.kind())),
            // SAFETY: Relies on LanguageError being POD, so it could technically break in a future version but very unlikely
            TreeCreateError::LoadLanguage(e) => TreeCreateError::LoadLanguage(unsafe { std::mem::transmute_copy(e) }),
            TreeCreateError::ParsingFailed => TreeCreateError::ParsingFailed,
            TreeCreateError::NotUtf8 { actual_index, error } => TreeCreateError::NotUtf8 { actual_index: *actual_index, error: error.clone() }
        }
    }
}

impl PartialEq<SubTree> for SubTree {
    #[inline]
    fn eq(&self, other: &SubTree) -> bool {
        self.text == other.text &&
            self.range == other.range
    }
}

impl Eq for SubTree {}

impl Display for SubTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl TraversalState {
    #[inline]
    pub fn is_up(&self) -> bool {
        match self {
            TraversalState::Up => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_end(&self) -> bool {
        match self {
            TraversalState::End => true,
            _ => false
        }
    }
}

impl<'tree> PreorderTraversal<'tree> {
    #[inline]
    pub fn with_cursor(cursor: TSCursor<'tree>) -> Self {
        Self {
            cursor,
            last_state: TraversalState::Start,
        }
    }

    #[inline]
    pub fn of_tree(tree: &'tree TSTree) -> Self {
        Self::with_cursor(tree.walk())
    }

    #[inline]
    pub fn of_node(node: TSNode<'tree>) -> Self {
        Self::with_cursor(node.walk())
    }

    #[inline]
    pub fn peek(&self) -> TraversalItem<'tree> {
        TraversalItem {
            node: self.cursor.node(),
            field_name: self.cursor.field_name(),
            last_state: self.last_state
        }
    }

    #[inline]
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

    #[inline]
    fn next(&mut self) -> Option<TraversalItem<'tree>> {
        if self.last_state.is_end() {
            return None
        }
        let item = self.peek();
        self.last_state = self.cursor.goto_preorder(self.last_state);
        Some(item)
    }
}