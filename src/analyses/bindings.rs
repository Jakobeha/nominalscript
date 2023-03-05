use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use smol_str::SmolStr;
use crate::analyses::types::FatType;
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::AstValueIdent;
use crate::misc::lazy::{Lazy, LazyError};

/// Declares an identifier which can be referenced:
/// imports, declarations, parameters, predefined globals, etc.
pub trait Binding {
    fn name(&self) -> &ValueName;
    fn resolve_type(&self) -> Lazy<&FatType>;
}

/// Binding inside the file. We track its local uses for type inference.
///
/// There are 3 kinds of bindings: local, imported, and global.
pub trait LocalBinding<'tree>: Binding {
    fn local_uses(&self) -> &LocalUses<'tree>;
}

/// Binding which can be referenced in its scope before when it was declared:
/// this includes type and function declarations, but not variable or "lexical" declarations.
///
/// [HoistedBinding] and [LocalBinding] are not related,
/// (although all imported bindings are currently hoisted unless `require` becomes supported)
pub trait HoistedBinding: Binding {}

/// Binding which is implicitly available to the file (available and not imported).
///
/// There are 3 kinds of bindings: local, imported, and global.
pub struct GlobalBinding {
    pub name: ValueName,
    pub type_: FatType
}

pub struct LocalUses<'tree> {
    uses: RefCell<BTreeSet<Use<'tree>>>
}

struct Use<'tree> {
    node: TSNode<'tree>
}

/// The string type used for all value names
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueName(SmolStr);

/// The string type used for all type names
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeName(SmolStr);

impl Binding for GlobalBinding {
    fn name(&self) -> &ValueName {
        &self.name
    }

    fn resolve_type(&self) -> Lazy<&FatType> {
        Lazy::immediate(&self.type_)
    }
}

impl HoistedBinding for GlobalBinding {}

impl<'tree> LocalUses<'tree> {
    pub fn new() -> Self {
        Self { uses: RefCell::new(BTreeSet::new()) }
    }

    pub fn insert(&self, node: TSNode<'tree>) {
        self.uses.borrow_mut().insert(Use { node });
    }
}

impl<'tree> PartialEq<Use<'tree>> for Use<'tree> {
    fn eq(&self, other: &Self) -> bool {
        self.node.start_byte() == other.node.start_byte()
    }
}

impl<'tree> Eq for Use<'tree> {}

impl<'tree> PartialOrd<Use<'tree>> for Use<'tree> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'tree> Ord for Use<'tree> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.node.start_byte().cmp(&other.node.start_byte())
    }
}

impl TypeName {
    pub const RESERVED: [&'static str; 4] = [
        "Any",
        "Never",
        "Null",
        "Void",
    ];

    //noinspection DuplicatedCode
    pub fn new(name: impl Into<SmolStr>) -> Self {
        let name = name.into();
        assert!(!Self::RESERVED.contains(&name.as_str()));
        Self(name.into())
    }

    pub const fn new_inline(name: &'static str) -> Self {
        assert!(!Self::RESERVED.contains(&name));
        Self(SmolStr::new_inline(name))
    }
}

impl ValueName {
    pub const RESERVED: [&'static str; 0] = [];

    //noinspection DuplicatedCode
    pub fn new(name: impl Into<SmolStr>) -> Self {
        let name = name.into();
        assert!(!Self::RESERVED.contains(&name.as_str()));
        Self(name.into())
    }

    pub const fn new_inline(name: &'static str) -> Self {
        assert!(!Self::RESERVED.contains(&name));
        Self(SmolStr::new_inline(name))
    }
}