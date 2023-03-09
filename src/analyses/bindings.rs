use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::rc::Rc;
use smol_str::SmolStr;
use derive_more::Display;
use crate::analyses::types::{FatType, FatTypeDecl, DynReType, DynReTypeDecl, ReType, ReTypeDecl};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::{AstNode, AstValueIdent};
use crate::misc::lazy::{Lazy, LazyError, RcLazy};

/// Declares a value identifier which can be referenced:
/// imports, declarations, parameters, predefined globals, etc.
pub trait ValueBinding {
    fn name(&self) -> &ValueName;
    fn resolve_type(&self) -> &DynReType;
}

/// Declares a type identifier which can be referenced:
/// imported types, type declarations, type parameters, predefined globals, etc.
pub trait TypeBinding {
    fn name(&self) -> &TypeName;
    fn resolve_decl(&self) -> &DynReTypeDecl;
}

/// [ValueBinding] inside the file. We track its local uses for type inference.
///
/// There are 3 kinds of bindings: local, imported, and global.
pub trait LocalValueBinding<'tree>: AstNode<'tree> + ValueBinding {
    // Currently it doesn't seem we need this, since we only use them for backwards inference
    // and we can do backwards inference for type holes. Maybe in the future...
    // fn local_uses(&self) -> &LocalUses<'tree>;
}

/// [TypeBinding] inside the file. We do not track uses for inference.
pub trait LocalTypeBinding<'tree>: AstNode<'tree> + TypeBinding {}

/// [ValueBinding] which can be referenced in its scope before when it was declared:
/// this includes type and function declarations, but not variable or "lexical" declarations.
///
/// [HoistedValueBinding] and [LocalValueBinding] are not related,
/// (although all imported bindings are currently hoisted unless `require` becomes supported).
///
/// Also, all [TypeBinding]s are hoisted so this kind of trait doesn't make sense for them.
pub trait HoistedValueBinding: ValueBinding {}

/// [ValueBinding] which is implicitly available to the file (available and not imported).
///
/// There are 3 kinds of bindings: local, imported, and global.
pub struct GlobalValueBinding {
    pub name: ValueName,
    pub type_: ReType,
}

/// [TypeBinding] which is implicitly available to the file (available and not imported).
///
/// There are 3 kinds of bindings: local, imported, and global.
pub struct GlobalTypeBinding {
    pub name: TypeName,
    pub decl: ReTypeDecl,
}

// pub struct LocalUses<'tree> {
//     uses: RefCell<BTreeSet<Use<'tree>>>
// }
//
// struct Use<'tree> {
//     node: TSNode<'tree>
// }

/// The string type used for all value names
#[derive(Debug, Clone, Display, PartialEq, Eq, Hash)]
pub struct ValueName(SmolStr);

/// The string type used for all type names
#[derive(Debug, Clone, Display, PartialEq, Eq, Hash)]
pub struct TypeName(SmolStr);

impl GlobalValueBinding {
    pub fn new(name: ValueName, type_: FatType) -> Self {
        Self {
            name,
            type_: ReType::resolved(type_),
        }
    }
}

impl ValueBinding for GlobalValueBinding {
    fn name(&self) -> &ValueName {
        &self.name
    }

    fn resolve_type(&self) -> &DynReType {
        &self.type_
    }
}

impl GlobalTypeBinding {
    pub fn new(name: TypeName, decl: FatTypeDecl) -> Self {
        Self {
            name,
            decl: ReTypeDecl::resolved(decl),
        }
    }
}

impl TypeBinding for GlobalTypeBinding {
    fn name(&self) -> &TypeName {
        &self.name
    }

    fn resolve_decl(&self) -> &DynReTypeDecl {
        &self.decl
    }
}

impl HoistedValueBinding for GlobalValueBinding {}

// impl<'tree> LocalUses<'tree> {
//     pub fn new() -> Self {
//         Self { uses: RefCell::new(BTreeSet::new()) }
//     }
//
//     pub fn insert(&self, node: TSNode<'tree>) {
//         self.uses.borrow_mut().insert(Use { node });
//     }
// }
//
// impl<'tree> PartialEq<Use<'tree>> for Use<'tree> {
//     fn eq(&self, other: &Self) -> bool {
//         self.node.start_byte() == other.node.start_byte()
//     }
// }
//
// impl<'tree> Eq for Use<'tree> {}
//
// impl<'tree> PartialOrd<Use<'tree>> for Use<'tree> {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         Some(self.cmp(other))
//     }
// }
//
// impl<'tree> Ord for Use<'tree> {
//     fn cmp(&self, other: &Self) -> Ordering {
//         self.node.start_byte().cmp(&other.node.start_byte())
//     }
// }

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