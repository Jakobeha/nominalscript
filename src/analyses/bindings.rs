use std::fmt::Debug;
use std::hash::Hash;
use derive_more::Display;
use smol_str::SmolStr;
use crate::analyses::scopes::ExprTypeMap;

use crate::analyses::types::{DynRlType, DynRlTypeDecl, FatType, FatTypeDecl, RlType, RlTypeDecl};
use crate::ast::typed_nodes::AstNode;

#[derive(Debug, Clone, Copy, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Locality {
    #[display(fmt = "global")]
    Global,
    #[display(fmt = "imported")]
    Imported,
    #[display(fmt = "local")]
    Local
}

/// Declares a value identifier which can be referenced:
/// imports, declarations, parameters, predefined globals, etc.
pub trait ValueBinding<'tree>: Debug {
    fn name(&self) -> &ValueName;
    fn value_type(&self) -> &DynRlType;
    fn locality(&self) -> Locality;

    fn infer_type<'a>(&'a self, _typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        self.value_type()
    }
}
pub type DynValueBinding<'tree> = dyn ValueBinding<'tree> + 'tree;

/// Declares a type identifier which can be referenced:
/// imported types, type declarations, type parameters, predefined globals, etc.
pub trait TypeBinding: Debug {
    fn name(&self) -> &TypeName;
    fn type_decl(&self) -> &DynRlTypeDecl;
    fn locality(&self) -> Locality;
}

/// [ValueBinding] inside the file. We track its local uses for type inference.
///
/// There are 3 kinds of bindings: local, imported, and global.
pub trait LocalValueBinding<'tree>: AstNode<'tree> + ValueBinding<'tree> {
    // Currently it doesn't seem we need this, since we only use them for backwards inference
    // and we can do backwards inference for type holes. Maybe in the future...
    // fn local_uses(&self) -> &LocalUses<'tree>;
    fn locality(&self) -> Locality {
        Locality::Local
    }
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
pub trait HoistedValueBinding<'tree>: ValueBinding<'tree> {}

/// [ValueBinding] which is implicitly available to the file (available and not imported).
///
/// There are 3 kinds of bindings: local, imported, and global.
#[derive(Debug, Clone)]
pub struct GlobalValueBinding {
    pub name: ValueName,
    pub type_: RlType,
}

/// [TypeBinding] which is implicitly available to the file (available and not imported).
///
/// There are 3 kinds of bindings: local, imported, and global.
#[derive(Debug, Clone)]
pub struct GlobalTypeBinding {
    pub name: TypeName,
    pub decl: RlTypeDecl,
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
            type_: RlType::resolved(type_),
        }
    }

    pub fn has(name: &ValueName) -> bool {
        todo!("something with lazy_static")
    }

    pub fn get(name: &ValueName) -> Option<&'static GlobalValueBinding> {
        todo!("something with lazy_static")
    }
}

impl<'tree> ValueBinding<'tree> for GlobalValueBinding {
    fn name(&self) -> &ValueName {
        &self.name
    }

    fn value_type(&self) -> &DynRlType {
        &self.type_
    }

    fn locality(&self) -> Locality {
        Locality::Global
    }
}

impl PartialEq<GlobalValueBinding> for GlobalValueBinding {
    fn eq(&self, other: &GlobalValueBinding) -> bool {
        // Name is the only thing that matters:
        // if 2 different global bindings had the same name, how would we resolve them?
        self.name == other.name
    }
}

impl Eq for GlobalValueBinding {}

impl Hash for GlobalValueBinding {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl GlobalTypeBinding {
    pub fn new(name: TypeName, decl: FatTypeDecl) -> Self {
        Self {
            name,
            decl: RlTypeDecl::resolved(decl),
        }
    }

    pub fn has(name: &TypeName) -> bool {
        todo!("something with lazy_static")
    }

    pub fn get(name: &TypeName) -> Option<&'static GlobalTypeBinding> {
        todo!("something with lazy_static")
    }
}

impl TypeBinding for GlobalTypeBinding {
    fn name(&self) -> &TypeName {
        &self.name
    }

    fn type_decl(&self) -> &DynRlTypeDecl {
        &self.decl
    }

    fn locality(&self) -> Locality {
        Locality::Global
    }
}

impl PartialEq<GlobalTypeBinding> for GlobalTypeBinding {
    fn eq(&self, other: &GlobalTypeBinding) -> bool {
        // Name is the only thing that matters:
        // if 2 different global bindings had the same name, how would we resolve them?
        self.name == other.name
    }
}

impl Eq for GlobalTypeBinding {}

impl Hash for GlobalTypeBinding {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl<'tree> HoistedValueBinding<'tree> for GlobalValueBinding {}

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
    pub const MISSING: TypeName = TypeName::new_inline("__Missing");
    pub const DUMMY_FOR_HOLE: TypeName = TypeName::new_inline("__Hole");

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
        // const_assert!(!Self::RESERVED.contains(&name));
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
        // assert!(!Self::RESERVED.contains(&name));
        Self(SmolStr::new_inline(name))
    }
}