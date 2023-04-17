use std::borrow::Borrow;
use std::fmt::Debug;
use std::hash::Hash;
use derive_more::Display;
use indexmap::Equivalent;
use smol_str::SmolStr;
use crate::analyses::scopes::ExprTypeMap;

use crate::analyses::types::{DeterminedType, DynRlType, DynRlTypeDecl, FatType, FatTypeDecl, ResolveCtx, RlType, RlTypeDecl};
use crate::ast::typed_nodes::AstNode;

macro_rules! define_names {
    ($($(#[$attr:meta])* $Name:ident $NameStr:ident),+) => { $(
$(#[$attr])*
#[derive(Debug, Clone, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct $Name(SmolStr);

#[doc = concat!("A wrapper around str which can be compared to [", stringify!($Name), "] (str wrapper is for type-safety)")]
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct $NameStr(str);

impl $Name {
    pub fn new(name: impl Into<SmolStr>) -> Self {
        let name = name.into();
        assert!(!Self::RESERVED.contains(&name.as_str()));
        Self(name.into())
    }

    pub const fn new_inline(name: &'static str) -> Self {
        // const_assert!(!Self::RESERVED.contains(&name));
        Self(SmolStr::new_inline(name))
    }

    pub fn fresh(base_name: &Self, mut is_valid: impl FnMut(&Self) -> bool) -> Self {
        Self::new(fresh_smol_str(base_name, |str| is_valid(Self::from_ref(str))))
    }

    fn from_ref(str: &SmolStr) -> &Self {
        // SAFETY: Same repr + transparent
        unsafe { &*(str as *const SmolStr as *const Self) }
    }
}

impl AsRef<str> for $Name {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl Borrow<$Name> for $Name {
    fn borrow(&self) -> &$Name {
        self
    }
}

impl Borrow<$NameStr> for $Name {
    fn borrow(&self) -> &$NameStr {
        $NameStr::from(self.as_ref())
    }
}

impl $NameStr {
    pub fn of(s: impl AsRef<str>) -> &Self {
        // SAFETY: Same repr + transparent
        unsafe { &*(s.as_ref() as *const str as *const Self) }
    }
}

impl<'a> From<&'a str> for &'a $NameStr {
    fn from(s: &'a str) -> &'a Self {
        Self::of(s)
    }
}

impl<'a> From<&'a $Name> for &'a $NameStr {
    fn from(s: &'a $Name) -> &'a Self {
        Self::from(s.as_ref())
    }
}

impl AsRef<str> for $NameStr {
    fn as_ref(&self) -> &str {
        // SAFETY: Same repr + transparent
        unsafe { &*(s as *const str as *const Self) }
    }
}

impl Into<SmolStr> for $NameStr {
    fn into(self) -> SmolStr {
        self.as_ref().into()
    }
}
    )+ };
}

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
    fn infer_type_det(&self, typed_exprs: Option<&ExprTypeMap<'tree>>, ctx: &ResolveCtx<'_>) -> DeterminedType<'tree>;

    #[allow(unused_variables)]
    fn infer_type<'a>(&'a self, typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
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

define_names!(
/// The string type used for all value names
ValueName ValueNameStr,
/// The string type used for all type names
TypeName TypeNameStr,
/// The string type used for all field names
FieldName FieldNameStr
);

impl GlobalValueBinding {
    pub fn new(name: ValueName, type_: FatType) -> Self {
        Self {
            name,
            type_: RlType::resolved(type_),
        }
    }

    pub fn has(name: &impl Equivalent<ValueName>) -> bool {
        // TODO: something with lazy_static
        name; false
    }

    pub fn get(name: &impl Equivalent<ValueName>) -> Option<&'static GlobalValueBinding> {
        // TODO: something with lazy_static
        name; None
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

    fn infer_type_det(&self, _typed_exprs: Option<&ExprTypeMap<'tree>>, _ctx: &ResolveCtx<'_>) -> DeterminedType<'tree> {
        DeterminedType::intrinsic( self.type_.clone())
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

    pub fn has(name: &impl Equivalent<TypeName>) -> bool {
        // TODO: something with lazy_static
        name; false
    }

    pub fn get(name: &impl Equivalent<TypeName>) -> Option<&'static GlobalTypeBinding> {
        // TODO: something with lazy_static
        name; None
    }

    /// The declared type as a determined type
    pub fn type_det<'tree>(&self) -> DeterminedType<'tree> {
        DeterminedType::intrinsic( self.decl.clone().into_type())
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

impl TypeNameStr {
    /// Gets the type of a number literal, if it is a valid number literal.
    pub fn of_number_literal(text: &str) -> &'static Self {
        TypeNameStr::of(match (text.parse::<usize>(), text.parse::<isize>(), text.parse::<f64>()) {
            (Ok(_), _, _) => "Natural",
            (_, Ok(_), _) => "Integer",
            (_, _, Ok(_)) => "Float",
            // Probably should not actually be possible
            _ => "Number"
        })
    }
}

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
}

impl ValueName {
    pub const RESERVED: [&'static str; 0] = [];
}

impl FieldName {
    pub const RESERVED: [&'static str; 0] = [];
}

fn fresh_smol_str(base_name: impl AsRef<str>, mut is_valid: impl FnMut(&SmolStr) -> bool) -> SmolStr {
    let base_name = base_name.as_ref();
    for i in 0.. {
        let new_name = SmolStr::new(format!("{}{}", base_name, i));
        if is_valid(&new_name) {
            return new_name
        }
    }
    unreachable!("i goes to infinity")
}