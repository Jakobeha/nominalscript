use std::borrow::Borrow;
use std::fmt::Debug;
use std::hash::Hash;
use derive_more::Display;
use smol_str::SmolStr;
use crate::analyses::scopes::ExprTypeMap;

use crate::analyses::types::{DeterminedType, DynRlType, DynRlTypeDecl, ResolveCtx};
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
        assert!(!Self::RESERVED.contains(&name.as_str()), "Reserved {}: {}", stringify!($Name), name);
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

impl Borrow<$NameStr> for $Name {
    fn borrow(&self) -> &$NameStr {
        $NameStr::of(self)
    }
}

impl PartialEq<$NameStr> for $Name {
    fn eq(&self, other: &$NameStr) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl PartialOrd<$NameStr> for $Name {
    fn partial_cmp(&self, other: &$NameStr) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }
}

impl $NameStr {
    pub fn of(s: &(impl AsRef<str> + ?Sized)) -> &Self {
        // SAFETY: Same repr + transparent
        unsafe { &*(s.as_ref() as *const str as *const Self) }
    }
}

impl<'a> From<&'a str> for &'a $NameStr {
    fn from(s: &'a str) -> Self {
        $NameStr::of(s)
    }
}

impl<'a> From<&'a $Name> for &'a $NameStr {
    fn from(s: &'a $Name) -> Self {
        Self::from(s.as_ref())
    }
}

impl AsRef<str> for $NameStr {
    fn as_ref(&self) -> &str {
        // SAFETY: Same repr + transparent
        unsafe { &*(self as *const Self as *const str) }
    }
}

impl PartialEq<$Name> for $NameStr {
    fn eq(&self, other: &$Name) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl PartialOrd<$Name> for $NameStr {
    fn partial_cmp(&self, other: &$Name) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
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