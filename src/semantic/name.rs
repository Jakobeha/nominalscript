use smol_str::SmolStr;
use crate::semantic::storage::ann::Ann;

macro_rules! define_names {
    ($($(#[$attr:meta])* $NameOwned:ident $Name:ident $NameCow:ident $Ident:ident),+) => { $(
#[doc = "An owned typed identifier = a type-safe wrapper around [SmolStr]"]
$(#[$attr])*
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct $NameOwned(SmolStr);

#[doc = "A borrowed typed identifier = a type-safe wrapper around [str]"]
#[doc = "\n"]
$(#[$attr])*
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct $Name(str);

#[doc = concat!("An owned or borrowed typed identifier = a type-safe wrapper around [std::borrow::Cow]<'_, [", stringify!($Name), "]>, which is either [", stringify!($Name), "] or [", stringify!($NameOwned), "]")]
#[doc = ""]
$(#[$attr])*
pub type $NameCow<'tree> = std::borrow::Cow<'tree, $Name>;

#[doc = concat!("A [", stringify!($Name), "] with source info.")]
#[doc = ""]
#[doc = "This *does not* implement [Eq], [Ord], or [Hash], because often you will want to compare the names, but sometimes you may want to compare source info as well (there's an explicit check, [Self::identical], for that). When you need the traits to be implemented (e.g. to store as a map key), use [crate::misc::ByEqv] or [crate::misc::ByIdentical] wrappers"]
#[doc = ""]
$(#[$attr])*
#[derive(Debug, Clone, Copy)]
pub struct $Ident<'tree> {
    #[doc = "The annotation"]
    pub ann: Ann<'tree>,
    #[doc = "The name"]
    pub name: &'tree $Name,
}
$crate::impl_has_eqv_ident_struct!($Ident<'tree> { name; ann });

impl $NameOwned {
    #[inline]
    pub fn new(str: impl Into<SmolStr>) -> Self {
        let str = str.into();
        assert!(!$Name::RESERVED.contains(&str.as_str()), "Reserved {}: {}", stringify!($Name), str);
        Self(str)
    }

    #[inline]
    pub const fn new_inline(str: &'static str) -> Self {
        // const_assert!(!$Name::RESERVED.contains(&str));
        Self(SmolStr::new_inline(str))
    }

    #[inline]
    fn of(str: &SmolStr) -> &Self {
        assert!(!$Name::RESERVED.contains(&str.as_str()), "Reserved {}: {}", stringify!($Name), str);
        // SAFETY: Same repr + transparent
        unsafe { &*(str as *const SmolStr as *const Self) }
    }

    #[inline]
    pub fn fresh(base_name: &Self, mut is_valid: impl FnMut(&Self) -> bool) -> Self {
        Self::new(fresh_smol_str(
            base_name,
            |str| !$Name::RESERVED.contains(&str.as_str()) && is_valid(Self::of(str))
        ))
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<str> for $NameOwned {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<$Name> for $NameOwned {
    #[inline]
    fn as_ref(&self) -> &$Name {
        // Skip checking RESERVED, because we know it's valid
        // SAFETY: Same repr + transparent
        unsafe { &*(self.as_str() as *const str as *const $Name) }
    }
}

impl std::borrow::Borrow<str> for $NameOwned {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl std::borrow::Borrow<$Name> for $NameOwned {
    #[inline]
    fn borrow(&self) -> &$Name {
        // Skip checking RESERVED, because we know it's valid
        // SAFETY: Same repr + transparent
        unsafe { &*(self.as_str() as *const str as *const $Name) }
    }
}

impl PartialEq<$Name> for $NameOwned {
    #[inline]
    fn eq(&self, other: &$Name) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialOrd<$Name> for $NameOwned {
    #[inline]
    fn partial_cmp(&self, other: &$Name) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl std::fmt::Display for $NameOwned {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl $Name {
    #[inline]
    pub const fn of_str(str: &str) -> &Self {
        assert!(!$Name::RESERVED.contains(&str), "Reserved {}: {}", stringify!($Name), str);
        // SAFETY: Same repr + transparent
        unsafe { &*(str as *const str as *const Self) }
    }

    #[inline]
    pub fn of(str: &(impl AsRef<str> + ?Sized)) -> &Self {
        Self::of_str(str.as_ref())
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        // SAFETY: Same repr + transparent
        unsafe { &*(self as *const Self as *const str) }
    }
}

impl<'a> From<&'a str> for &'a $Name {
    #[inline]
    fn from(str: &'a str) -> Self {
        $Name::of(str)
    }
}

impl<'a> From<&'a $NameOwned> for &'a $Name {
    #[inline]
    fn from(str: &'a $NameOwned) -> Self {
        $Name::of(str)
    }
}

impl AsRef<str> for $Name {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl ToOwned for $Name {
    type Owned = $NameOwned;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        // Skip checking RESERVED, because we know it's valid
        $NameOwned(SmolStr::new(self))
    }
}

impl PartialEq<$NameOwned> for $Name {
    #[inline]
    fn eq(&self, other: &$NameOwned) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialOrd<$NameOwned> for $Name {
    #[inline]
    fn partial_cmp(&self, other: &$NameOwned) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl std::fmt::Display for $Name {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'tree> $Ident<'tree> {
    #[inline]
    pub const fn of_str(ann: Ann<'tree>, name: &'tree str) -> Self {
        Self { ann, name: $Name::of_str(name) }
    }

    #[inline]
    pub fn of(ann: Ann<'tree>, name: &'tree (impl AsRef<str> + ?Sized)) -> Self {
        Self::of_str(ann, name.as_ref())
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl<'tree> AsRef<str> for $Ident<'tree> {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<'tree> AsRef<$Name> for $Ident<'tree> {
    #[inline]
    fn as_ref(&self) -> &$Name {
        self.name
    }
}

impl<'tree> std::borrow::Borrow<str> for $Ident<'tree> {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl<'tree> std::borrow::Borrow<$Name> for $Ident<'tree> {
    #[inline]
    fn borrow(&self) -> &$Name {
        self.name
    }
}

impl<'tree> std::fmt::Display for $Ident<'tree> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}
    )+ };
}

define_names!(
/// For value names, e.g. `foo` in `let foo: Bar = { baz: 10 }`
ValueNameOwned ValueName ValueNameCow ValueIdent,
/// For type names, e.g. `Bar` in `let foo: Bar = { baz: 10 }`
TypeNameOwned TypeName TypeNameCow TypeIdent,
/// For field names, e.g. `baz` in `let foo: Bar = { baz: 10 }`
FieldNameOwned FieldName FieldNameCow FieldIdent
);

impl TypeNameOwned {
    /// Name to substitute for missing types, should never actually appear in code
    pub const MISSING: TypeNameOwned = TypeNameOwned::new_inline("__Missing");
    /// Name to substitute for hole types when you need some AST representation, should never
    /// actually appear in code
    pub const DUMMY_FOR_HOLE: TypeNameOwned = TypeNameOwned::new_inline("__Hole");
}

impl TypeName {
    /// Name to substitute for missing types, should never actually appear in code
    pub const MISSING: &'static Self = Self::of_str("__Missing");
    /// Name to substitute for hole types when you need some AST representation, should never
    /// actually appear in code
    pub const DUMMY_FOR_HOLE: &'static Self = Self::of_str("__Hole");

    /// Reserved type names which should never be identifiers. Some of these may be valid types,
    /// like `Any`, just not [crate::semantic::expr::TypeIdentifier]s.
    pub const RESERVED: [&'static str; 4] = [
        "Any",
        "Never",
        "Null",
        "Void",
    ];

    /// Gets the type of a number literal, if it's a valid number literal.
    pub fn of_number_literal(text: &str) -> &'static Self {
        Self::of(match (text.parse::<usize>(), text.parse::<isize>(), text.parse::<f64>()) {
            (Ok(_), _, _) => "Natural",
            (_, Ok(_), _) => "Integer",
            (_, _, Ok(_)) => "Float",
            // Probably should not actually be possible
            _ => "Number"
        })
    }
}

impl<'tree> TypeIdent<'tree> {
    /// Name to substitute for missing types, should never actually appear in code
    pub const MISSING: Self = Self {
        ann: Ann::Intrinsic,
        name: TypeName::MISSING,
    };

    /// Name to substitute for hole types when you need some AST representation, should never
    /// actually appear in code
    pub const DUMMY_FOR_HOLE: Self = Self {
        ann: Ann::Intrinsic,
        name: TypeName::DUMMY_FOR_HOLE,
    };
}

impl ValueName {
    /// Reserved value names which should never be identifiers
    pub const RESERVED: [&'static str; 0] = [];
}

impl FieldName {
    /// Reserved field names which should never be identifiers
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