use smol_str::SmolStr;
use crate::type_sitter::ann::Ann;

macro_rules! define_names {
    ($($(#[$attr:meta])* $Name:ident $NameStr:ident $NameIdent:ident),+) => { $(
#[doc = "An owned typed identifier: a type-safe wrapper around [SmolStr]"]
$(#[$attr])*
#[derive(Debug, Clone, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct $Name(SmolStr);

#[doc = concat!("A borrowed typed identifier: a type-safe wrapper around str which can be compared to [", stringify!($Name), "]")]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct $NameStr(str);

$crate::impl_has_ann_record_struct!($NameIdent);
#[doc = concat!("A [", stringify!($NameStr), "] with source info.")]
#[doc = "This *does not* implement [Eq], [Ord], or [Hash], because often you will want to compare the names, but sometimes you may want to compare source info as well (there's an explicit check, [Self::identical], for that)"]
#[derive(Debug, Clone, Copy)]
pub struct $NameIdent<'tree> {
    pub ann: Ann<'tree>,
    #[doc = "The name"]
    pub name: &'tree $NameStr,
}
$crate::impl_has_eqv_ident_struct!(<'tree> $NameIdent<'tree> { name; ann });

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
    pub const fn of_str(s: &str) -> &Self {
        // SAFETY: Same repr + transparent
        unsafe { &*(s as *const str as *const Self) }
    }

    pub fn of(s: &(impl AsRef<str> + ?Sized)) -> &Self {
        Self::of_str(s.as_ref())
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

impl std::fmt::Display for $NameStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'tree> $NameIdent<'tree> {
    pub const fn of_str(ann: Ann<'tree>, name: &'tree str) -> Self {
        Self { ann, name: $NameStr::of_str(name) }
    }

    pub fn of(ann: Ann<'tree>, name: &'tree (impl AsRef<str> + ?Sized)) -> Self {
        Self::of_str(ann, name.as_ref())
    }
}

impl<'tree> AsRef<str> for $NameIdent<'tree> {
    fn as_ref(&self) -> &str {
        self.name.as_ref()
    }
}

impl<'tree> Borrow<$NameStr> for $NameIdent<'tree> {
    fn borrow(&self) -> &$NameStr {
        self.name
    }
}

impl<'tree> std::fmt::Display for $NameIdent<'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}
    )+ };
}

define_names!(
/// The string type used for all value names
ValueName ValueNameStr ValueIdent,
/// The string type used for all type names
TypeName TypeNameStr TypeIdent,
/// The string type used for all field names
FieldName FieldNameStr FieldIdent
);

impl TypeName {
    pub const MISSING: TypeName = TypeName::new_inline("__Missing");
    pub const DUMMY_FOR_HOLE: TypeName = TypeName::new_inline("__Hole");
}

impl TypeNameStr {
    pub const MISSING: &'static Self = Self::of_str("__Missing");
    pub const DUMMY_FOR_HOLE: &'static Self = Self::of_str("__Hole");

    pub const RESERVED: [&'static Self; 4] = [
        Self::of_str("Any"),
        Self::of_str("Never"),
        Self::of_str("Null"),
        Self::of_str("Void"),
    ];

    /// Gets the type of a number literal, if it is a valid number literal.
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
    pub const MISSING: Self = Self {
        ann: Ann::Intrinsic,
        name: TypeNameStr::MISSING,
    };

    pub const DUMMY_FOR_HOLE: Self = Self {
        ann: Ann::Intrinsic,
        name: TypeNameStr::DUMMY_FOR_HOLE,
    };
}

impl ValueNameStr {
    pub const RESERVED: [&'static Self; 0] = [];
}

impl FieldNameStr {
    pub const RESERVED: [&'static Self; 0] = [];
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