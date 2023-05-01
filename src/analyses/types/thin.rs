use std::cmp::Ordering;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};

use replace_with::replace_with_or_default;

use crate::analyses::bindings::TypeIdent;
use crate::analyses::types::generic::{Field, FnType, HasNullability, IdentType, impl_structural_type_constructors, InheritedTrait, NominalGuard, Nullability, Optionality, OptionalType, RestArgTrait, ReturnType, StructureKind, StructureType, TypeArgTrait, TypeParam, TypescriptType, TypeTrait, TypeTraitMapsFrom, Variance};
use crate::ast::ann::Ann;
use crate::{impl_has_ann_enum, impl_has_ann_record_struct};

/// Type declaration before we've resolved the supertypes
#[derive(Debug, Clone)]
pub struct ThinTypeDecl<'tree> {
    pub ann: Ann<'tree>,
    pub name: TypeIdent<'tree>,
    pub type_params: Vec<TypeParam<'tree, ThinType<'tree>>>,
    pub supertypes: Vec<ThinType<'tree>>,
    pub typescript_supertype: Vec<TypescriptType<'tree>>,
    pub guard: Vec<NominalGuard<'tree>>,
}
impl_has_ann_record_struct!(ThinTypeDecl);

/// Thin type = type reference which is parsed from a string or AST node
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum ThinType<'tree> {
    #[default]
    Any { ann: Ann<'tree> },
    Never {
        ann: Ann<'tree>,
        nullability: Nullability
    },
    Structural {
        ann: Ann<'tree>,
        nullability: Nullability,
        structure: StructureType<'tree, ThinType<'tree>>
    },
    Nominal {
        ann: Ann<'tree>,
        nullability: Nullability,
        id: IdentType<'tree, ThinType<'tree>>
    },
    IllegalVoid {
        ann: Ann<'tree>
    }
}
impl_has_ann_enum!(ThinType { Any, Never, Structural, Nominal, IllegalVoid });

impl<'tree> ThinTypeDecl<'tree> {
    pub const MISSING: Self = Self::missing(Ann::Intrinsic);

    pub const fn missing(ann: Ann<'tree>) -> Self {
        Self {
            ann,
            name: TypeIdent::<'tree>::MISSING,
            type_params: Vec::new(),
            supertypes: Vec::new(),
            typescript_supertype: Vec::new(),
            guard: Vec::new()
        }
    }

    /// Converts this into the nominal type it declares: for [ThinType], just a nominal type with
    /// the name and type parameters
    pub fn into_type(self) -> ThinType<'tree> {
        ThinType::Nominal {
            ann: self.ann,
            nullability: Nullability::NonNullable,
            id: IdentType {
                ann: self.ann,
                name: self.name,
                generic_args: self.type_params.into_iter().map(|x| x.into_type()).collect(),
            }
        }
    }
}

impl<'tree> ThinType<'tree> {
    pub const NEVER: Self = Self::never(Ann::Intrinsic);

    pub const NULL: Self = Self::null(Ann::Intrinsic);

    pub const fn never(ann: Ann<'tree>) -> Self {
        Self::Never { ann, nullability: Nullability::NonNullable }
    }

    pub const fn null(ann: Ann<'tree>) -> Self {
        Self::Never { ann, nullability: Nullability::Nullable }
    }

    pub fn dummy_for_hole(ann: Ann<'tree>, nullability: Nullability) -> Self {
        Self::Nominal {
            ann,
            nullability,
            id: IdentType {
                ann,
                name: TypeIdent::<'tree>::DUMMY_FOR_HOLE,
                generic_args: Vec::new(),
            }
        }
    }

    pub fn ident(ann: Ann<'tree>, name: TypeIdent<'tree>) -> Self {
        Self::Nominal {
            ann,
            nullability: Nullability::NonNullable,
            id: IdentType {
                ann,
                name,
                generic_args: Vec::new(),
            }
        }
    }

    pub fn generic(ann: Ann<'tree>, name: TypeIdent<'tree>, generic_args: impl Iterator<Item=Self>) -> Self {
        Self::Nominal {
            ann,
            nullability: Nullability::NonNullable,
            id: IdentType {
                ann,
                name,
                generic_args: generic_args.collect(),
            }
        }
    }

    pub fn ident2(ann: Ann<'tree>, name: &str) -> Self {
        match name {
            "Any" => Self::Any { ann },
            "Never" => Self::never(ann),
            "Null" => Self::null(ann),
            "Void" => Self::IllegalVoid { ann },
            _ => Self::ident(ann, TypeIdent::of(ann, name))
        }
    }

    pub fn generic2(ann: Ann<'tree>, name: &str, generic_args: impl Iterator<Item=Self>) -> Self {
        Self::generic(ann, TypeIdent::of(ann, name), generic_args)
    }

    impl_structural_type_constructors!('tree);
}

impl<'tree> HasNullability for ThinType<'tree> {
    fn nullability(&self) -> Nullability {
        match self {
            Self::Any { .. } => Nullability::Nullable,
            Self::Never { nullability, .. } => *nullability,
            Self::Structural { nullability, .. } => *nullability,
            Self::Nominal { nullability, .. } => *nullability,
            Self::IllegalVoid { .. } => Nullability::NonNullable,
        }
    }

    fn make_nullable(&mut self) {
        match self {
            Self::Any { .. } => {},
            Self::Never { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::Nullable,
            Self::IllegalVoid { .. } => {},
        }
    }
}

impl<'tree> TypeTrait<'tree> for ThinType<'tree> {
    type Inherited = Vec<ThinType<'tree>>;
    type TypeArg = ThinType<'tree>;
    type RestArg = ThinType<'tree>;

    fn is_any(&self) -> bool {
        matches!(self, Self::Any)
    }

    fn is_never(&self) -> bool {
        matches!(self, Self::Never { nullability: Nullability::NonNullable })
    }

    fn map_inherited(
        mut inherited: Self::Inherited,
        mut f: impl FnMut(Self) -> Self
    ) -> Self::Inherited {
        for inherited in inherited.iter_mut() {
            replace_with_or_default(inherited, &mut f);
        }
        inherited
    }

    fn map_ref_inherited(
        inherited: &Self::Inherited,
        f: impl FnMut(&Self) -> Self,
    ) -> Self::Inherited {
        inherited.iter().map(f).collect()
    }

    fn map_type_arg(type_arg: Self::TypeArg, mut f: impl FnMut(Self) -> Self) -> Self::TypeArg where Self: Sized {
        f(type_arg)
    }

    fn map_ref_type_arg(type_arg: &Self::TypeArg, mut f: impl FnMut(&Self) -> Self) -> Self::TypeArg {
        f(type_arg)
    }

    fn map_rest_arg_type(rest_arg_type: Self::RestArg, mut f: impl FnMut(Self) -> Self) -> Self::RestArg {
        f(rest_arg_type)
    }

    fn map_ref_rest_arg_type(rest_arg_type: &Self::RestArg, mut f: impl FnMut(&Self) -> Self) -> Self::RestArg {
        f(rest_arg_type)
    }
}

impl<'tree> InheritedTrait<'tree> for Vec<ThinType<'tree>> {
    fn is_empty_inherited(&self) -> bool {
        self.is_empty()
    }
}

impl<'tree> TypeArgTrait<'tree> for ThinType<'tree> {
    type Type = ThinType<'tree>;

    fn type_(&self) -> &Self::Type {
        self
    }

    fn type_mut(&mut self) -> &mut Self::Type {
        self
    }

    fn into_type(self) -> Self::Type where Self::Type: Sized {
        self
    }
}

impl<'tree> RestArgTrait<'tree> for ThinType<'tree> {
    fn is_empty_rest_arg(&self) -> bool {
        matches!(self, Self::Structural {
            nullability: Nullability::NonNullable,
            structure: StructureType::Tuple { element_types }
        } if element_types.is_empty())
    }
}

impl<'tree> TypeParam<ThinType<'tree>> {
    /// Converts this into the nominal type it declares: for [ThinType], just a nominal type with
    /// the name
    pub fn into_type(self) -> ThinType<'tree> {
        ThinType::Nominal {
            ann: self.ann,
            nullability: Nullability::NonNullable,
            id: IdentType {
                ann: self.ann,
                name: self.name,
                // No HKTs
                generic_args: Vec::new()
            }
        }
    }
}

impl<'tree, Type: TypeTrait<'tree>> IdentType<'tree, Type> {
    pub fn map<NewType: TypeTraitMapsFrom<'tree, Type>>(self, mut f: impl FnMut(Type) -> NewType) -> IdentType<'tree, NewType> {
        IdentType {
            ann: self.ann,
            name: self.name,
            generic_args: self.generic_args.into_iter().map(|arg| <NewType as TypeTraitMapsFrom<'tree, Type>>::map_type_arg(arg, &mut f)).collect()
        }
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<'tree, Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> IdentType<'tree, NewType> {
        IdentType {
            ann: self.ann,
            name: self.name.clone(),
            generic_args: self.generic_args.iter().map(|arg| <NewType as TypeTraitMapsFrom<'tree, Type>>::map_ref_type_arg(arg, &mut f)).collect(),
        }
    }
}

impl<'tree, Type: TypeTrait<'tree>> TypeParam<'tree, Type> {
    pub fn map<NewType: TypeTraitMapsFrom<'tree, Type>>(self, f: impl FnMut(Type) -> NewType) -> TypeParam<'tree, NewType> {
        TypeParam {
            ann: self.ann,
            variance_bound: self.variance_bound,
            name: self.name,
            supers: <NewType as TypeTraitMapsFrom<'tree, Type>>::map_inherited(self.supers, f),
        }
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<'tree, Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> TypeParam<'tree, NewType> {
        TypeParam {
            ann: self.ann,
            variance_bound: self.variance_bound,
            name: self.name.clone(),
            supers: <NewType as TypeTraitMapsFrom<'tree, Type>>::map_ref_inherited(&self.supers, |x| f(&x)),
        }
    }
}

impl<'tree, Type: TypeTrait<'tree>> StructureType<'tree, Type> {
    /// Get the structure-kind AKA enum variant.
    ///
    /// This is different than the formal definition of "kind" as in "higher-kinded types".
    pub fn kind(&self) -> StructureKind {
        match self {
            StructureType::Fn { .. } => StructureKind::Fn,
            StructureType::Array { .. } => StructureKind::Array,
            StructureType::Tuple { .. } => StructureKind::Tuple,
            StructureType::Object { .. } => StructureKind::Object,
        }
    }

    /// If this is an array, returns the element type. Otherwise `None`
    pub fn into_array_element_type(self) -> Option<Type> {
        match self {
            StructureType::Array { element_type, .. } => Some(*element_type),
            _ => None,
        }
    }

    /// If this is a tuple, returns the element types. Otherwise `None`
    pub fn into_tuple_element_types(self) -> Option<Vec<OptionalType<'tree, Type>>> {
        match self {
            StructureType::Tuple { element_types, ..  } => Some(element_types),
            _ => None,
        }
    }

    /// If this is an object, returns the field types. Otherwise `None`
    pub fn into_object_field_types(self) -> Option<Vec<Field<'tree, OptionalType<'tree, Type>>>> {
        match self {
            StructureType::Object { field_types, .. } => Some(field_types),
            _ => None,
        }
    }

    pub fn map<NewType: TypeTraitMapsFrom<'tree, Type>>(self, mut f: impl FnMut(Type) -> NewType) -> StructureType<'tree, NewType> {
        match self {
            StructureType::Fn(fn_type) => {
                StructureType::Fn(Box::new(fn_type.map(f)))
            },
            StructureType::Array { ann, element_type } => StructureType::Array {
                ann,
                element_type: Box::new(f(*element_type)),
            },
            StructureType::Tuple { ann, element_types } => StructureType::Tuple {
                ann,
                element_types: element_types.into_iter().map(|elem| elem.map(&mut f)).collect(),
            },
            StructureType::Object { ann, field_types } => StructureType::Object {
                ann,
                field_types: field_types.into_iter().map(|field| field.map(|opt| opt.map(&mut f))).collect(),
            },
        }
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<'tree, Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> StructureType<'tree, NewType> {
        match self {
            StructureType::Fn(fn_type) => {
                StructureType::Fn(Box::new(fn_type.map_ref(f)))
            },
            StructureType::Array { ann, element_type } => StructureType::Array {
                ann: *ann,
                element_type: Box::new(f(element_type)),
            },
            StructureType::Tuple { ann, element_types } => StructureType::Tuple {
                ann: *ann,
                element_types: element_types.iter().map(|elem| elem.map_ref(&mut f)).collect(),
            },
            StructureType::Object { ann, field_types } => StructureType::Object {
                ann: *ann,
                field_types: field_types.iter().map(|field| field.map_ref(|opt| opt.map_ref(&mut f))).collect(),
            },
        }
    }
}

impl<'tree, Type: TypeTrait + Hash> Hash for StructureType<'tree, Type> where Type::RestArg: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            StructureType::Fn(fn_type) => {
                state.write_u8(0);
                fn_type.hash(state);
            }
            StructureType::Array { ann, element_type } => {
                state.write_u8(1);
                ann.hash(state);
                element_type.hash(state);
            }
            StructureType::Tuple { ann, element_types } => {
                state.write_u8(2);
                ann.hash(state);
                element_types.hash(state);
            }
            StructureType::Object { ann, field_types } => {
                state.write_u8(3);
                ann.hash(state);
                field_types.hash(state);
            }
        }
    }
}

impl<'tree, Type: TypeTrait<'tree>> FnType<'tree, Type> {
    pub fn new(
        ann: Ann<'tree>,
        type_params: impl IntoIterator<Item=TypeParam<'tree, Type>>,
        this_type: Type,
        arg_types: impl IntoIterator<Item=OptionalType<'tree, Type>>,
        rest_arg_type: Type::RestArg,
        return_type: ReturnType<'tree, Type>
    ) -> Self {
        FnType {
            ann,
            type_params: type_params.into_iter().collect(),
            this_type,
            arg_types: arg_types.into_iter().collect(),
            rest_arg_type,
            return_type,
        }
    }

    pub fn map<NewType: TypeTraitMapsFrom<'tree, Type>>(self, mut f: impl FnMut(Type) -> NewType) -> FnType<'tree, NewType> {
        let type_params = self.type_params.into_iter().map(|param| param.map(&mut f)).collect();
        let this_type = f(self.this_type);
        let arg_types = self.arg_types.into_iter().map(|arg| arg.map(&mut f)).collect();
        let return_type = self.return_type.map(&mut f);
        NewType::map_rest_arg_type_in_fn(
            self.ann,
            type_params,
            this_type,
            arg_types,
            self.rest_arg_type,
            return_type,
            f
        )
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<'tree, Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> FnType<'tree, NewType> {
        let type_params = self.type_params.iter().map(|param| param.map_ref(&mut f)).collect();
        let this_type = f(&self.this_type);
        let arg_types = self.arg_types.iter().map(|arg| arg.map_ref(&mut f)).collect();
        let return_type = self.return_type.map_ref(&mut f);
        NewType::map_ref_rest_arg_type_in_fn(
            self.ann,
            type_params,
            this_type,
            arg_types,
            &self.rest_arg_type,
            return_type,
            |x| f(&x)
        )
    }

    pub fn with_return_type(self, return_type: ReturnType<'tree, Type>) -> Self {
        FnType {
            ann: self.ann,
            type_params: self.type_params,
            this_type: self.this_type,
            arg_types: self.arg_types,
            rest_arg_type: self.rest_arg_type,
            return_type,
        }
    }
}

impl<'tree, Type: TypeTrait + Hash> Hash for FnType<'tree, Type> where Type::RestArg: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Don't need to hash everything, and 2 functions will rarely differ in only type-params
        // (also requires more boilerplate...).
        // We should look for other cases to try and hash less without running into conflucts
        // self.type_params.hash(state);
        self.this_type.hash(state);
        self.arg_types.hash(state);
        self.rest_arg_type.hash(state);
        self.return_type.hash(state);
    }
}

impl<'tree, Type> OptionalType<'tree, Type> {
    pub fn new(ann: Ann<'tree>, type_: Type, is_optional: bool) -> Self {
        Self {
            ann,
            optionality: Optionality::from(is_optional),
            type_,
        }
    }

    pub fn required(ann: Ann<'tree>, type_: Type) -> Self {
        Self {
            ann,
            optionality: Optionality::Required,
            type_,
        }
    }

    pub fn optional(ann: Ann<'tree>, type_: Type) -> Self {
        Self {
            ann,
            optionality: Optionality::Required,
            type_,
        }
    }

    /// Like [HasNullability::make_nullable] but for optional types
    pub fn make_optional(&mut self) {
        self.optionality = Optionality::Optional;
    }

    /// Like [HasNullability::make_nullable_if] but for optional types
    pub fn make_optional_if(&mut self, condition: bool) {
        if condition {
            self.make_optional();
        }
    }

    pub fn map<'tree, NewType>(self, f: impl FnOnce(Type) -> NewType) -> OptionalType<'tree, NewType> {
        OptionalType {
            ann: self.ann,
            optionality: self.optionality,
            type_: f(self.type_),
        }
    }

    pub fn map_ref<'tree, NewType>(&self, f: impl FnOnce(&Type) -> NewType) -> OptionalType<'tree, NewType> {
        OptionalType {
            ann: self.ann,
            optionality: self.optionality,
            type_: f(&self.type_),
        }
    }

    pub fn as_ref(&self) -> OptionalType<&Type> {
        OptionalType {
            ann: self.ann,
            optionality: self.optionality,
            type_: &self.type_,
        }
    }
}

impl<'tree, Type: Clone> OptionalType<&Type> {
    pub fn cloned(self) -> OptionalType<'tree, Type> {
        OptionalType {
            ann: self.ann,
            optionality: self.optionality,
            type_: self.type_.clone(),
        }
    }
}

impl<'tree, Type: HasNullability> OptionalType<'tree, Type> {
    /// If optional, returns the inner type nullable. Otherwise just the inner type.
    /// Optionality is very similar to nullability, the difference is that you can completely omit
    /// providing optional-typed values at the end of an argument list or tuple
    pub fn collapse_optionality_into_nullability(mut self) -> Type {
        match self.optionality {
            Optionality::Required => {},
            Optionality::Optional => { self.type_.make_nullable(); },
        }
        self.type_.ann_mut() = self.ann;
        self.type_
    }
}

impl<'tree, Type> ReturnType<'tree, Type> {
    pub fn map<'tree, NewType>(self, f: impl FnOnce(Type) -> NewType) -> ReturnType<'tree, NewType> {
        match self {
            ReturnType::Type(type_) => ReturnType::Type(f(type_)),
            ReturnType::Void => ReturnType::Void,
        }
    }

    pub fn map_ref<'tree, NewType>(&self, f: impl FnOnce(&Type) -> NewType) -> ReturnType<'tree, NewType> {
        match self {
            ReturnType::Type(type_) => ReturnType::Type(f(type_)),
            ReturnType::Void => ReturnType::Void,
        }
    }
}

impl<'tree, Type> From<Type> for ReturnType<'tree, Type> {
    fn from(value: Type) -> Self {
        ReturnType::Type(value)
    }
}

impl<'tree, Type> Field<Type> {
    pub fn map<'tree, NewType>(self, f: impl FnOnce(Type) -> NewType) -> Field<'tree, NewType> {
        Field {
            ann: self.ann,
            name: self.name,
            type_: f(self.type_),
        }
    }

    pub fn map_ref<'tree, NewType>(&self, f: impl FnOnce(&Type) -> NewType) -> Field<'tree, NewType> {
        Field {
            ann: self.ann,
            name: self.name.clone(),
            type_: f(&self.type_),
        }
    }
}

impl<'tree, Type: Default> Default for ReturnType<'tree, Type> {
    fn default() -> Self {
        Self::Type(Default::default())
    }
}

impl Variance {
    pub fn new(is_covariant: bool, is_contravariant: bool) -> Self {
        match (is_covariant, is_contravariant) {
            (true, true) => Variance::Bivariant,
            (true, false) => Variance::Covariant,
            (false, true) => Variance::Contravariant,
            (false, false) => Variance::Invariant,
        }
    }

    pub fn reversed(self) -> Self {
        match self {
            Variance::Bivariant => Variance::Bivariant,
            Variance::Covariant => Variance::Contravariant,
            Variance::Contravariant => Variance::Covariant,
            Variance::Invariant => Variance::Invariant,
        }
    }

    pub fn is_covariant(self) -> bool {
        match self {
            Variance::Bivariant => true,
            Variance::Covariant => true,
            Variance::Contravariant => false,
            Variance::Invariant => false,
        }
    }

    pub fn is_contravariant(self) -> bool {
        match self {
            Variance::Bivariant => true,
            Variance::Covariant => false,
            Variance::Contravariant => true,
            Variance::Invariant => false,
        }
    }

    pub fn do_union(self) -> bool {
        self.is_covariant()
    }

    /// Applies variance bound `bound` onto `self`.
    /// TODO: Also check for variance bounds which are less restricted than the actual inferred
    ///   variance, but in another place
    pub fn bounded(self, bound: Variance) -> Self {
        match bound {
            Variance::Bivariant => Variance::Bivariant,
            Variance::Covariant => self,
            Variance::Contravariant => self.reversed(),
            Variance::Invariant => Variance::Invariant
        }
    }
}

impl PartialOrd for Variance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Variance::Invariant, Variance::Invariant) => Some(Ordering::Equal),
            (Variance::Invariant, _) => Some(Ordering::Less),
            (_, Variance::Invariant) => Some(Ordering::Greater),
            (Variance::Bivariant, Variance::Bivariant) => Some(Ordering::Equal),
            (Variance::Bivariant, _) => Some(Ordering::Greater),
            (_, Variance::Bivariant) => Some(Ordering::Less),
            (Variance::Covariant, Variance::Covariant) => Some(Ordering::Equal),
            (Variance::Covariant, Variance::Contravariant) => None,
            (Variance::Contravariant, Variance::Contravariant) => Some(Ordering::Equal),
            (Variance::Contravariant, Variance::Covariant) => None,
        }
    }
}

impl BitOr for Variance {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        match (self, other) {
            (Variance::Invariant, other) => other,
            (this, Variance::Invariant) => this,
            (Variance::Bivariant, _) => Variance::Bivariant,
            (_, Variance::Bivariant) => Variance::Bivariant,
            (Variance::Covariant, Variance::Covariant) => Variance::Covariant,
            (Variance::Contravariant, Variance::Contravariant) => Variance::Contravariant,
            (Variance::Covariant, Variance::Contravariant) => Variance::Bivariant,
            (Variance::Contravariant, Variance::Covariant) => Variance::Bivariant,
        }
    }
}

impl BitOrAssign for Variance {
    fn bitor_assign(&mut self, other: Self) {
        *self = *self | other;
    }
}

impl BitAnd for Variance {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        match (self, other) {
            (Variance::Invariant, _) => Variance::Invariant,
            (_, Variance::Invariant) => Variance::Invariant,
            (Variance::Bivariant, other) => other,
            (this, Variance::Bivariant) => this,
            (Variance::Covariant, Variance::Covariant) => Variance::Covariant,
            (Variance::Contravariant, Variance::Contravariant) => Variance::Contravariant,
            (Variance::Covariant, Variance::Contravariant) => Variance::Invariant,
            (Variance::Contravariant, Variance::Covariant) => Variance::Invariant,
        }
    }
}

impl BitAndAssign for Variance {
    fn bitand_assign(&mut self, other: Self) {
        *self = *self & other;
    }
}

impl StructureKind {
    pub fn display(self) -> &'static str {
        match self {
            StructureKind::Fn => "function",
            StructureKind::Array => "array",
            StructureKind::Tuple => "tuple",
            StructureKind::Object => "object",
        }
    }

    pub fn a_display(self) -> &'static str {
        match self {
            StructureKind::Fn => "a function",
            StructureKind::Array => "an array",
            StructureKind::Tuple => "a tuple",
            StructureKind::Object => "an object",
        }
    }
}

impl From<bool> for Optionality {
    fn from(is_optional: bool) -> Self {
        if is_optional {
            Self::Optional
        } else {
            Self::Required
        }
    }
}

impl BitOr for Optionality {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Optionality::Required, Optionality::Required) => Optionality::Required,
            _ => Optionality::Optional,
        }
    }
}

impl BitOrAssign for Optionality {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitAnd for Optionality {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Optionality::Optional, Optionality::Optional) => Optionality::Optional,
            _ => Optionality::Required,
        }
    }
}

impl BitAndAssign for Optionality {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl From<bool> for Nullability {
    fn from(is_nullable: bool) -> Self {
        if is_nullable {
            Self::Nullable
        } else {
            Self::NonNullable
        }
    }
}

impl BitOr for Nullability {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Nullability::NonNullable, Nullability::NonNullable) => Nullability::NonNullable,
            _ => Nullability::Nullable,
        }
    }
}

impl BitOrAssign for Nullability {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitAnd for Nullability {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Nullability::Nullable, Nullability::Nullable) => Nullability::Nullable,
            _ => Nullability::NonNullable,
        }
    }
}

impl BitAndAssign for Nullability {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl<F: FnOnce() -> Nullability> BitOr<F> for Nullability {
    type Output = Self;

    fn bitor(self, rhs: F) -> Self::Output {
        match self {
            Nullability::NonNullable => Nullability::NonNullable,
            Nullability::Nullable => rhs(),
        }
    }
}