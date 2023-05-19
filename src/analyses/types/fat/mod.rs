use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::iter::{once, repeat};
use std::ops::Range;
use std::rc::Rc;

pub use data::*;

use crate::analyses::bindings::TypeName;
use crate::analyses::types::{FnType, HasNullability, InheritedTrait, Nullability, Optionality, OptionalType, RestArgTrait, ReturnType, StructureKind, ThinType, TypeArgTrait, IdentType, TypeParam, StructureType, TypeTrait, TypeTraitMapsFrom, Variance};
use crate::analyses::types::generic::{FnType, HasNullability, IdentType, impl_structural_type_constructors, InheritedTrait, Nullability, OptionalType, RestArgTrait, ReturnType, StructureKind, StructureType, TypeArgTrait, TypeParam, TypeTrait, TypeTraitMapsFrom, Variance};
use crate::type_sitter::ann::{Ann, HasAnn};
use crate::diagnostics::TypeLogger;
use crate::misc::{once_if, rc_unwrap_or_clone};

mod data;
mod display;
mod occurrences;
mod unify;
/// Field type, element type, promise inner type
mod inner;
/// We try to make [FatType] canonical so normalization usually shouldn't be necessary / do much
mod normalize;

impl<'tree> FatTypeDecl<'tree> {
    pub fn missing(ann: Ann<'tree>) -> Self {
        Self {
            ann,
            name: TypeName::MISSING,
            type_params: Vec::new(),
            inherited: Box::new(FatTypeInherited::empty())
        }
    }

    /// Converts this into the nominal type it declares: for [FatType], a nominal type with
    /// the name, type parameters, and inherited
    pub fn into_type(self) -> FatType<'tree> {
        FatType::Nominal {
            ann: self.ann,
            nullability: Nullability::NonNullable,
            id: IdentType {
                ann: self.name.ann,
                name: self.name,
                generic_args: self.type_params.into_iter().map(|x| x.into_type()).collect(),
            },
            inherited: self.inherited
        }
    }
}

impl<'tree> Default for FatTypeDecl {
    fn default() -> Self {
        Self::missing(Ann::INHERITED)
    }
}

impl<'tree> FatType<'tree> {
    pub const NEVER: Self = Self::never(Ann::INHERITED);

    pub const NULL: Self = Self::null(Ann::INHERITED);

    pub fn never(ann: Ann<'tree>) -> Self {
        Self::Never { ann, nullability: Nullability::NonNullable }
    }

    pub fn null(ann: Ann<'tree>) -> Self {
        Self::Never { ann, nullability: Nullability::Nullable }
    }

    impl_structural_type_constructors!('tree);

    pub fn hole(ann: Ann<'tree>) -> Self {
        Self::Hole {
            ann,
            nullability: Nullability::NonNullable,
            hole: FatTypeHole::new(ann)
        }
    }

    /// Make nullable. Makes type holes' upper bound nullable, but not outer nullable.
    pub fn make_nullable_intrinsically(&mut self) {
        match self {
            Self::Any { .. } => {},
            Self::Never { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Hole { nullability: _, hole, .. } => {
                hole.upper_bound.borrow_mut().make_nullable_intrinsically();
            },
        }
    }

    /// Make non-nullable. Make type holes' upper bound non-nullable *and* outer non-nullable
    pub fn make_non_nullable_intrinsically(&mut self) {
        match self {
            Self::Any { .. } => {},
            Self::Never { nullability, .. } => *nullability = Nullability::NonNullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::NonNullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::NonNullable,
            Self::Hole { nullability, hole, .. } => {
                *nullability = Nullability::NonNullable;
                hole.upper_bound.borrow_mut().make_non_nullable_intrinsically();
            },
        }
    }

    /// Structure kind, if there is a structure, otherwise `None`
    pub fn structure_kind(&self) -> Option<StructureKind> {
        match self {
            Self::Any { .. } => None,
            Self::Never { .. } => None,
            Self::Structural { structure, .. } => Some(structure.kind()),
            Self::Nominal { inherited, .. } => inherited.structure.as_ref().map(|s| s.kind()),
            Self::Hole { hole, .. } => hole.upper_bound.borrow().structure_kind()
        }
    }

    /// Temporarily access the type id and supertypes.
    ///
    /// If this is a type hole, it must borrow the id and supertypes for the duration, so attempting
    /// to modify will **panic**.
    pub fn with_idents<R>(
        &self,
        fun: impl FnOnce(Option<std::iter::Chain<std::iter::Once<&IdentType<'tree, FatType<'tree>>>, std::collections::vec_deque::Iter<'_, IdentType<'tree, FatType<'tree>>>>>) -> R
    ) -> R {
        match self {
            Self::Any { .. } => fun(None),
            Self::Never { .. } => fun(None),
            Self::Structural { .. } => fun(None),
            Self::Nominal { id, inherited, ..  } => fun(Some(once(id).chain(&inherited.super_ids))),
            Self::Hole { hole, .. } => hole.upper_bound.borrow().with_idents(fun)
        }
    }

    /// Temporarily access the type structure.
    ///
    /// If this is a type hole, it must borrow the structure for the duration, so attempting to
    /// modify will **panic**.
    pub fn with_structure<R>(&self, fun: impl FnOnce(Option<&StructureType<FatType<'tree>>>) -> R) -> R {
        match self {
            Self::Any { .. } => fun(None),
            Self::Never { .. } => fun(None),
            Self::Structural { structure, .. } => fun(Some(structure)),
            Self::Nominal { inherited, .. } => fun(inherited.structure.as_ref()),
            Self::Hole { hole, .. } => hole.upper_bound.borrow().with_structure(fun)
        }
    }

    /// Temporarily access the type's function structure if it's a function type.
    ///
    /// If this is a type hole, it must borrow the structure for the duration, so attempting to
    /// modify will **panic**.
    pub fn with_function_structure<R>(&self, fun: impl FnOnce(Option<&FnType<'tree, FatType<'tree>>>) -> R) -> R {
        self.with_structure(|structure| fun(match structure {
            Some(StructureType::Fn(fn_type)) => Some(fn_type),
            _ => None
        }))
    }

    /// Destructs this and returns its annotation, nullability, ids and structure
    pub fn into_ann_nullability_ids_and_structure(self) -> (Ann<'tree>, Nullability, Vec<IdentType<'tree, FatType<'tree>>>, Option<StructureType<FatType<'tree>>>) {
        match self {
            Self::Any { ann } => {
                (ann, Nullability::Nullable, Vec::new(), None)
            },
            Self::Never { ann, nullability } => {
                (ann, nullability, Vec::new(), None)
            },
            Self::Structural { ann, nullability, structure } => {
                (ann, nullability, Vec::new(), Some(structure))
            },
            Self::Nominal { ann, nullability, id, inherited } => {
                let mut ids = vec![id];
                ids.extend(inherited.super_ids.into_iter());
                (ann, nullability, ids, inherited.structure)
            },
            Self::Hole { ann, nullability, hole } => {
                let (inner_nullability, ids, structure) = rc_unwrap_or_clone(hole.upper_bound).into_inner().into_nullability_ids_and_structure();
                (ann, nullability | inner_nullability, ids, structure)
            }
        }
    }

    /// Destructs this and returns its ids
    pub fn into_ids(self) -> Vec<IdentType<'tree, FatType<'tree>>> {
        let (_, _, ids, _) = self.into_ann_nullability_ids_and_structure();
        ids
    }

    /// Destructs this and returns its structure
    pub fn into_structure(self) -> Option<StructureType<FatType<'tree>>> {
        let (_, _, _, structure) = self.into_ann_nullability_ids_and_structure();
        structure
    }
}

impl<'tree> HasNullability for FatType<'tree> {
    fn nullability(&self) -> Nullability {
        match self {
            Self::Any { .. } => Nullability::Nullable,
            Self::Never { nullability, .. } => *nullability,
            Self::Structural { nullability, .. } => *nullability,
            Self::Nominal { nullability, .. } => *nullability,
            Self::Hole { nullability, hole, .. } => *nullability | || hole.upper_bound.borrow().nullability()
        }
    }

    fn make_nullable(&mut self) {
        match self {
            Self::Any { .. } => {},
            Self::Never { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Hole { nullability, .. } => *nullability = Nullability::Nullable,
        }
    }
}

impl<'tree> TypeTrait<'tree> for FatType<'tree> {
    type Inherited = Box<FatTypeInherited<'tree>>;
    type TypeArg = FatTypeArg<'tree>;
    type RestArg = FatRestArgType<'tree>;

    fn is_any(&self) -> bool {
        matches!(self, FatType::Any)
    }

    fn is_never(&self) -> bool {
        matches!(self, FatType::Never { nullability: Nullability::NonNullable })
    }

    fn map_inherited(inherited: Self::Inherited, f: impl FnMut(Self) -> Self) -> Self::Inherited {
        Box::new(inherited.map(f))
    }

    fn map_ref_inherited(inherited: &Self::Inherited, f: impl FnMut(&Self) -> Self) -> Self::Inherited {
        Box::new(inherited.map_ref(f))
    }

    fn map_type_arg(type_arg: Self::TypeArg, mut f: impl FnMut(Self) -> Self) -> Self::TypeArg where Self: Sized {
        FatTypeArg {
            variance_bound: type_arg.variance_bound,
            type_: f(type_arg.type_)
        }
    }

    fn map_ref_type_arg(type_arg: &Self::TypeArg, mut f: impl FnMut(&Self) -> Self) -> Self::TypeArg {
        FatTypeArg {
            variance_bound: type_arg.variance_bound,
            type_: f(&type_arg.type_)
        }
    }

    fn map_rest_arg_type(rest_arg_type: Self::RestArg, f: impl FnMut(Self) -> Self) -> Self::RestArg {
        rest_arg_type.map(f)
    }

    fn map_ref_rest_arg_type(rest_arg_type: &Self::RestArg, f: impl FnMut(&Self) -> Self) -> Self::RestArg {
        rest_arg_type.map_ref(f)
    }
}

// TODO: Don't implement this trait at all, instead implement it on a wrapper since it's a shallow conversion
impl<'tree> TypeTraitMapsFrom<ThinType<'tree>> for FatType<'tree> {
    fn map_inherited(
        inherited: <ThinType<'tree> as TypeTrait<'tree>>::Inherited,
        f: impl FnMut(ThinType<'tree>) -> Self
    ) -> Self::Inherited {
        let inherited = inherited.into_iter().map(f).collect::<Vec<_>>();
        Box::new(Self::unify_all_supers(inherited, TypeLogger::ignore()))
    }

    fn map_ref_inherited(
        inherited: &<ThinType<'tree> as TypeTrait<'tree>>::Inherited,
        mut f: impl FnMut(Cow<'_, ThinType<'tree>>) -> Self
    ) -> Self::Inherited {
        let inherited = inherited.iter().map(|x| f(Cow::Borrowed(x))).collect::<Vec<_>>();
        Box::new(Self::unify_all_supers(inherited, TypeLogger::ignore()))
    }

    fn map_type_arg(
        type_arg: <ThinType<'tree> as TypeTrait<'tree>>::TypeArg,
        mut f: impl FnMut(ThinType<'tree>) -> Self
    ) -> Self::TypeArg {
        FatTypeArg {
            variance_bound: Variance::default(),
            type_: f(type_arg)
        }
    }

    fn map_ref_type_arg(
        type_arg: &<ThinType<'tree> as TypeTrait<'tree>>::TypeArg,
        mut f: impl FnMut(&ThinType<'tree>) -> Self
    ) -> Self::TypeArg {
        FatTypeArg {
            variance_bound: Variance::default(),
            type_: f(type_arg)
        }
    }

    fn map_rest_arg_type_in_fn(
        ann: Ann<'tree>,
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        mut arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: ThinType,
        return_type: ReturnType<Self>,
        mut f: impl FnMut(ThinType<'tree>) -> Self
    ) -> FnType<Self> {
        let intended_rest_arg_type = f(rest_arg_type);
        let rest_arg_type = FatRestArgType::from(intended_rest_arg_type, &mut arg_types);
        FnType {
            ann,
            type_params,
            this_type,
            arg_types,
            rest_arg_type,
            return_type
        }
    }

    fn map_ref_rest_arg_type_in_fn(
        ann: Ann<'tree>,
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        mut arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: &ThinType,
        return_type: ReturnType<Self>,
        mut f: impl FnMut(Cow<'_, ThinType<'tree>>) -> Self
    ) -> FnType<Self> {
        let intended_rest_arg_type = f(Cow::Borrowed(rest_arg_type));
        let rest_arg_type = FatRestArgType::from(intended_rest_arg_type, &mut arg_types);
        FnType {
            ann,
            type_params,
            this_type,
            arg_types,
            rest_arg_type,
            return_type
        }
    }
}

impl<'tree> TypeTraitMapsFrom<FatType<'tree>> for ThinType<'tree> {
    fn map_inherited(
        inherited: <FatType<'tree> as TypeTrait<'tree>>::Inherited,
        f: impl FnMut(FatType<'tree>) -> Self
    ) -> Self::Inherited {
        inherited.into_bare_fat_types().map(f).collect()
    }

    fn map_ref_inherited(
        inherited: &<FatType<'tree> as TypeTrait<'tree>>::Inherited,
        mut f: impl FnMut(Cow<'_, FatType<'tree>>) -> Self
    ) -> Self::Inherited {
        inherited.clone().into_bare_fat_types().map(|x| f(Cow::Owned(x))).collect()
    }

    fn map_type_arg(
        type_arg: <FatType<'tree> as TypeTrait<'tree>>::TypeArg,
        mut f: impl FnMut(FatType<'tree>) -> Self
    ) -> Self::TypeArg {
        f(type_arg.type_)
    }

    fn map_ref_type_arg(
        type_arg: &<FatType<'tree> as TypeTrait<'tree>>::TypeArg,
        mut f: impl FnMut(&FatType<'tree>) -> Self
    ) -> Self::TypeArg {
        f(&type_arg.type_)
    }

    fn map_rest_arg_type_in_fn(
        ann: Ann<'tree>,
        type_params: Vec<TypeParam<'tree, Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<'tree, Self>>,
        rest_arg_type: FatRestArgType<'tree>,
        return_type: ReturnType<'tree, Self>,
        mut f: impl FnMut(FatType<'tree>) -> Self
    ) -> FnType<Self> {
        FnType {
            ann,
            type_params,
            this_type,
            arg_types,
            rest_arg_type: f(rest_arg_type.into()),
            return_type
        }
    }

    fn map_ref_rest_arg_type_in_fn(
        ann: Ann<'tree>,
        type_params: Vec<TypeParam<'tree, Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<'tree, Self>>,
        rest_arg_type: &FatRestArgType<'tree>,
        return_type: ReturnType<'tree, Self>,
        mut f: impl FnMut(Cow<'_, FatType<'tree>>) -> Self
    ) -> FnType<Self> {
        FnType {
            ann,
            type_params,
            this_type,
            arg_types,
            rest_arg_type: f(rest_arg_type.as_cow()),
            return_type
        }
    }
}

impl<'tree> FnType<'tree, FatType<'tree>> {
    /// Min and max number of arguments.
    /// Returns `usize::MAX` for unbounded max arguments (array rest type)
    pub fn arity_range(&self) -> Range<usize> {
        let min = self.arg_types.len() - self.arg_types.iter().rev().take_while(|x| matches!(x.optionality, Optionality::Optional)).count();
        let max = match self.rest_arg_type {
            FatRestArgType::None => self.arg_types.len(),
            FatRestArgType::Array { .. } => usize::MAX,
            FatRestArgType::Illegal { .. } => self.arg_types.len()
        };
        min..max
    }
}

impl<'tree> TypeParam<FatType<'tree>> {
    pub fn into_decl(self) -> FatTypeDecl<'tree> {
        FatTypeDecl {
            ann: self.ann,
            name: self.name,
            // No higher-kinded types
            type_params: Vec::new(),
            inherited: self.supers
        }
    }

    /// Converts this into the nominal type it declares: for [FatType], just a nominal type with the
    /// name, inherited, and variance bound
    pub fn into_type(self) -> FatTypeArg<'tree> {
        FatTypeArg {
            variance_bound: self.variance_bound,
            type_: FatType::Nominal {
                ann: self.ann,
                nullability: Nullability::NonNullable,
                id: IdentType {
                    ann: self.ann,
                    name: self.name,
                    // No HKTs
                    generic_args: Vec::new()
                },
                inherited: self.supers
            }
        }
    }
}

impl<'tree> FatTypeInherited<'tree> {
    pub const EMPTY: Self = Self::empty();

    /// Also = `default()`
    pub const fn empty() -> Self {
        Self {
            super_ids: VecDeque::new(),
            structure: None,
            typescript_types: Vec::new(),
            guards: Vec::new(),
            is_never: false
        }
    }

    pub fn map(self, f: impl FnMut(FatType<'tree>) -> FatType<'tree>) -> Self {
        Self {
            super_ids: self.super_ids,
            structure: self.structure.map(|x| x.map(f)),
            typescript_types: self.typescript_types,
            guards: self.guards,
            is_never: self.is_never
        }
    }

    pub fn map_ref(&self, f: impl FnMut(&FatType<'tree>) -> FatType<'tree>) -> Self {
        Self {
            super_ids: self.super_ids.clone(),
            structure: self.structure.as_ref().map(|x| x.map_ref(f)),
            typescript_types: self.typescript_types.clone(),
            guards: self.guards.clone(),
            is_never: self.is_never
        }
    }

    /// Completes this, making it a [FatType]:
    /// - If `is_never` is true, this will either be the `Never` type or `Null` if nullable
    /// - If there is at least one id, it will be a nominal type
    /// - Otherwise, if there is a structure, it will be a structural type
    /// - Otherwise, it will be the `Any` type
    ///
    /// If this becomes `Any`, `Never`, or `Null`, the TypeScript type and guards are stripped,
    /// because presumably they shouldn't be needed (`Any` = structural so no guard and `any` type,
    /// `Never` = no possible instances no guard and `never` type, `Null` = the only possible
    /// instance is `null` so no guard and `null` type)
    pub fn into_fat_type(mut self, ann: Ann<'tree>, nullability: Nullability) -> FatType<'tree> {
        if self.is_never {
            return FatType::Never { ann, nullability };
        }
        if let Some(id) = self.super_ids.pop_front() {
            return FatType::Nominal {
                ann,
                nullability,
                id,
                inherited: Box::new(self)
            };
        }
        if let Some(structure) = self.structure {
            return FatType::Structural {
                ann,
                nullability,
                structure
            };
        }
        FatType::Any { ann }
    }

    /// Converts into bare fat types, i.e. without the inherited parts.
    /// Just a type for never, each identifier, and each structure.
    /// These types convert into thin types but should not be used anything else,
    /// as they have ids with incorrect supertypes.
    fn into_bare_fat_types(self) -> impl Iterator<Item=FatType<'tree>> {
        once_if(self.is_never, || FatType::NEVER)
            .chain(self.super_ids.into_iter().map(|id| FatType::Nominal {
                ann: id.ann,
                nullability: Nullability::NonNullable,
                id,
                inherited: Box::default()
            }))
            .chain(self.structure.map(|structure| FatType::Structural {
                ann: *structure.ann(),
                nullability: Nullability::NonNullable,
                structure
            }).into_iter())
    }
}

impl<'tree> InheritedTrait<'tree> for FatTypeInherited<'tree> {
    fn is_empty_inherited(&self) -> bool {
        !self.is_never &&
            self.super_ids.is_empty() &&
            self.structure.is_none() &&
            self.typescript_types.is_empty() &&
            self.guards.is_empty()
    }
}

impl<'tree> TypeArgTrait<'tree> for FatTypeArg<'tree> {
    type Type = FatType<'tree>;

    fn type_(&self) -> &Self::Type {
        &self.type_
    }

    fn type_mut(&mut self) -> &mut Self::Type {
        &mut self.type_
    }

    fn into_type(self) -> Self::Type {
        self.type_
    }
}

impl<'tree> FatRestArgType<'tree> {
    const EMPTY_REST_ARG_REF: &'static FatType<'static> = &FatType::<'static>::EMPTY_REST_ARG;

    /// Creates from the given [FatType].
    ///
    /// Nominal info is stripped, only the structure if it's `Any` or a tuple or array matters:
    ///
    /// - If it's a tuple, this will return [FatRestArgType::None] but append the elems to the end
    ///   of the args array.
    /// - If it's an array, this will return [FatRestArgType::Array] with the element type,
    ///   meaning the function can take zero or more extre elements of this type
    /// - If it's `Any`, that is equivalent to an array of `Any` (can take extra elements of any type)
    /// - Otherwise, this will return [FatRestArgType::Illegal], which will log an error later if
    ///   the type is actually used somewhere.
    pub fn from(intended_type: FatType, arg_types: &mut Vec<OptionalType<FatType<'tree>>>) -> Self {
        let kind = FatRestArgKind::of(&intended_type);
        match kind {
            FatRestArgKind::Any => Self::Array { ann: *intended_type.ann(), element: FatType::Any },
            FatRestArgKind::Tuple => {
                let mut elements = intended_type.into_structure().and_then(|x| x.into_tuple_element_types())
                    .expect("FatRestArgType::from: intended_type is not a tuple but FatRestArgKind::of returned FatRestArgKind::Tuple");
                arg_types.append(&mut elements);
                Self::None
            }
            FatRestArgKind::Array => {
                let element = intended_type.into_structure().and_then(|x| x.into_array_element_type())
                    .expect("FatRestArgType::from: intended_type is not an array but FatRestArgKind::of returned FatRestArgKind::Array");
                Self::Array { ann: *intended_type.ann(), element }
            }
            FatRestArgKind::Illegal => Self::Illegal { intended_type }
        }
    }

    //Can't really abstract (I mean we could proc-macro this whole map/map_ref thing...)
    //noinspection DuplicatedCode
    pub fn map(self, mut f: impl FnMut(FatType<'tree>) -> FatType<'tree>) -> Self {
        match self {
            Self::None => Self::None,
            Self::Array { ann, element } => Self::Array { ann, element: f(element) },
            Self::Illegal { intended_type } => Self::Illegal { intended_type: f(intended_type) }
        }
    }

    //noinspection DuplicatedCode
    pub fn map_ref(&self, mut f: impl FnMut(&FatType<'tree>) -> FatType<'tree>) -> Self {
        match self {
            Self::None => Self::None,
            Self::Array { ann, element } => Self::Array { ann: *ann, element: f(element) },
            Self::Illegal { intended_type } => Self::Illegal { intended_type: f(intended_type) }
        }
    }

    /// Gets the argument at the specified index, if any.
    ///
    /// (Rest arguments are always optional)
    pub fn get(&self, _idx: usize) -> Option<&FatType<'tree>> {
        match self {
            Self::None => None,
            Self::Array { element, .. } => Some(element),
            Self::Illegal { .. } => None
        }
    }

    /// Iterates the rest arguments: returns infinite element types if this is an array, otherwise empty.
    ///
    /// (Rest arguments are always optional)
    pub fn iter(&self) -> impl Iterator<Item = &FatType<'tree>> {
        // Use option/flatten to avoid auto_enum
        match self {
            Self::None => None,
            Self::Array { element, .. } => Some(repeat(element)),
            Self::Illegal { .. } => None
        }.into_iter().flatten()
    }

    /// Iterates the rest arguments: returns infinite element types if this is an array, otherwise empty.
    ///
    /// (Rest arguments are always optional)
    pub fn into_iter(self) -> impl Iterator<Item = FatType<'tree>> {
        // Use option/flatten to avoid auto_enum
        match self {
            Self::None => None,
            Self::Array { element, .. } => Some(repeat(element)),
            Self::Illegal { .. } => None
        }.into_iter().flatten()
    }

    /// Converts into a [FatType]. If possible this will use a static or internal reference.
    pub fn as_cow(&self) -> Cow<'_, FatType<'tree>> {
        match self {
            Self::None => Cow::Borrowed(Self::EMPTY_REST_ARG_REF),
            Self::Array { ann, element } => Cow::Owned(FatType::array(*ann, element.clone())),
            Self::Illegal { intended_type } => Cow::Borrowed(intended_type)
        }
    }
}

impl<'tree> RestArgTrait<'tree> for FatRestArgType<'tree> {
    fn is_empty_rest_arg(&self) -> bool {
        matches!(self, Self::None)
    }
}

impl<'tree> Into<FatType<'tree>> for FatRestArgType<'tree> {
    fn into(self) -> FatType<'tree> {
        match self {
            Self::None => FatType::EMPTY_REST_ARG,
            Self::Array { ann, element } => FatType::array(ann, element),
            Self::Illegal { intended_type } => intended_type
        }
    }
}

impl FatRestArgKind {
    pub fn of(intended_type: &FatType<'_>) -> Self {
        if matches!(intended_type, FatType::Any) {
            return Self::Any;
        }
        match intended_type.structure_kind() {
            Some(StructureKind::Tuple) => Self::Tuple,
            Some(StructureKind::Array) => Self::Array,
            _ => Self::Illegal
        }
    }
}

impl<'tree> FatTypeHole<'tree> {
    pub fn new(ann: Ann<'tree>) -> Self {
        Self {
            ann,
            upper_bound: Rc::new(RefCell::new(FatType::never(ann))),
        }
    }
}

impl<'tree> PartialEq for FatTypeHole<'tree> {
    fn eq(&self, other: &FatTypeHole<'tree>) -> bool {
        Rc::ptr_eq(&self.upper_bound, &other.upper_bound)
    }
}

impl<'tree> Eq for FatTypeHole<'tree> {}

impl<'tree> Default for FatTypeHole<'tree> {
    fn default() -> Self {
        Self::new(Ann::default())
    }
}

impl<'tree> From<FatType<'tree>> for FatTypeHole<'tree> {
    fn from(t: FatType<'tree>) -> Self {
        Self {
            ann: *t.ann(),
            upper_bound: Rc::new(RefCell::new(t))
        }
    }
}