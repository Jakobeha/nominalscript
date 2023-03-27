use std::cell::RefCell;
use std::iter::once;
use std::rc::Rc;

use replace_with::replace_with;

use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::types::{Field, FnType, impl_structural_type_constructors, NominalGuard, Nullability, OptionalType, ReturnType, StructureKind, ThinType, TypeIdent, TypeLoc, TypeParam, TypeStructure, TypeTrait, TypeTraitMapsFrom, Variance};
use crate::ast::tree_sitter::SubTree;
use crate::diagnostics::TypeLogger;
use crate::{error, note};

/// Type declaration after we've resolved the supertypes
#[derive(Debug, Clone)]
pub struct FatTypeDecl {
    pub name: TypeName,
    pub type_params: Vec<TypeParam<FatType>>,
    /// Boxed because it's large
    pub inherited: Box<FatTypeInherited>
}

/// Fat type = type after we've resolved the supertypes so that they are also in this structure,
/// and fat types can be compared / unified directly
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum FatType {
    /// Top = type of untyped values
    #[default]
    Any,
    /// *non-nullable* `Never` = bottom = type of values which cannot be produced (e.g. loops)
    /// *nullable* `Never` = `Null` = only null
    Never {
        nullability: Nullability,
    },
    /// Type with no nominal id, so a non-nominally wrapped structure can be an instance.
    /// This also means there is no typescript type or guard
    Structural {
        nullability: Nullability,
        structure: TypeStructure<FatType>,
    },
    /// Type with at least one nominal id, so all instances are nominally wrapped.
    /// There may be an additional typescript type and guards as well
    Nominal {
        nullability: Nullability,
        id: TypeIdent<FatType>,
        /// Boxed because it's large
        inherited: Box<FatTypeInherited>
    },
    /// Uninstantiated generic: equivalent to `Never` if never unified,
    /// but when unified, it becomes the type it was unified with and stays that way.
    ///
    /// Note that it is not `==` to the equivalent fat type even after unification.
    /// It will be a subtype / supertype though
    Hole {
        nullability: Nullability,
        hole: FatTypeHole
    }
}

/// Everything a fat type can inherit:
///
/// - super nominal types
/// - super structural type (only one)
/// - super typescript types
/// - guards
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FatTypeInherited {
    pub super_ids: Vec<TypeIdent<FatType>>,
    pub structure: Option<TypeStructure<FatType>>,
    pub typescript_types: Vec<SubTree>,
    pub guards: Vec<NominalGuard>,
}

/// Possible rest argument in a fat function type
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum FatRestArgType {
    #[default]
    None,
    Array { element: FatType },
    Illegal { intended_type: FatType },
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
enum FatRestArgKind {
    Any,
    Tuple,
    Array,
    Illegal
}

#[derive(Debug, Clone)]
pub struct FatTypeHole {
    upper_bound: Rc<RefCell<FatType>>
}

impl FatTypeDecl {
    pub fn missing() -> Self {
        Self {
            name: TypeName::MISSING,
            type_params: Vec::new(),
            inherited: Box::new(FatTypeInherited::empty())
        }
    }
}

impl Default for FatTypeDecl {
    fn default() -> Self {
        Self::missing()
    }
}

impl FatType {
    pub const NEVER: Self = Self::Never { nullability: Nullability::NonNullable };

    pub const NULL: Self = Self::Never { nullability: Nullability::Nullable };

    impl_structural_type_constructors!();

    pub fn hole() -> Self {
        Self::Hole {
            nullability: Nullability::NonNullable,
            hole: FatTypeHole::new()
        }
    }

    /// Make nullable
    pub fn make_nullable(&mut self) {
        match self {
            Self::Any => {},
            Self::Never { nullability } => *nullability = Nullability::Nullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Hole { nullability, .. } => *nullability = Nullability::Nullable,
        }
    }

    /// Make nullable if `nullable` is true.
    ///
    /// If `nullable` is false but this is already nullable it will still be nullable.
    pub fn make_nullable_if(&mut self, nullable: bool) {
        if nullable {
            self.make_nullable();
        }
    }

    /// Returns a nullable clone
    pub fn nullable(&self) -> Self {
        let mut clone = self.clone();
        clone.make_nullable();
        clone
    }

    /// Returns a nullable clone if `nullable` is true, otherwise a normal clone
    pub fn nullable_if(&self, nullable: bool) -> Self {
        if nullable {
            self.nullable()
        } else {
            self.clone()
        }
    }

    /// Structure kind, if there is a structure, otherwise `None`
    pub fn structure_kind(&self) -> Option<StructureKind> {
        match self {
            Self::Any => None,
            Self::Never { nullability: _ } => None,
            Self::Structural { nullability: _, structure } => Some(structure.kind()),
            Self::Nominal { nullability: _, id: _, inherited } => inherited.structure.map(|s| s.kind()),
            Self::Hole { nullability: _, hole } => hole.upper_bound.borrow().structure_kind()
        }
    }

    /// Mutable references to ids and structure
    ///
    /// No nullability because you can only `make_nullable`, otherwise problems arise from type
    /// holes.
    pub fn mut_ids_and_structure(&mut self) -> (impl Iterator<Item=&mut TypeIdent<FatType>>, Option<&mut TypeStructure<FatType>>) {
        // Handle type holes first because...
        if let Self::Hole { nullability: _, hole } = self {
            return hole.upper_bound.get_mut().mut_ids_and_structure()
        }
        // ...use Some(iterable) and flatten so that we don't have to use auto_enum or something else
        let (ids, structure) = match self {
            Self::Any => (None, None),
            Self::Never { nullability: _ } => (None, None),
            Self::Structural { nullability: _, structure } => (None, Some(structure)),
            Self::Nominal { nullability: _, id, inherited } => {
                (Some(once(id).chain(inherited.super_ids.iter_mut())), inherited.structure.as_mut())
            },
            Self::Hole { nullability: _, hole: _ } => unreachable!("Should've been handled in prior if")
        };
        (ids.into_iter().flatten(), structure)
    }

    /// Destructs this and returns its nullability, ids and structure
    pub fn into_nullability_ids_and_structure(self) -> (Nullability, Vec<TypeIdent<FatType>>, Option<TypeStructure<FatType>>) {
        match self {
            Self::Any => (Nullability::Nullable, Vec::new(), None),
            Self::Never { nullability } => (nullability, Vec::new(), None),
            Self::Structural { nullability, structure } => (nullability, Vec::new(), Some(structure)),
            Self::Nominal { nullability, id, mut inherited } => {
                let mut ids = vec![id];
                ids.append(&mut inherited.super_ids);
                (nullability, ids, inherited.structure)
            },
            Self::Hole { nullability, hole } => {
                let (inner_nullability, ids, structure) = hole.upper_bound.into_inner().into_nullability_ids_and_structure();
                (nullability | inner_nullability, ids, structure)
            }
        }
    }

    /// Destructs this and returns its ids
    pub fn into_ids(self) -> Vec<TypeIdent<FatType>> {
        let (_, ids, _) = self.into_nullability_ids_and_structure();
        ids
    }

    /// Destructs this and returns its structure
    pub fn into_structure(self) -> Option<TypeStructure<FatType>> {
        let (_, _, structure) = self.into_nullability_ids_and_structure();
        structure
    }

    /// Create a [FatTypeInherited] which is the union of `supers`.
    ///
    /// This is [FatTypeInherited] because it will be the inherited of a type declaration or
    /// parameter, which is a nominal type with its own id. Also, the returned value may not have
    /// any structure or identifiers if `supers` is empty.
    pub fn collapse_supers(
        supers: impl IntoIterator<Item=Self>,
        mut e: TypeLogger<'_, '_, '_>
    ) -> FatTypeInherited {
        let mut inherited = FatTypeInherited::empty();
        for super_ in supers {
            match super_ {
                // Do nothing
                FatType::Any => {}
                FatType::Never { nullability } => {
                    error!(e, "Can't extend `{}`", match nullability {
                        Nullability::NonNullable => "Never",
                        Nullability::Nullable => "Null"
                    });
                }
                FatType::Structural { nullability, structure } => {
                    if nullability == Nullability::Nullable {
                        error!(e, "Can't extend nullable type";
                            note!("instead you must extend the non-nullable type and annotate null at all of your uses"));
                    }
                    FatType::unify_structure(
                        &mut inherited.structure,
                        Some(structure),
                        Variance::Bivariant,
                        e.with_context(TypeLoc::Supertypes)
                    );
                }
                FatType::Nominal { nullability, id: super_id, inherited: super_inherited } => {
                    if nullability == Nullability::Nullable {
                        error!(e, "Can't extend nullable type";
                            note!("instead you must extend the non-nullable type and annotate null at all of your uses"));
                    }
                    inherited.super_ids.push(super_id);
                    inherited.merge(
                        *super_inherited,
                        e.with_context(TypeLoc::Supertypes)
                    );
                }
                FatType::Hole { .. } => {
                    log::error!("Tried to extend hole! This should never happen!");
                    error!(e, "Can't extend hole (though you shouldn't be able to cause this...)");
                }
            }
        }
        return inherited
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_structure(
        this: &mut Option<TypeStructure<Self>>,
        other: Option<TypeStructure<Self>>,
        bias: Variance,
        mut e: TypeLogger<'_, '_, '_>
    ) {
        let Some(other) = other else {
            if !bias.is_contravariant() {
                error!(e, "assigned type input must be a structure but required type does not");
            }
            return
        };
        let Some(this) = this else {
            if !bias.is_covariant() {
                error!(e, "assigned type has no structure but required type does");
            }
            *this = Some(other);
            return
        };
        Self::unify_structure2(this, other, bias, e);
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_structure2(
        this: &mut TypeStructure<Self>,
        mut other: TypeStructure<Self>,
        bias: Variance,
        mut e: TypeLogger<'_, '_, '_>
    ) {
        // Do whole structure conversions to get equivalent kinds of possible
        match (this.kind(), other.kind()) {
            (StructureKind::Tuple, StructureKind::Array) => {
                if !bias.is_covariant() {
                    error!(e, "an array is not a subtype of a tuple");
                }
                replace_with(this, || TypeStructure::Array { element_type: Box::default() }, |this| {
                    let TypeStructure::Tuple { element_types } = this else { unreachable!() };
                    TypeStructure::Array {
                        element_type: Box::new(FatType::unify_all_optionals(element_types, Variance::Bivariant, TypeLogger::ignore()))
                    }
                });
            }
            (StructureKind::Array, StructureKind::Tuple) => {
                if !bias.is_contravariant() {
                    error!(e, "a tuple is not a supertype of an array");
                }
                let TypeStructure::Tuple { element_types } = other else { unreachable!() };
                other = TypeStructure::Array {
                    element_type: Box::new(FatType::unify_all_optionals(element_types, Variance::Bivariant, TypeLogger::ignore()))
                }
            }
            _ => {}
        }
        // Merge if equivalent kinds or log error
        match (this, other) {
            (TypeStructure::Fn { fn_type }, TypeStructure::Fn { fn_type: other_fn_type }) => {
                Self::unify_fn(fn_type, *other_fn_type, bias, e);
            }
            (TypeStructure::Array { element_type }, TypeStructure::Array { element_type: other_element_type }) => {
                Self::unify(
                    element_type,
                    *other_element_type,
                    bias,
                    e.with_context(TypeLoc::ArrayElem)
                );
            }
            (TypeStructure::Tuple { element_types }, TypeStructure::Tuple { element_types: other_element_types }) => {
                Self::unify_optionals(
                    element_types,
                    other_element_types,
                    bias,
                    |index | e.with_context(TypeLoc::TupleElem { index })
                );
            }
            (TypeStructure::Object { field_types }, TypeStructure::Object { field_types: other_field_types }) => {
                Self::unify_fields(
                    field_types,
                    other_field_types,
                    bias,
                    |name| e.with_context(TypeLoc::ObjectField { name })
                );
            }
            (this, other) => {
                error!(e, "different structure-kinds: {} and {}", this.kind().a_display(), other.kind().a_display());
            }
        }
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_fn(
        this: &mut FnType<Self>,
        other: FnType<Self>,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
    ) {
        Self::unify_type_params(
            &mut this.type_params,
            other.type_params,
            bias.reversed(),
            |index| e.with_context(TypeLoc::FunctionTypeParam { index })
        );
        Self::unify(
            &mut this.this_type,
            other.this_type,
            bias.reversed(),
            e.with_context(TypeLoc::FunctionThisParam)
        );
        Self::unify_regular_and_rest_parameters(
            (&mut this.arg_types, &mut this.rest_arg_type),
            (other.arg_types, other.rest_arg_type),
            bias.reversed(),
            |index| e.with_context(TypeLoc::FunctionParam { index })
        );
        Self::unify_return(
            &mut this.return_type,
            other.return_type,
            bias,
            e.with_context(TypeLoc::FunctionReturn)
        );
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_return(
        this: &mut ReturnType<Self>,
        other: ReturnType<Self>,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
    ) {
        match (this, other) {
            (ReturnType::Void, ReturnType::Void) => {}
            (ReturnType::Type(this), ReturnType::Type(other)) => {
                Self::unify(this, other, bias, e);
            }
            (ReturnType::Void, ReturnType::Type(_)) => {
                error!(e, "assigned returns void but required returns a type");
            },
            (ReturnType::Type(_), ReturnType::Void) => {
                error!(e, "assigned returns a type but required returns void");
            }
        }
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_optionals<'a, 'b: 'a, 'tree: 'a>(
        this: &mut Vec<OptionalType<Self>>,
        other: Vec<OptionalType<Self>>,
        bias: Variance,
        e: impl Fn(usize) -> TypeLogger<'a, 'b, 'tree>
    ) {
        todo!();
        /* let len = this.len().max(other.len());
        this.resize(len, Self::NEVER);
        other.resize(len, Self::NEVER);
        for (index, (this, other)) in this.iter_mut().zip(other).enumerate() {
            Self::unify(this, other, bias, e(index));
        } */
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// This unifies both the parameter types (optional types) and rest parameter types. However, the
    /// rest types will affect the parameter types' unification if there is an array rest type, and
    /// its corresponding parameter type list doesnt't have as much elements as the other parameter
    /// type list. Also be aware that the function's `this` parameter types and type parameters must
    /// be unified, but in separate functions, since they are not affected by regular or rest
    /// parameter types (or each other).
    pub fn unify_regular_and_rest_parameters<'a, 'b: 'a, 'tree: 'a>(
        this: (&mut Vec<OptionalType<Self>>, &mut FatRestArgType),
        other: (Vec<OptionalType<Self>>, FatRestArgType),
        bias: Variance,
        e: impl Fn(usize) -> TypeLogger<'a, 'b, 'tree>
    ) {
        let (this_regular, this_rest) = this;
        let (other_regular, other_rest) = other;
        todo!();
        /* let len = this.len().max(other.len());
        this.resize(len, Self::NEVER);
        other.resize(len, Self::NEVER);
        for (index, (this, other)) in this.iter_mut().zip(other).enumerate() {
            Self::unify(this, other, bias, e(index));
        } */
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_type_parameters<'a, 'b: 'a, 'tree: 'a>(
        this: &mut Vec<TypeParam<Self>>,
        other: Vec<TypeParam<Self>>,
        bias: Variance,
        e: impl Fn(usize) -> TypeLogger<'a, 'b, 'tree>
    ) {
        todo!();
        /* let len = this.len().max(other.len());
        this.resize(len, Self::NEVER);
        other.resize(len, Self::NEVER);
        for (index, (this, other)) in this.iter_mut().zip(other).enumerate() {
            Self::unify(this, other, bias, e(index));
        } */
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// In the subtype-checking field order isn't important and field keys are unified like sets.
    /// However, this still tries to preserve field order in `this` as much as possible for better
    /// user-facing support.
    pub fn unify_fields<'a, 'b: 'a, 'tree: 'a>(
        this: &mut Vec<Field<OptionalType<Self>>>,
        other: Vec<Field<OptionalType<Self>>>,
        bias: Variance,
        e: impl Fn(&ValueName) -> TypeLogger<'a, 'b, 'tree>
    ) {
        todo!();
        /* for (name, this) in this.iter_mut() {
            let other = other.remove(name).unwrap_or(Self::NEVER);
            Self::unify(this, other, bias, e(name));
        }
        for (name, other) in other {
            let this = Self::NEVER;
            Self::unify(this, other, bias, e(name));
        } */
    }

    /// **Unifies** `self` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// `self` will be mutated into the union type `self U other`: a value is an instance of this
    /// type if it's an instance of the original `self` *or* `other`.
    ///
    /// Besides creating the union type, this also logs "type mismatch" errors depending on `bias:
    ///
    /// - If `bias` is covariant, then `self` must be a subtype of `other` (i.e. all instances of
    ///   `self` must also be instances of `other`).
    /// - If `bias` is contravariant, then `self` must be a supertype of `other` (i.e. all instances
    ///   of `other` must also be instances of `self`).
    /// - If `bias` is invariant, `self` must be equivalent to `other` (i.e. all instances of `self`
    ///   must also be instances of `other` and vice versa).
    /// - If `bias` is bivariant, `self` must be a subtype *or* supertype of `other` (i.e. there
    ///   must exist at least one possible value which is an instance of both `self` and `other`,
    ///   *if* there exists at least one possible value which is an instance of `self` *and* there
    ///   exists at least one possible value which is an instance of `other`).
    ///
    /// If you are only creating a union and don't care about type mismatches, even if `self` and
    /// `other` are disjoint, you can provide `TypeLogger::ignore` for `e`, which skips logging
    /// and makes `bias` irrelevant.
    fn unify(
        &mut self,
        other: Self,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
    ) {
        todo!()
    }


    /// [Unifies](FatType::unify_all) types and logs subtype/disjoint errors based on `bias`.
    pub fn unify_all_optionals(types: impl IntoIterator<Item=OptionalType<Self>>, bias: Variance, e: TypeLogger<'_, '_, '_>) -> Self {
        todo!();
    }

    /// [Unifies](FatType::unify) all types and logs subtype/disjoint errors based on `bias`.
    ///
    /// `bias` is transitive, so e.g. covariant bias would mean every types must be a subtype of
    /// types which come afterward.
    pub fn unify_all(types: impl IntoIterator<Item=Self>, bias: Variance, e: TypeLogger<'_, '_, '_>) -> Self {
        todo!();
        /* let mut types = types.into_iter();
        let Some(mut result) = types.next() else {
            return Self::NEVER
        };
        for other in types {
            result = result.unify(other, Variance::Bivariant, todo!());
        }
        result */
    }
}

impl TypeTrait for FatType {
    type RestArgType = FatRestArgType;

    fn map_rest_arg_type(rest_arg_type: Self::RestArgType, f: impl FnMut(Self) -> Self) -> Self::RestArgType {
        rest_arg_type.map(f)
    }

    fn map_ref_rest_arg_type(rest_arg_type: &Self::RestArgType, f: impl FnMut(&Self) -> Self) -> Self::RestArgType {
        rest_arg_type.map_ref(f)
    }
}

impl TypeTraitMapsFrom<ThinType> for FatType {
    fn map_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        mut arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: ThinType,
        return_type: ReturnType<Self>,
        f: impl FnMut(ThinType) -> Self
    ) -> FnType<Self> {
        let intended_rest_arg_type = f(rest_arg_type);
        let rest_arg_type = FatRestArgType::from(intended_rest_arg_type, &mut arg_types);
        FnType {
            type_params,
            this_type,
            arg_types,
            rest_arg_type,
            return_type
        }
    }

    fn map_ref_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        mut arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: &ThinType,
        return_type: ReturnType<Self>,
        f: impl FnMut(&ThinType) -> Self
    ) -> FnType<Self> {
        let intended_rest_arg_type = f(rest_arg_type);
        let rest_arg_type = FatRestArgType::from(intended_rest_arg_type, &mut arg_types);
        FnType {
            type_params,
            this_type,
            arg_types,
            rest_arg_type,
            return_type
        }
    }
}

impl FatRestArgType {
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
    pub fn from(intended_type: FatType, arg_types: &mut Vec<OptionalType<FatType>>) -> Self {
        let kind = FatRestArgKind::of(&intended_type);
        match kind {
            FatRestArgKind::Any => Self::Array { element: FatType::Any },
            FatRestArgKind::Tuple => {
                let mut elements = intended_type.into_structure().and_then(|x| x.into_tuple_element_types())
                    .expect("FatRestArgType::from: intended_type is not a tuple but FatRestArgKind::of returned FatRestArgKind::Tuple")
                arg_types.append(&mut elements);
                Self::None
            }
            FatRestArgKind::Array => {
                let element = intended_type.into_structure().and_then(|x| x.into_array_element_type())
                    .expect("FatRestArgType::from: intended_type is not an array but FatRestArgKind::of returned FatRestArgKind::Array");
                Self::Array { element }
            }
            FatRestArgKind::Illegal => Self::Illegal { intended_type }
        }
    }

    //Can't really abstract (I mean we could proc-macro this whole map/map_ref thing...)
    //noinspection DuplicatedCode
    fn map(self, f: impl FnMut(FatType) -> FatType) -> Self {
        match self {
            Self::None => Self::None,
            Self::Array { element } => Self::Array { element: f(element) },
            Self::Illegal { intended_type } => Self::Illegal { intended_type: f(intended_type) }
        }
    }

    //noinspection DuplicatedCode
    fn map_ref(&self, f: impl FnMut(&FatType) -> FatType) -> Self {
        match self {
            Self::None => Self::None,
            Self::Array { element } => Self::Array { element: f(element) },
            Self::Illegal { intended_type } => Self::Illegal { intended_type: f(intended_type) }
        }
    }
}

impl FatRestArgKind {
    pub fn of(intended_type: &FatType) -> Self {
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

impl FatTypeInherited {
    /// Also = `default()`
    pub const fn empty() -> Self {
        Self {
            super_ids: Vec::new(),
            structure: None,
            typescript_types: Vec::new(),
            guards: Vec::new()
        }
    }

    pub fn merge(&mut self, other: Self<>, e: TypeLogger<'_, '_, '_>) {
        FatType::unify_structure(&mut self.structure, other.structure, Variance::Bivariant, e);
        self.super_ids.extend(other.super_ids);
        self.typescript_types.extend(other.typescript_types);
        self.guards.extend(other.guards);
    }
}


impl FatTypeHole {
    pub fn new() -> Self {
        Self {
            upper_bound: Rc::new(RefCell::new(FatType::NEVER)),
        }
    }
}

impl PartialEq<FatTypeHole> for FatTypeHole {
    fn eq(&self, other: &FatTypeHole) -> bool {
        Rc::ptr_eq(&self.upper_bound, &other.upper_bound)
    }
}

impl Eq for FatTypeHole {}

impl Default for FatTypeHole {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeParam<FatType> {
    pub fn into_type(self, e: TypeLogger<'_, '_, '_>) -> FatType {
        let inherited = FatType::collapse_supers(self.supers, e);
        FatType::Nominal {
            nullability: Nullability::NonNullable,
            id: TypeIdent {
                name: self.name,
                generic_args: Vec::new()
            },
            inherited: Box::new(inherited)
        }
    }
}

impl TypeParam<FatType> {
    pub fn into_decl(self, e: TypeLogger<'_, '_, '_>) -> FatTypeDecl {
        let inherited = FatType::collapse_supers(self.supers, e);
        FatTypeDecl {
            name: self.name,
            // No higher-kinded types
            type_params: Vec::new(),
            inherited: Box::new(inherited)
        }
    }
}