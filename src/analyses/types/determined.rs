use crate::analyses::types::{ResolveCtx, ReturnType, RlReturnType, RlType, Variance};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::{AstReturnType, AstType};
use crate::diagnostics::{FileLogger, TypeCheckInfo, TypeInfo};
use crate::{error, hint, hint_if};

/// Required or assigned type with information on what node it was determined from,
/// and whether it was explicitly provided and/or inferred. For diagnostics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeterminedType<'tree> {
    pub type_: RlType,
    /// Last defined value before the use (the use is assigned the determined type, type_ is the
    /// value's type unless an explicit annotation is provided).
    pub defined_value: Option<TSNode<'tree>>,
    /// Explicit annotation for the expression, if provided (expression is assigned the explicit type)
    pub explicit_type: Option<TSNode<'tree>>,
}

/// Return type with information on where we determined it for diagnostics (see [DeterminedType]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeterminedReturnType<'tree> {
    pub type_: RlReturnType,
    pub return_node: Option<TSNode<'tree>>,
    pub explicit_type: Option<TSNode<'tree>>,
}

impl<'tree> DeterminedType<'tree> {
    /// Type was inferred from the most recent initializer or assignment (def) before the location
    /// where its type is checked (use).
    ///
    /// Currently the "most recent initializer or assignment" is simply the initializer,
    /// and types don't get inferred from assignments. We don't support flow typing and don't even
    /// do control-flow analysis
    pub fn from_last_def(type_: RlType, inferred_from: TSNode<'tree>) -> Self {
        Self { type_, defined_value: Some(inferred_from), explicit_type: None }
    }

    /// Type was explicitly provided via type annotation
    pub fn explicit(annotation: &AstType<'tree>) -> Self {
        Self { type_: annotation.shape.clone(), defined_value: None, explicit_type: Some(annotation.node) }
    }

    /// Checks that `assigned` is a subtype of `required`. If not, emits an error at `loc_node`
    /// (the location where we want a type of `required` e.g. function argument)
    pub(crate) fn check_subtype2(self, required: Option<Self>, loc_node: TSNode<'tree>, e: &mut FileLogger<'_>, ctx: &ResolveCtx<'_>) {
        Self::check_subtype(Some(self), required, loc_node, e, ctx)
    }

    /// Checks that `assigned` is a subtype of `required`. If not, emits an error at `loc_node`
    /// (the location where we want a type of `required` e.g. function argument)
    pub(crate) fn check_subtype(
        assigned: Option<Self>,
        required: Option<Self>,
        loc_node: TSNode<'tree>,
        e: &mut FileLogger<'_>,
        ctx: &ResolveCtx<'_>
    ) {
        let Some(
            DeterminedType {
                type_: required_type,
                defined_value: required_defined_value,
                explicit_type: required_explicit_type
            }
        ) = required else {
            return
        };
        let Some(
            DeterminedType {
                type_: assigned_type,
                defined_value: assigned_defined_value,
                explicit_type: assigned_explicit_type
            }
        ) = assigned else {
            error!(e, "this value must be typed" => loc_node;
                hint_if!(required_defined_value => "type inferred here" => required_defined_value);
                hint_if!(required_explicit_type => "type defined here" => required_explicit_type));
            return
        };
        let (assigned_thin_type, mut assigned_type) = assigned_type.into_resolve(ctx);
        let (required_thin_type, required_type) = required_type.into_resolve(ctx);
        let e = e.type_(
            TypeCheckInfo {
                loc_node,
                assigned_info: TypeInfo {
                    thin_type: assigned_thin_type,
                    defined_value: assigned_defined_value,
                    explicit_type: assigned_explicit_type,
                },
                required_info: TypeInfo {
                    thin_type: required_thin_type,
                    defined_value: required_defined_value,
                    explicit_type: required_explicit_type,
                },
            }
        );

        assigned_type.unify(required_type, Variance::Covariant, e);
    }

    /// Checks that `assigned` and `required` are not disjoint, i.e. `assigned` is a subtype *or*
    /// supertype of `required`.
    ///
    /// This is important for wrap expressions, which enable up-casting: if the wrapped type is a
    /// supertype of the wrapper type, a runtime guard is generated to check each individual value's
    /// type and ensure it is an instance. However, if the wrapped type and wrapper type are disjoint,
    /// a wrapped value can never be an instance, so the guard will always fail, and it makes sense
    /// to notify the user of this at compile-time.
    ///
    /// `loc_node` is where we emit the error, e.g. the location of the wrap expression.
    pub(crate) fn check_not_disjoint(
        assigned: Option<Self>,
        required: Option<Self>,
        loc_node: TSNode<'tree>,
        e: &mut FileLogger<'_>,
        ctx: &ResolveCtx<'_>
    ) {
        let Some(
            DeterminedType {
                type_: required_type,
                defined_value: required_defined_value,
                explicit_type: required_explicit_type
            }
        ) = required else {
            return
        };
        let Some(
            DeterminedType {
                type_: assigned_type,
                defined_value: assigned_defined_value,
                explicit_type: assigned_explicit_type
            }
        ) = assigned else {
            return
        };
        let (assigned_thin_type, mut assigned_type) = assigned_type.into_resolve(ctx);
        let (required_thin_type, required_type) = required_type.into_resolve(ctx);
        let e = e.type_(
            TypeCheckInfo {
                loc_node,
                assigned_info: TypeInfo {
                    thin_type: assigned_thin_type,
                    defined_value: assigned_defined_value,
                    explicit_type: assigned_explicit_type,
                },
                required_info: TypeInfo {
                    thin_type: required_thin_type,
                    defined_value: required_defined_value,
                    explicit_type: required_explicit_type,
                },
            }
        );

        assigned_type.unify(required_type, Variance::Bivariant, e);

    }
}

impl<'tree> DeterminedReturnType<'tree> {
    /// Checks that `assigned` is a subtype of `required`. If not, emits an error at `loc_node`
    /// (the location where we want a type of `required` e.g. function argument)
    pub(crate) fn check_subtype(self, required: Option<AstReturnType<'tree>>, loc_node: TSNode<'tree>, e: &mut FileLogger<'_>, ctx: &ResolveCtx<'_>) {
        let Some(
            AstReturnType {
                node: required_explicit_type_node,
                shape: required_type
            }
        ) = required else {
            return
        };
        let DeterminedReturnType {
            type_: assigned_type,
            return_node,
            explicit_type: return_type_node
        } = self;
        let (assigned_thin_type, assigned_type) = assigned_type.into_resolve(ctx);
        let (required_thin_type, required_type) = required_type.into_resolve(ctx);

        // Check that explicit and inferred returned types match,
        // and print a descriptive error based on return Void etc.
        match (assigned_type, required_type) {
            (ReturnType::Void, ReturnType::Void) => {},
            (ReturnType::Type(mut assigned_type), ReturnType::Type(required_type)) => {
                let (ReturnType::Type(assigned_thin_type), ReturnType::Type(required_thin_type)) = (assigned_thin_type, required_thin_type) else {
                    unreachable!("fat type is Void iff thin type is")
                };
                let e = e.type_(
                    TypeCheckInfo {
                        loc_node,
                        assigned_info: TypeInfo {
                            thin_type: assigned_thin_type,
                            defined_value: return_node,
                            explicit_type: return_type_node,
                        },
                        required_info: TypeInfo {
                            thin_type: required_thin_type,
                            defined_value: None,
                            explicit_type: Some(required_explicit_type_node),
                        },
                    }
                );

                assigned_type.unify(required_type, Variance::Covariant, e);
            }
            (ReturnType::Void, ReturnType::Type(_required_type)) => {
                error!(e, "function annotated with a return type but returns Void" => loc_node;
                    hint!("return type here" => required_explicit_type_node);
                    hint_if!(return_node => "returning here" => return_node);
                    hint_if!(return_type_node => "return type here" => return_type_node));
            }
            (ReturnType::Type(_assigned_type), ReturnType::Void) => {
                error!(e, "function annotated with Void but returns a type" => loc_node;
                    hint!("return type here" => required_explicit_type_node);
                    hint_if!(return_node => "returning here" => return_node);
                    hint_if!(return_type_node => "return type here" => return_type_node));
            }
        }
    }
}

impl<'tree> From<AstType<'tree>> for DeterminedType<'tree> {
    fn from(value: AstReturnType<'tree>) -> Self {
        Self { type_: value.shape.into(), defined_value: None, explicit_type: Some(value.node) }
    }
}

impl<'tree> From<DeterminedType<'tree>> for DeterminedReturnType<'tree> {
    fn from(value: DeterminedType<'tree>) -> Self {
        Self { type_: value.type_.into(), return_node: value.defined_value, explicit_type: value.explicit_type }
    }
}