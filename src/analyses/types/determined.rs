use std::borrow::Borrow;
use std::fmt::Display;
use std::ops::Range;
use crate::analyses::types::{FatType, HasNullability, Nullability, OptionalType, ResolveCtx, ReturnType, RlReturnType, RlType, Variance};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::{AstReturnType, AstType};
use crate::diagnostics::{FileLogger, TypeCheckInfo, TypeInfo};
use crate::{error, issue, hint, hint_if};
use crate::analyses::bindings::FieldName;

/// Required or assigned type with information on what node it was determined from,
/// and whether it was explicitly provided and/or inferred. For diagnostics.
// TODO: more information on this (e.g. if the checked type is an arg in a function or ...)
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
    /// Type with no location info, e.g. a builtin
    pub fn intrinsic(type_: RlType) -> Self {
        Self {
            type_,
            defined_value: None,
            explicit_type: None,
        }
    }

    /// Checks that `assigned` is a subtype of `required`. If not, emits an error at `loc_node`
    /// (the location where we want a type of `required` e.g. function argument)
    pub(crate) fn check_subtype2(self, required: Option<Self>, loc_node: TSNode<'tree>, e: &FileLogger<'_>, ctx: &ResolveCtx<'_>) {
        Self::check_subtype(Some(self), required, loc_node, e, ctx)
    }

    /// Checks that `assigned` is a subtype of `required`. If not, emits an error at `loc_node`
    /// (the location where we want a type of `required` e.g. function argument)
    pub(crate) fn check_subtype(
        assigned: Option<Self>,
        required: Option<Self>,
        loc_node: TSNode<'tree>,
        e: &FileLogger<'_>,
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
        e: &FileLogger<'_>,
        ctx: &ResolveCtx<'_>
    ) {
      Self::_check_not_disjoint(assigned, required, loc_node, e, ctx);
    }

    /// [check_not_disjoint] and then returns the unified type
    pub(crate) fn check_not_disjoint_then_unify(
        left: Option<Self>,
        right: Option<Self>,
        loc_node: TSNode<'tree>,
        e: &FileLogger<'_>,
        ctx: &ResolveCtx<'_>
    ) -> Option<Self> {
        Self::_check_not_disjoint(left, right, loc_node, e, ctx).map(|fat_type| {
            let explicit_type = {
                // Already unwrapped and resolved
                let left = left.unwrap();
                let right = right.unwrap();
                let left_fat_type = left.type_.resolve(ctx);
                let right_fat_type = right.type_.resolve(ctx);
                if &fat_type == left_fat_type {
                    left.explicit_type
                } else if &fat_type == right_fat_type {
                    right.explicit_type
                } else {
                    None
                }
            };
            DeterminedType {
                type_: RlType::resolved(fat_type),
                defined_value: Some(loc_node),
                explicit_type,
            }
        })
    }

    fn _check_not_disjoint(
        assigned: Option<Self>,
        required: Option<Self>,
        loc_node: TSNode<'tree>,
        e: &FileLogger<'_>,
        ctx: &ResolveCtx<'_>
    ) -> Option<FatType> {
        let DeterminedType {
            type_: required_type,
            defined_value: required_defined_value,
            explicit_type: required_explicit_type
        } = required?;
        let DeterminedType {
            type_: assigned_type,
            defined_value: assigned_defined_value,
            explicit_type: assigned_explicit_type
        } = assigned?;
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
        Some(assigned_type)
    }

    pub fn member_type<N: Display + ?Sized>(
        &self,
        field_name: &N,
        is_nullable_access: bool,
        loc_node: TSNode<'tree>,
        e: &FileLogger<'_>,
        ctx: &ResolveCtx<'_>
    ) -> DeterminedType<'tree> where FieldName: Borrow<N> {
        let fat_type = self.type_.resolve(ctx);
        let field_type = fat_type.member(
            field_name,
            is_nullable_access,
            &self.type_.thin,
            loc_node,
            self.defined_value,
            self.explicit_type,
            e
        );
        DeterminedType {
            type_: RlType::resolved(field_type),
            defined_value: self.defined_value,
            explicit_type: None,
        }
    }

    pub fn subscript_type(
        &self,
        index: Option<usize>,
        is_nullable_access: bool,
        loc_node: TSNode<'tree>,
        e: &FileLogger<'_>,
        ctx: &ResolveCtx<'_>
    ) -> DeterminedType<'tree> {
        let fat_type = self.type_.resolve(ctx);
        let elem_type = fat_type.subscript(
            index,
            is_nullable_access,
            &self.type_.thin,
            loc_node,
            self.defined_value,
            self.explicit_type,
            e
        );
        DeterminedType {
            type_: RlType::resolved(elem_type),
            defined_value: self.defined_value,
            explicit_type: None,
        }
    }

    pub fn awaited_type(
        &self,
        loc_node: TSNode<'tree>,
        e: &FileLogger<'_>,
        ctx: &ResolveCtx<'_>
    ) -> DeterminedType<'tree> {
        let fat_type = self.type_.resolve(ctx);
        let awaited_type = fat_type.awaited(
            &self.type_.thin,
            loc_node,
            self.defined_value,
            self.explicit_type,
            e
        );
        DeterminedType {
            type_: RlType::resolved(awaited_type),
            defined_value: self.defined_value,
            explicit_type: None,
        }
    }

    pub fn call_type<'a, 'b>(
        &self,
        this_arg: Option<(Option<&'a DeterminedType<'tree>>, TSNode<'tree>)>,
        args: impl IntoIterator<Item=(Option<&'b DeterminedType<'tree>>, TSNode<'tree>)>,
        // TODO: Variable spread args
        is_nullable_call: bool,
        fn_loc_node: TSNode<'tree>,
        e: &FileLogger<'_>,
        ctx: &ResolveCtx<'_>
    ) -> DeterminedType<'tree> where 'tree: 'a, 'tree: 'b {
        let defined_value = self.defined_value;
        let explicit_type = self.explicit_type;
        let fat_type = self.type_.resolve(ctx);
        let return_type = {
            if matches!(fat_type.nullability(), Nullability::Nullable) && !is_nullable_call {
                error!(e, "Can't call nullable function without optional chain: {}", self.type_.thin => fn_loc_node;
                    hint_if!(defined_value => "type inferred here" => defined_value);
                    hint_if!(explicit_type => "type defined here" => explicit_type));
            }

            let mut return_type = fat_type.with_function_structure(|fn_type| match fn_type {
                None => {
                    error!(e, "Can't call (not a function): {}", self.type_.thin => fn_loc_node;
                    hint_if!(defined_value => "type inferred here" => defined_value);
                    hint_if!(explicit_type => "type defined here" => explicit_type));
                    FatType::NEVER
                }
                Some(fn_type) => {
                    // TODO: Subst type params (which makes the return type non-trivial)
                    // Check this arg
                    let this_param = &fn_type.this_type;
                    let this_param_det = DeterminedType {
                        type_: RlType::resolved(this_param.clone()),
                        defined_value: defined_value.or(explicit_type),
                        explicit_type: None,
                    };
                    match this_arg {
                        None => {
                            if !matches!(this_param.nullability(), Nullability::Nullable) {
                                error!(e, "Can't call without this arg: {}", self.type_.thin => fn_loc_node;
                                issue!("this arg has type {}, which isn't nullable", this_param_det.type_.thin);
                                hint_if!(defined_value => "type inferred here" => defined_value);
                                hint_if!(explicit_type => "type defined here" => explicit_type));
                            }
                        },
                        Some((this_arg, this_arg_loc_node)) => {
                            Self::check_subtype(this_arg.cloned(), Some(this_param_det), this_arg_loc_node, e, ctx);
                        }
                    }

                    // Check args
                    let Range { start: min_num_args, end: max_num_args } = fn_type.arity_range();
                    let mut num_args = 0;
                    for (arg, arg_loc_node) in args {
                        let idx = num_args;
                        num_args += 1;
                        let Some(param) = fn_type.arg_types.get(idx).map(|t| t.as_ref())
                            .or_else(|| fn_type.rest_arg_type.get(idx - fn_type.arg_types.len()).map(OptionalType::optional)) else {
                            continue
                        };
                        let param_det = DeterminedType {
                            type_: RlType::resolved(param.cloned().collapse_optionality_into_nullability()),
                            defined_value: self.defined_value.or(self.explicit_type),
                            explicit_type: None,
                        };
                        Self::check_subtype(arg.cloned(), Some(param_det), arg_loc_node, e, ctx);
                    }
                    // Check num args (arity range)
                    if num_args < min_num_args {
                        error!(e, "Too few arguments: expected at least {}, got {}", min_num_args, num_args => fn_loc_node;
                        hint_if!(defined_value => "type inferred here" => defined_value);
                        hint_if!(explicit_type => "type defined here" => explicit_type));
                    } else if num_args > max_num_args {
                        error!(e, "Too many arguments: expected at most {}, got {}", max_num_args, num_args => fn_loc_node;
                        hint_if!(defined_value => "type inferred here" => defined_value);
                        hint_if!(explicit_type => "type defined here" => explicit_type));
                    }
                    // Return the return type
                    // TODO: Should be special-cased so we can prevent void functions in expression position)
                    //   This also means we change the return type of this, though not to `DeterminedReturnType` since there is no `return_node`,
                    //   instead give `DeterminedType` a default generic for the type.
                    match fn_type.return_type.clone() {
                        ReturnType::Void => FatType::Any,
                        ReturnType::Type(t) => t,
                    }
                }
            });
            return_type.make_nullable_if(is_nullable_call);
            return_type
        };
        DeterminedType {
            type_: RlType::resolved(return_type),
            defined_value: self.defined_value,
            explicit_type: None,
        }
    }
}

impl<'tree> DeterminedReturnType<'tree> {
    /// Functions with no `return` statement at the end implicitly return `Void` AKA this
    pub fn implicit_void() -> Self {
        DeterminedReturnType {
            type_: RlReturnType::implicit_void(),
            return_node: None,
            explicit_type: None,
        }
    }

    /// Checks that `assigned` is a subtype of `required`. If not, emits an error at `loc_node`
    /// (the location where we want a type of `required` e.g. function argument)
    pub(crate) fn check_subtype(self, required: Option<AstReturnType<'tree>>, loc_node: TSNode<'tree>, e: &FileLogger<'_>, ctx: &ResolveCtx<'_>) {
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
    fn from(value: AstType<'tree>) -> Self {
        Self { type_: value.shape, defined_value: None, explicit_type: Some(value.node) }
    }
}

impl<'tree> From<AstReturnType<'tree>> for DeterminedReturnType<'tree> {
    fn from(value: AstReturnType<'tree>) -> Self {
        Self { type_: value.shape, return_node: None, explicit_type: Some(value.node) }
    }
}

impl<'tree> From<DeterminedType<'tree>> for DeterminedReturnType<'tree> {
    fn from(value: DeterminedType<'tree>) -> Self {
        Self { type_: RlReturnType::from(value.type_), return_node: value.defined_value, explicit_type: value.explicit_type }
    }
}