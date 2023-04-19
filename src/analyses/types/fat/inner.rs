use std::borrow::Borrow;
use std::fmt::Display;
use crate::analyses::bindings::{FieldName, TypeNameStr};
use crate::analyses::types::{FatType, HasNullability, Nullability, ThinType, TypeStructure, Variance};
use crate::ast::tree_sitter::TSNode;
use crate::diagnostics::{FileLogger, TypeLogger};
use crate::{error, issue, hint_if};
use join_lazy_fmt::Join;

impl FatType {
    /// Returns the member access type.
    /// Will log an error if this is a nominal or structural type and not an object or doesn't
    /// contain the field. Also throws an error on non-nullable access of a nullable type.
    pub fn member<N: Display + ?Sized>(
        &self,
        field_name: &N,
        is_nullable_access: bool,
        thin_type: &ThinType,
        loc_node: TSNode<'_>,
        defined_value: Option<TSNode<'_>>,
        explicit_type: Option<TSNode<'_>>,
        e: &FileLogger<'_>
    ) -> FatType where FieldName: Borrow<N> {
        if matches!(self.nullability(), Nullability::Nullable) && !is_nullable_access {
            error!(e, "Can't do non-nullable member access on potentially nullable type {}", thin_type => loc_node;
                    hint_if!(defined_value => "type inferred here" => defined_value);
                    hint_if!(explicit_type => "type declared here" => explicit_type))
        }
        match self {
            FatType::Any => FatType::Any,
            FatType::Never { nullability } => FatType::Never { nullability: *nullability },
            FatType::Structural { .. } | FatType::Nominal { .. } => self.with_structure(|structure| match structure {
                Some(TypeStructure::Object { field_types }) => {
                    let member_type = field_types.iter()
                        .find(|f| f.name.borrow() == field_name)
                        .map(|f| f.type_.clone().collapse_optionality_into_nullability());
                    match member_type {
                        None => {
                            error!(e, "No such field {} in {}", field_name, thin_type => loc_node;
                                issue!("the fields are: {}", ", ".join(field_types.iter().map(|f| f.name)));
                                hint_if!(defined_value => "type inferred here" => defined_value);
                                hint_if!(explicit_type => "type declared here" => explicit_type));
                            FatType::NULL
                        }
                        Some(mut member_type) => {
                            member_type.make_nullable_if(is_nullable_access || matches!(self.nullability(), Nullability::Nullable));
                            member_type
                        }
                    }
                }
                structure => {
                    let structure_kind_desc = match structure {
                        None => "unstructured nominal type",
                        Some(structure) => structure.kind().display()
                    };
                    error!(e, "Expected object, got {}", structure_kind_desc => loc_node;
                        hint_if!(defined_value => "type inferred here" => defined_value);
                        hint_if!(explicit_type => "type inferred here" => explicit_type));
                    FatType::NULL
                }
            }),
            // TODO: Fix this and other cases which just assume hole.upper_bound.borrow is ok;
            //    they should proably use a mapped hole or something
            FatType::Hole { nullability, hole } => {
                let mut result = hole.upper_bound.borrow().member(
                    field_name,
                    is_nullable_access,
                    thin_type,
                    loc_node,
                    defined_value,
                    explicit_type,
                    e
                );
                result.make_nullable_if(matches!(nullability, Nullability::Nullable));
                result
            }
        }
    }

    /// Returns the subscript access type.
    /// Will log an error if this is a nominal or structural type and not a tuple or an array, or if
    /// the index is out of bounds. Also throws an error on non-nullable access of a nullable type.
    pub fn subscript(
        &self,
        index: Option<usize>,
        is_nullable_access: bool,
        thin_type: &ThinType,
        loc_node: TSNode<'_>,
        defined_value: Option<TSNode<'_>>,
        explicit_type: Option<TSNode<'_>>,
        e: &FileLogger<'_>
    ) -> FatType {
        if matches!(self.nullability(), Nullability::Nullable) && !is_nullable_access {
            error!(e, "Can't do non-nullable subscript access on potentially nullable type {}", thin_type => loc_node;
                hint_if!(defined_value => "type inferred here" => defined_value);
                hint_if!(explicit_type => "type declared here" => explicit_type))
        }
        match self {
            FatType::Any => FatType::Any,
            FatType::Never { nullability } => FatType::Never { nullability: *nullability },
            FatType::Structural { .. } | FatType::Nominal { .. } => self.with_structure(|structure| match structure {
                Some(TypeStructure::Tuple { element_types }) => {
                    let subscript_type = match index {
                        None => {
                            Some(FatType::unify_all(
                                element_types.iter().map(|e| e.clone().collapse_optionality_into_nullability()),
                                Variance::Bivariant,
                                TypeLogger::ignore()
                            ))
                        },
                        Some(index) => {
                            if index < element_types.len() {
                                Some(element_types[index].clone().collapse_optionality_into_nullability())
                            } else {
                                None
                            }
                        }
                    };
                    match subscript_type {
                        None => {
                            error!(e, "Index {} out of bounds in {}", index.unwrap(), thin_type => loc_node;
                                issue!("the length is {}", element_types.len());
                                hint_if!(defined_value => "type inferred here" => defined_value);
                                hint_if!(explicit_type => "type declared here" => explicit_type));
                            FatType::NULL
                        }
                        Some(mut subscript_type) => {
                            subscript_type.make_nullable_if(is_nullable_access || matches!(self.nullability(), Nullability::Nullable));
                            subscript_type
                        }
                    }
                }
                Some(TypeStructure::Array { element_type }) => {
                    let mut subscript_type = element_type.as_ref().clone();
                    subscript_type.make_nullable_if(is_nullable_access || matches!(self.nullability(), Nullability::Nullable));
                    subscript_type
                }
                structure => {
                    let structure_kind_desc = match structure {
                        None => "unstructured nominal type",
                        Some(structure) => structure.kind().display()
                    };
                    error!(e, "Expected tuple or array, got {}", structure_kind_desc => loc_node;
                        hint_if!(defined_value => "type inferred here" => defined_value);
                        hint_if!(explicit_type => "type inferred here" => explicit_type));
                    FatType::NULL
                }
            }),
            // TODO: Fix this and other cases which just assume hole.upper_bound.borrow is ok;
            //    they should proably use a mapped hole or something
            FatType::Hole { nullability, hole } => {
                let mut result = hole.upper_bound.borrow().subscript(
                    index,
                    is_nullable_access,
                    thin_type,
                    loc_node,
                    defined_value,
                    explicit_type,
                    e
                );
                result.make_nullable_if(matches!(nullability, Nullability::Nullable));
                result
            }
        }
    }

    /// If this is a promise, returns the type of the awaited value.
    /// Will throw an error if this is a nominal or structural type and not a promise.
    pub fn awaited(
        &self,
        thin_type: &ThinType,
        loc_node: TSNode<'_>,
        defined_value: Option<TSNode<'_>>,
        explicit_type: Option<TSNode<'_>>,
        e: &FileLogger<'_>
    ) -> FatType {
        match self {
            FatType::Any => FatType::Any,
            FatType::Never { nullability } => FatType::Never { nullability: *nullability },
            FatType::Structural { .. } | FatType::Nominal { .. } => self.with_idents(|ids| match ids.into_iter().flatten().find(|id| &id.name == TypeNameStr::of("Promise")) {
                Some(promise) => {
                    let mut awaited_type = promise.generic_args.get(0).cloned().unwrap_or(FatType::Any);
                    awaited_type.make_nullable_if(matches!(self.nullability(), Nullability::Nullable));
                    awaited_type
                }
                None => {
                    error!(e, "Expected promise, got {}", thin_type => loc_node;
                        hint_if!(defined_value => "type inferred here" => defined_value);
                        hint_if!(explicit_type => "type inferred here" => explicit_type));
                    FatType::NULL
                }
            }),
            // TODO: Fix this and other cases which just assume hole.upper_bound.borrow is ok;
            //    they should proably use a mapped hole or something
            FatType::Hole { nullability, hole } => {
                let mut result = hole.upper_bound.borrow().awaited(
                    thin_type,
                    loc_node,
                    defined_value,
                    explicit_type,
                    e
                );
                result.make_nullable_if(matches!(nullability, Nullability::Nullable));
                result
            }
        }
    }
}