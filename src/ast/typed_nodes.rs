use std::any::Any;
use enquote::unquote;
use once_cell::unsync::OnceCell;
use smol_str::SmolStr;
use crate::analyses::bindings::{Binding, LocalBinding, TypeName, ValueName};
use crate::analyses::types::{FatType, FatTypeDecl, Field, InferredType, NominalGuard, OptionalType, ReturnType, ThinType, TypeParam, Variance};
use crate::ast::NOMINALSCRIPT_PARSER;
use crate::ast::tree_sitter::TSNode;
use crate::diagnostics::FileLogger;
use crate::misc::lazy::{Lazy, LazyError};

pub trait AstNode<'tree> {
    fn node(&self) -> TSNode<'tree>;
}

macro_rules! impl_ast_node_common_trait {
    ($AstNode:ident) => {
        impl<'tree> AstNode<'tree> for $AstNode<'tree> {
            fn node(&self) -> TSNode<'tree> {
                self.node
            }
        }
    }
}

macro_rules! impl_ast_node_common_kind_guard {
    ($node:ident, [ $($NODE_KIND:literal),* $(,)? ]) => {
        if ![$($NODE_KIND),*].contains(&$node.kind()) {
            panic!("Expected {:?} to be one of {:?}, but was {:?}", node, [$($NODE_KIND),*], $node.kind());
        }
    }
}

macro_rules! impl_ast_node_common {
    ($AstNode:ident, [ $($NODE_KIND:literal),* $(,)? ]) => {
        impl_ast_node_common_trait!($AstNode);

        impl<'tree> $AstNode<'tree> {
            fn new(node: TSNode<'tree>) -> Self {
                impl_ast_node_common_kind_guard!(node, [$($NODE_KIND),*]);
                Self::_new(node)
            }
        }
    };
}

pub trait TypedAstNode<'tree>: AstNode<'tree> {
    type InferTypeFn: FnOnce() -> Result<FatType, LazyError>;

    fn _infer_type(&self) -> Lazy<FatType, Self::InferTypeFn>;
    fn infer_type_for(&self, use_: TSNode<'tree>, e: &mut FileLogger<'_>) -> FatType {
        e
            .unwrap_import_result(self._infer_type().get(), self.node(), Some(use_))
            .unwrap_or(FatType::Any)
    }
}

pub trait AstBinding<'tree>: TypedAstNode<'tree> + LocalBinding<'tree> {
    fn name_ident(&self) -> &AstValueIdent<'tree>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstValueIdent<'tree> {
    pub node: TSNode<'tree>,
    pub name: ValueName,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstTypeIdent<'tree> {
    pub node: TSNode<'tree>,
    pub name: TypeName,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstTypeParameter<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstTypeIdent<'tree>,
    pub nominal_supertypes: Vec<AstType<'tree>>,
    pub thin: TypeParam<ThinType>,
    pub resolved: OnceCell<TypeParam<FatType>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstType<'tree> {
    pub node: TSNode<'tree>,
    pub shape: ThinType,
    pub resolved: OnceCell<FatType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstParameter<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstValueIdent<'tree>,
    pub is_rest_param: bool,
    pub type_: Option<AstType<'tree>>,
    pub inferred_type: OnceCell<InferredType<'tree>>,
    pub value: Option<TSNode<'tree>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstValueDecl<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstValueIdent<'tree>,
    pub type_: Option<AstType<'tree>>,
    pub inferred_type: OnceCell<InferredType<'tree>>,
    pub value: Option<TSNode<'tree>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstFunctionDecl<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstValueIdent<'tree>,
    pub nominal_params: Vec<AstTypeIdent<'tree>>,
    pub formal_params: Vec<AstParameter<'tree>>,
    pub return_type: Option<AstType<'tree>>,
    pub inferred_return_type: OnceCell<InferredType<'tree>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNominalGuard<'tree> {
    pub node: TSNode<'tree>,
    pub parameter: AstValueIdent<'tree>,
    pub body: TSNode<'tree>,
    pub shape: NominalGuard,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstTypeDecl<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstTypeIdent<'tree>,
    pub nominal_params: Vec<AstTypeParameter<'tree>>,
    pub typescript_supertype: Option<TSNode<'tree>>,
    pub nominal_supertypes: Vec<AstType<'tree>>,
    pub guard: Option<AstNominalGuard<'tree>>,
    pub resolved: OnceCell<FatTypeDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstValueImportSpecifier<'tree> {
    pub node: TSNode<'tree>,
    pub original_name: AstValueIdent<'tree>,
    pub alias: AstValueIdent<'tree>,
    pub resolved_type: OnceCell<FatType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstTypeImportSpecifier<'tree> {
    pub node: TSNode<'tree>,
    pub original_name: AstTypeIdent<'tree>,
    pub alias: AstTypeIdent<'tree>,
    pub resolved_decl: OnceCell<FatTypeDecl>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstImportStatement<'tree> {
    pub node: TSNode<'tree>,
    pub path_node: TSNode<'tree>,
    pub path: String,
    pub imported_values: Vec<AstValueImportSpecifier<'tree>>,
    pub imported_types: Vec<AstTypeImportSpecifier<'tree>>,
}

impl_ast_node_common!(AstValueIdent, ["identifier"]);
impl<'tree> AstValueIdent<'tree> {
    pub fn _new(node: TSNode<'tree>) -> Self {
        Self {
            node,
            name: ValueName::new(node.text()),
        }
    }
}

impl_ast_node_common!(AstTypeIdent, ["nominal_type_identifier"]);
impl<'tree> AstTypeIdent<'tree> {
    pub fn _new(node: TSNode<'tree>) -> Self {
        Self {
            node,
            name: TypeName::new(node.text()),
        }
    }
}

impl_ast_node_common!(AstTypeParameter, ["nominal_type_parameter"]);
impl<'tree> AstTypeParameter<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        let name = AstTypeIdent::new(node.named_child(0).unwrap());
        let nominal_supertypes = node.named_child(1).map(|node| {
            node.named_children(&mut node.walk())
                .map(AstType::of_annotation)
                .collect::<Vec<_>>()
        }).unwrap_or_default();
        let thin = TypeParam {
            variance_bound: Variance::Bivariant,
            name: name.name.clone(),
            supers: nominal_supertypes.iter().map(|t| t.shape.clone()).collect(),
        };
        Self {
            node,
            name,
            nominal_supertypes,
            thin,
            resolved: OnceCell::new(),
        }
    }
}

impl_ast_node_common!(AstType, [
    "nominal_type_identifier",
    "parenthesized_nominal_type",
    "generic_nominal_type",
    "object_nominal_type",
    "array_nominal_type",
    "tuple_nominal_type",
    "function_nominal_type",
    "nullable_nominal_type",
]);
impl<'tree> AstType<'tree> {
    pub fn of_annotation(node: TSNode<'tree>) -> Self {
        impl_ast_node_common_kind_guard!(node, ["nominal_type_annotation"]);
        node.mark();
        Self::new(node.named_child(0).unwrap())
    }

    fn _new(node: TSNode<'tree>) -> Self {
        Self {
            node,
            shape: Self::parse(node),
            resolved: OnceCell::new(),
        }
    }

    // TODO: Allow this_type to be set (requires modifying the grammar),
    // and optional fields/tuple elements
    fn parse(node: TSNode<'tree>) -> ThinType {
        match node.kind() {
            "nominal_type_identifier" => ThinType::ident(node.text()),
            "parenthesized_nominal_type" => Self::parse(node.named_child(0).unwrap()),
            "generic_nominal_type" => ThinType::generic(
                node.named_child(0).unwrap().text(),
                node.named_child(1).unwrap().named_children(&mut node.walk()).map(Self::parse),
            ),
            "function_nominal_type" => {
                let final_rest_param = node
                    .named_child(1).unwrap()
                    .field_child("rest_param");
                ThinType::func(
                    node.field_child("nominal_type_parameters").unwrap()
                        .named_children(&mut node.walk())
                        .map(|x| AstTypeParameter::new(x).thin),
                    ThinType::Any,
                    node.field_child("parameters").unwrap()
                        .named_children(&mut node.walk())
                        .filter(|x| x != final_rest_param)
                        .map(Self::parse)
                        .map(OptionalType::required),
                    final_rest_param.map_or(ThinType::empty_rest_arg(), Self::parse),
                    Self::parse_return(node.field_child("return_type").unwrap()),
                )
            }
            "array_nominal_type" => ThinType::array(
                Self::parse(node.named_child(0).unwrap()),
            ),
            "tuple_nominal_type" => ThinType::tuple(
                node.named_children(&mut node.walk())
                    .map(Self::parse)
                    .map(OptionalType::required),
            ),
            "object_nominal_type" => ThinType::object(
                node.named_children(&mut node.walk()).map(Self::parse_field)
            ),
            "nullable_nominal_type" => ThinType::nullable(
                Self::parse(node.named_child(0).unwrap()),
            ),
            _ => panic!("unhandled node kind: {}", node.kind()),
        }
    }

    fn parse_field(node: TSNode<'tree>) -> Field<OptionalType<ThinType>> {
        match node.kind() {
            "nominal_property_signature" => {
                let name = node.named_child(0).unwrap().text();
                let type_ = Self::parse(node.named_child(1).unwrap().named_child(0).unwrap());
                Field {
                    name: SmolStr::new(name),
                    type_: OptionalType::required(type_),
                }
            }
            "nominal_method_signature" => {
                let name = node.named_child(0).unwrap().text().to_string();
                let final_rest_param = node
                    .named_child(2).unwrap()
                    .field_child("rest_param");
                let method_type = ThinType::func(
                    node.named_child(1).unwrap()
                        .named_children(&mut node.walk())
                        .map(|x| AstTypeParameter::new(x).thin),
                    ThinType::Any,
                    node.named_child(2).unwrap()
                        .named_children(&mut node.walk())
                        .filter(|x| x != final_rest_param)
                        .map(Self::parse)
                        .map(OptionalType::required),
                    final_rest_param.map_or(ThinType::empty_rest_arg(), Self::parse),
                    Self::parse_return(node.named_child(3).unwrap()),
                );
                Field {
                    name: SmolStr::new(name),
                    type_: OptionalType::required(method_type),
                }
            }
            _ => panic!("unhandled node kind: {}", node.kind()),
        }
    }

    fn parse_return(node: TSNode<'tree>) -> ReturnType<Box<ThinType>> {
        if node.text() == "Void" {
            ReturnType::Void
        } else {
            ReturnType::boxed(Self::parse(node))
        }
    }
}

impl_ast_node_common_trait!(AstParameter);
impl<'tree> AstParameter<'tree> {
    pub fn formal(node: TSNode<'tree>) -> Self {
        impl_ast_node_common_kind_guard!(node, ["required_parameter", "optional_parameter"]);
        let name_node = node.named_child(0).unwrap();
        Self {
            node,
            name: AstValueIdent::new(name_node),
            is_rest_param: name_node.kind() != "rest_pattern",
            type_: node.field_child("nominal_type").map(AstType::of_annotation),
            inferred_type: OnceCell::new(),
            value: node.field_child("value"),
        }
    }

    pub fn arrow(node: TSNode<'tree>) -> Self {
        impl_ast_node_common_kind_guard!(node, ["identifier"]);
        Self {
            node,
            name: AstValueIdent::new(node),
            is_rest_param: false,
            type_: None,
            inferred_type: OnceCell::new(),
            value: None,
        }
    }
}

impl_ast_node_common!(AstValueDecl, ["variable_declarator"]);
impl<'tree> AstValueDecl<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        Self {
            node,
            name: AstValueIdent::new(node.named_child(0).unwrap()),
            type_: node.field_child("nominal_type").map(AstType::of_annotation),
            inferred_type: OnceCell::new(),
            value: node.field_child("value"),
        }
    }
}

impl_ast_node_common!(AstFunctionDecl, ["function_declaration", "function_signature"]);
impl<'tree> AstFunctionDecl<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        Self {
            node,
            name: AstValueIdent::new(node.named_child(0).unwrap()),
            nominal_params: Self::nominal_params(node),
            formal_params: node.field_child("parameters").unwrap()
                .named_children(&mut node.walk())
                .map(AstParameter::formal)
                .collect(),
            return_type: node.field_child("nominal_return_type").map(AstType::of_annotation),
            inferred_return_type: OnceCell::new(),
        }
    }

    pub fn nominal_params(node: TSNode<'tree>) -> Vec<AstTypeIdent<'tree>> {
        node.field_child("type_parameters").map(|node| {
            node.named_children(&mut node.walk())
                .filter(|node| node.kind() == "nominal_type_parameter")
                .map(AstTypeParameter::new)
                .collect()
        }).unwrap_or_default()
    }
}

impl_ast_node_common!(AstNominalGuard, ["nominal_type_guard"]);
impl<'tree> AstNominalGuard<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        let parameter = AstValueIdent::new(node.named_child(0).unwrap());
        let body = node.named_child(1).unwrap();
        let shape = NominalGuard {
            param: parameter.name.clone(),
            body: body.into_tree(&mut *NOMINALSCRIPT_PARSER.borrow_mut())
                .expect("nominal guard body is invalid syntax"),
        };
        Self {
            node,
            parameter,
            body,
            shape
        }
    }
}

impl_ast_node_common!(AstTypeDecl, ["nominal_type_declaration"]);
impl<'tree> AstTypeDecl<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        node.mark();
        Self {
            node,
            name: AstTypeIdent::new(node.named_child(0).unwrap()),
            nominal_params: node.field_child("nominal_type_parameters").map(|x| {
                x.named_children(&mut node.walk())
                    .map(AstTypeParameter::new)
                    .collect::<Vec<_>>()
            }).unwrap_or_default(),
            typescript_supertype: node.field_child("type"),
            nominal_supertypes: node.field_child("nominal_supertypes").map(|x| {
                x.named_children(&mut node.walk())
                    .map(AstType::of_annotation)
                    .collect::<Vec<_>>()
            }).unwrap_or_default(),
            guard: node.field_child("guard").map(AstNominalGuard::new),
            resolved: OnceCell::new(),
        }
    }
}

impl_ast_node_common!(AstValueImportSpecifier, ["import_specifier"]);
impl<'tree> AstValueImportSpecifier<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        let original_name = AstValueIdent::new(node.named_child(0).unwrap());
        let alias = node.named_child(1).map_or_else(
            || original_name.clone(),
            AstValueIdent::new
        );
        Self {
            node,
            original_name,
            alias,
            resolved_type: OnceCell::new(),
        }
    }
}

impl_ast_node_common!(AstTypeImportSpecifier, ["import_specifier"]);
impl<'tree> AstTypeImportSpecifier<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        node.mark();
        let original_name = AstTypeIdent::new(node.named_child(0).unwrap());
        let alias = node.named_child(1).map_or_else(
            || original_name.clone(),
            AstTypeIdent::new
        );
        Self {
            node,
            original_name,
            alias,
            resolved_decl: OnceCell::new(),
        }
    }
}

impl_ast_node_common!(AstImportStatement, ["import_statement"]);
impl<'tree> AstImportStatement<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        let path_node = node.named_child(1).unwrap();
        let path = unquote(path_node.text()).expect("import path is not a well-formed string literal");
        let import_container = node.named_child(0).unwrap();
        let imported_values = import_container
            .named_children(&mut node.walk())
            .filter(|node| node.named_child(0).unwrap().kind() == "identifier")
            .map(AstValueImportSpecifier::new)
            .collect();
        let imported_types = import_container
            .named_children(&mut node.walk())
            .filter(|node| node.named_child(0).unwrap().kind() == "nominal_type_identifier")
            .map(AstTypeImportSpecifier::new)
            .collect();
        if let Some(invalid_specifier) = import_container
            .named_children(&mut node.walk())
            .find(|node| node.named_child(0).unwrap().kind() != "identifier" && node.named_child(0).unwrap().kind() != "nominal_type_identifier") {
            panic!("import statement contains an invalid import specifier {:?} of kind {:?}", invalid_specifier, invalid_specifier.kind());
        }
        Self {
            node,
            path_node,
            path,
            imported_values,
            imported_types
        }
    }
}