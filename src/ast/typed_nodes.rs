use std::any::Any;
use enquote::unquote;
use once_cell::unsync::OnceCell;
use smol_str::SmolStr;
use crate::analyses::bindings::{ValueBinding, LocalValueBinding, TypeName, ValueName, LocalTypeBinding, LocalUses, TypeBinding, HoistedValueBinding};
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

pub trait AstBinding<'tree>: TypedAstNode<'tree> + LocalValueBinding<'tree> {
    fn name_ident(&self) -> &AstValueIdent<'tree>;
}

#[derive(Debug, Clone)]
pub struct AstValueIdent<'tree> {
    pub node: TSNode<'tree>,
    pub name: ValueName,
}

#[derive(Debug, Clone)]
pub struct AstTypeIdent<'tree> {
    pub node: TSNode<'tree>,
    pub name: TypeName,
}

#[derive(Debug)]
pub struct AstTypeParameter<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstTypeIdent<'tree>,
    pub nominal_supertypes: Vec<AstType<'tree>>,
    pub thin: TypeParam<ThinType>,
    pub resolved: OnceCell<TypeParam<FatType>>,
}

#[derive(Debug)]
pub struct AstType<'tree> {
    pub node: TSNode<'tree>,
    pub shape: ThinType,
    pub resolved: OnceCell<FatType>,
}

#[derive(Debug)]
pub struct AstParameter<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstValueIdent<'tree>,
    pub is_this_param: bool,
    pub is_rest_param: bool,
    pub type_: Option<AstType<'tree>>,
    pub inferred_type: OnceCell<InferredType<'tree>>,
    pub value: Option<TSNode<'tree>>,
}

#[derive(Debug, Clone, Copy)]
pub struct AstReturn<'tree> {
    pub node: TSNode<'tree>,
    pub returned_value: Option<TSNode<'tree>>,
}

#[derive(Debug, Clone, Copy)]
pub struct AstThrow<'tree> {
    pub node: TSNode<'tree>,
    pub thrown_value: Option<TSNode<'tree>>,
}

#[derive(Debug)]
pub struct AstValueDecl<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstValueIdent<'tree>,
    pub type_: Option<AstType<'tree>>,
    pub inferred_type: OnceCell<InferredType<'tree>>,
    pub value: Option<TSNode<'tree>>
}

#[derive(Debug)]
pub struct AstFunctionDecl<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstValueIdent<'tree>,
    pub nominal_params: Vec<AstTypeIdent<'tree>>,
    pub formal_params: Vec<AstParameter<'tree>>,
    pub return_type: Option<AstType<'tree>>,
    pub inferred_return_type: OnceCell<InferredType<'tree>>,
}

#[derive(Debug)]
pub struct AstNominalGuard<'tree> {
    pub node: TSNode<'tree>,
    pub parameter: AstValueIdent<'tree>,
    pub body: TSNode<'tree>,
    pub shape: NominalGuard,
}

#[derive(Debug)]
pub struct AstTypeDecl<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstTypeIdent<'tree>,
    pub nominal_params: Vec<AstTypeParameter<'tree>>,
    pub typescript_supertype: Option<TSNode<'tree>>,
    pub nominal_supertypes: Vec<AstType<'tree>>,
    pub guard: Option<AstNominalGuard<'tree>>,
    pub resolved: OnceCell<FatTypeDecl>,
}

#[derive(Debug)]
pub struct AstValueImportSpecifier<'tree> {
    pub node: TSNode<'tree>,
    pub original_name: AstValueIdent<'tree>,
    pub alias: AstValueIdent<'tree>,
    pub resolved_type: OnceCell<FatType>,
}

#[derive(Debug)]
pub struct AstTypeImportSpecifier<'tree> {
    pub node: TSNode<'tree>,
    pub original_name: AstTypeIdent<'tree>,
    pub alias: AstTypeIdent<'tree>,
    pub resolved_decl: OnceCell<FatTypeDecl>
}

#[derive(Debug)]
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
            variance_bound: node.field_child("variance")
                .map(Self::variance_from)
                .unwrap_or_default(),
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

    fn variance_from(node: TSNode<'tree>) -> Variance {
        match node.text() {
            "biv" => Variance::Bivariant,
            "cov" => Variance::Covariant,
            "con" => Variance::Contravariant,
            "inv" => Variance::Invariant,
            _ => panic!("Invalid variance: {}", node.text()),
        }
    }
}

impl<'tree> TypeBinding<'tree> for AstTypeParameter<'tree> {
    fn name(&self) -> &TypeName {
        &self.name.name
    }

    fn resolve_decl(&self) -> Lazy<FatTypeDecl> {
        match self.resolved.get() {
            None => todo!(),
            Some(decl) => Lazy::immediate(decl.clone().into_decl()),
        }
    }
}

impl<'tree> LocalTypeBinding<'tree> for AstTypeParameter<'tree> {}

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

    fn parse(node: TSNode<'tree>) -> ThinType {
        match node.kind() {
            "nominal_type_identifier" => ThinType::ident(node.text()),
            "parenthesized_nominal_type" => Self::parse(node.named_child(0).unwrap()),
            "generic_nominal_type" => ThinType::generic(
                node.named_child(0).unwrap().text(),
                node.named_child(1).unwrap().named_children(&mut node.walk()).map(Self::parse),
            ),
            "function_nominal_type" => {
                let parameters = node.field_child("parameters").unwrap();
                let this_param = parameters.field_child("this_param");
                let rest_param = parameters.field_child("rest_param");
                ThinType::func(
                    node.field_child("nominal_type_parameters").unwrap()
                        .named_children(&mut node.walk())
                        .map(|x| AstTypeParameter::new(x).thin),
                    this_param.map_or(ThinType::Any, Self::parse),
                    parameters
                        .named_children(&mut node.walk())
                        .filter(|x| x != this_param && x != rest_param)
                        .map(Self::parse_optional),
                    rest_param.map_or(ThinType::empty_rest_arg(), Self::parse),
                    Self::parse_return(node.field_child("return_type").unwrap()),
                )
            }
            "array_nominal_type" => ThinType::array(
                Self::parse(node.named_child(0).unwrap()),
            ),
            "tuple_nominal_type" => ThinType::tuple(
                node.named_children(&mut node.walk())
                    .map(Self::parse_optional),
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
                let is_optional = node.field_child("is_optional").is_some();
                let type_ = Self::parse(node.named_child(1).unwrap().named_child(0).unwrap());
                Field {
                    name: SmolStr::new(name),
                    type_: OptionalType::new(type_, is_optional),
                }
            }
            "nominal_method_signature" => {
                let name = node.named_child(0).unwrap().text().to_string();
                let is_optional = node.field_child("is_optional").is_some();
                let parameters = node.named_child(2).unwrap();
                let this_param = parameters.field_child("this_param");
                let rest_param = parameters.field_child("rest_param");
                let method_type = ThinType::func(
                    node.named_child(1).unwrap()
                        .named_children(&mut node.walk())
                        .map(|x| AstTypeParameter::new(x).thin),
                    this_param.map_or(ThinType::Any, Self::parse),
                    node.named_child(2).unwrap()
                        .named_children(&mut node.walk())
                        .filter(|x| x != this_param && x != rest_param)
                        .map(Self::parse_optional),
                    rest_param.map_or(ThinType::empty_rest_arg(), Self::parse),
                    Self::parse_return(node.named_child(3).unwrap()),
                );
                Field {
                    name: SmolStr::new(name),
                    type_: OptionalType::new(method_type, is_optional),
                }
            }
            _ => panic!("unhandled node kind: {}", node.kind()),
        }
    }

    fn parse_optional(node: TSNode<'tree>) -> OptionalType<ThinType> {
        if node.kind() == "optional_nominal_type" {
            OptionalType::optional(Self::parse(node.named_child(0).unwrap()))
        } else {
            OptionalType::required(Self::parse(node))
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
            is_this_param: name_node.kind() == "this",
            is_rest_param: name_node.kind() == "rest_pattern",
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
            is_this_param: false,
            is_rest_param: false,
            type_: None,
            inferred_type: OnceCell::new(),
            value: None,
        }
    }
}

impl<'tree> ValueBinding<'tree> for AstParameter<'tree> {
    fn name(&self) -> &ValueName {
        self.name.name()
    }

    fn resolve_type(&self) -> Lazy<FatType> {
        match (self.inferred_type.get(), &self.type_) {
            (Some(inferred_type), _) => Lazy::immediate(inferred_type.type_.clone()),
            (None, None) => Lazy::immediate(FatType::Any),
            (None, Some(type_)) => todo!()
        }
    }
}

impl<'tree> LocalValueBinding<'tree> for AstParameter<'tree> {
    fn local_uses(&self) -> &LocalUses<'tree> {
        todo!()
    }
}

impl<'tree> HoistedValueBinding<'tree> for AstParameter<'tree> {}

impl_ast_node_common!(AstReturn, ["return_statement"]);
impl<'tree> AstReturn<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        Self {
            node,
            returned_value: node.last_named_child(),
        }
    }
}

impl_ast_node_common!(AstThrow, ["throw_statement"]);
impl<'tree> AstThrow<'tree> {
    fn _new(node: TSNode<'tree>) -> Self {
        Self {
            node,
            thrown_value: node.last_named_child(),
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


impl<'tree> ValueBinding<'tree> for AstValueDecl<'tree> {
    fn name(&self) -> &ValueName {
        self.name.name()
    }

    fn resolve_type(&self) -> Lazy<FatType> {
        match (self.inferred_type.get(), &self.type_) {
            (Some(inferred_type), _) => Lazy::immediate(inferred_type.type_.clone()),
            (None, None) => Lazy::immediate(FatType::Any),
            (None, Some(type_)) => todo!()
        }
    }
}

impl<'tree> LocalValueBinding<'tree> for AstValueDecl<'tree> {
    fn local_uses(&self) -> &LocalUses<'tree> {
        todo!()
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