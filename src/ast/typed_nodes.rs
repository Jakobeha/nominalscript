use enquote::unquote;
use once_cell::unsync::Lazy;
use smol_str::SmolStr;

use crate::analyses::bindings::{DynValueBinding, HoistedValueBinding, Locality, LocalTypeBinding, LocalValueBinding, TypeBinding, TypeName, ValueBinding, ValueName};
use crate::analyses::scopes::{ExprTypeMap, ScopePtr, ScopeTypeImportIdx, ScopeValueImportIdx};
use crate::analyses::types::{DynRlType, DynRlTypeDecl, FatType, Field, NominalGuard, Optionality, OptionalType, ResolvedLazy, ReturnType, RlImportedTypeDecl, RlImportedValueType, RlReturnType, RlType, RlTypeDecl, RlTypeParam, ThinType, ThinTypeDecl, TypeParam, Variance};
use crate::ast::tree_sitter::TSNode;
use crate::import_export::export::ImportPath;

pub trait AstNode<'tree> {
    fn node(&self) -> TSNode<'tree>;
}

pub trait TypedAstNode<'tree>: AstNode<'tree> {
    fn infer_type<'a>(&'a self, typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType;
}

pub trait AstValueBinding<'tree>: TypedAstNode<'tree> + ValueBinding<'tree> {
    /// Upcast on stable Rust
    fn up(&self) -> &DynValueBinding<'tree>;
}
pub type DynAstValueBinding<'tree> = dyn AstValueBinding<'tree> + 'tree;

pub trait AstTypeBinding<'tree>: AstNode<'tree> + TypeBinding {}
pub type DynAstTypeBinding<'tree> = dyn AstTypeBinding<'tree> + 'tree;

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
    pub shape: RlTypeParam
}

#[derive(Debug)]
pub struct AstType<'tree> {
    pub node: TSNode<'tree>,
    pub shape: RlType,
}

#[derive(Debug)]
pub struct AstReturnType<'tree> {
    pub node: TSNode<'tree>,
    pub shape: RlReturnType,
}

#[derive(Debug)]
pub struct AstParameter<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstValueIdent<'tree>,
    pub is_this_param: bool,
    pub is_rest_param: bool,
    pub is_optional: bool,
    pub type_: Option<AstType<'tree>>,
    pub value: Option<TSNode<'tree>>,
    backwards_hole: Option<Lazy<RlType>>,
    // pub local_uses: LocalUses<'tree>,
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
    pub value: Option<TSNode<'tree>>,
    // pub local_uses: LocalUses<'tree>,
}

#[derive(Debug)]
pub struct AstFunctionDecl<'tree> {
    pub node: TSNode<'tree>,
    pub name: AstValueIdent<'tree>,
    pub nominal_params: Vec<AstTypeParameter<'tree>>,
    pub formal_params: Vec<AstParameter<'tree>>,
    pub return_type: Option<AstReturnType<'tree>>,
    fn_type: RlType,
    // pub local_uses: LocalUses<'tree>,
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
    pub nominal_supertypes: Vec<AstType<'tree>>,
    pub typescript_supertype: Option<TSNode<'tree>>,
    pub guard: Option<AstNominalGuard<'tree>>,
    pub shape: RlTypeDecl,
}

#[derive(Debug)]
pub struct AstValueImportSpecifier<'tree> {
    pub node: TSNode<'tree>,
    pub original_name: AstValueIdent<'tree>,
    pub alias: AstValueIdent<'tree>,
    pub shape: RlImportedValueType,
}

#[derive(Debug)]
pub struct AstTypeImportSpecifier<'tree> {
    pub node: TSNode<'tree>,
    pub original_name: AstTypeIdent<'tree>,
    pub alias: AstTypeIdent<'tree>,
    pub shape: RlImportedTypeDecl,
}

#[derive(Debug)]
pub struct AstImportPath<'tree> {
    pub node: TSNode<'tree>,
    pub module_path: ImportPath,
}

#[derive(Debug)]
pub struct AstImportStatement<'tree> {
    pub node: TSNode<'tree>,
    pub path: AstImportPath<'tree>,
    pub imported_values: Vec<AstValueImportSpecifier<'tree>>,
    pub imported_types: Vec<AstTypeImportSpecifier<'tree>>,
    pub(crate) import_path_idx: usize
}

macro_rules! impl_ast {
    ($($Type:ident),+) => {
        $(
            impl<'tree> AstNode<'tree> for $Type<'tree> {
                fn node(&self) -> TSNode<'tree> {
                    self.node
                }
            }
        )+
    }
}

macro_rules! assert_kind {
    ($node:ident, [ $($NODE_KIND:literal),* $(,)? ]) => {
        if ![$($NODE_KIND),*].contains(&$node.kind()) {
            panic!("Expected {:?} to be one of {:?}, but was {:?}", $node, [$($NODE_KIND),*], $node.kind());
        }
    }
}

macro_rules! impl_ast_value_binding {
    ($($Type:ident $name:ident $locality:ident),+) => {
        $(
            impl_ast!($Type);

            impl<'tree> ValueBinding<'tree> for $Type<'tree> {
                fn name(&self) -> &ValueName {
                    &self.$name.name
                }

                fn value_type(&self) -> &DynRlType {
                    <Self as TypedAstNode>::infer_type(self, None)
                }

                fn locality(&self) -> Locality {
                    Locality::$locality
                }
            }

            impl<'tree> AstValueBinding<'tree> for $Type<'tree> {
                fn up(&self) -> &DynValueBinding<'tree> {
                    self
                }
            }
        )+
    }
}

macro_rules! impl_ast_local_value_binding {
    ($($Type:ident),+) => {
        $(
            impl_ast_value_binding!($Type name Local);

            impl<'tree> LocalValueBinding<'tree> for $Type<'tree> {
                // fn local_uses(&self) -> &LocalUses<'tree> {
                //     &self.local_uses
                // }
            }
        )+
    }
}

macro_rules! impl_ast_type_binding {
    ($($Type:ident $name:ident $locality:ident),+) => {
        $(
            impl_ast!($Type);

            impl<'tree> TypeBinding for $Type<'tree> {
                fn name(&self) -> &TypeName {
                    &self.$name.name
                }

                fn type_decl(&self) -> &DynRlTypeDecl {
                    &self.shape
                }

                fn locality(&self) -> Locality {
                    Locality::$locality
                }
            }

            impl<'tree> AstTypeBinding<'tree> for $Type<'tree> {}
        )+
    }
}

impl_ast!(AstValueIdent);
impl<'tree> AstValueIdent<'tree> {
    pub fn new(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["identifier"]);
        Self {
            node,
            name: ValueName::new(node.text()),
        }
    }
}

impl_ast!(AstTypeIdent);
impl<'tree> AstTypeIdent<'tree> {
    pub fn new(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_identifier"]);
        Self {
            node,
            name: TypeName::new(node.text()),
        }
    }
}

impl_ast_type_binding!(AstTypeParameter name Local);
impl<'tree> AstTypeParameter<'tree> {
    //noinspection DuplicatedCode
    pub fn new(scope: &ScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_parameter"]);
        let name = AstTypeIdent::new(node.named_child(0).unwrap());
        let nominal_supertypes = ast_nominal_supertypes(scope, node.named_child(1));
        let thin = TypeParam {
            variance_bound: node.field_child("variance")
                .map(Self::variance_from)
                .unwrap_or_default(),
            name: name.name.clone(),
            supers: nominal_supertypes.iter().map(|t| t.shape.thin.clone()).collect(),
        };
        Self {
            node,
            name,
            nominal_supertypes,
            shape: RlTypeParam::new(scope, thin)
        }
    }

    fn parse(node: TSNode<'tree>) -> TypeParam<ThinType> {
        assert_kind!(node, ["nominal_type_parameter"]);
        let name = AstTypeIdent::new(node.named_child(0).unwrap());
        let nominal_supertypes_thin = node.named_child(1).map(|node| {
            node.named_children(&mut node.walk())
                .map(AstType::parse)
                .collect::<Vec<_>>()
        }).unwrap_or_default();
        TypeParam {
            variance_bound: node.field_child("variance")
                .map(Self::variance_from)
                .unwrap_or_default(),
            name: name.name,
            supers: nominal_supertypes_thin
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

impl<'tree> LocalTypeBinding<'tree> for AstTypeParameter<'tree> {}

impl_ast!(AstType);
impl<'tree> AstType<'tree> {
    //noinspection DuplicatedCode
    pub fn of_annotation(scope: &ScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_annotation"]);
        node.mark();
        Self::new(scope, node.named_child(0).unwrap())
    }

    pub fn new(scope: &ScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, [
            "nominal_type_identifier",
            "parenthesized_nominal_type",
            "generic_nominal_type",
            "object_nominal_type",
            "array_nominal_type",
            "tuple_nominal_type",
            "function_nominal_type",
            "nullable_nominal_type",
        ]);
        let thin = Self::parse(node);
        Self {
            node,
            shape: RlType::new(scope, thin),
        }
    }

    fn parse(node: TSNode<'tree>) -> ThinType {
        match node.kind() {
            "nominal_type_identifier" => ThinType::ident2(node.text()),
            "parenthesized_nominal_type" => Self::parse(node.named_child(0).unwrap()),
            "generic_nominal_type" => ThinType::generic2(
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
                        .map(AstTypeParameter::parse),
                    this_param.map_or(ThinType::Any, Self::parse),
                    parameters
                        .named_children(&mut node.walk())
                        .filter(|x| Some(x) != this_param.as_ref() && Some(x) != rest_param.as_ref())
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
                let name = SmolStr::new(node.named_child(0).unwrap().text());
                let is_optional = node.field_child("is_optional").is_some();
                let type_ = Self::parse(node.named_child(1).unwrap().named_child(0).unwrap());
                Field {
                    name,
                    type_: OptionalType::new(type_, is_optional),
                }
            }
            "nominal_method_signature" => {
                let name = SmolStr::new(node.named_child(0).unwrap().text().to_string());
                let is_optional = node.field_child("is_optional").is_some();
                let parameters = node.named_child(2).unwrap();
                let this_param = parameters.field_child("this_param");
                let rest_param = parameters.field_child("rest_param");
                let method_type = ThinType::func(
                    node.named_child(1).unwrap()
                        .named_children(&mut node.walk())
                        .map(AstTypeParameter::parse),
                    this_param.map_or(ThinType::Any, Self::parse),
                    node.named_child(2).unwrap()
                        .named_children(&mut node.walk())
                        .filter(|x| Some(x) != this_param.as_ref() && Some(x) != rest_param.as_ref())
                        .map(Self::parse_optional),
                    rest_param.map_or(ThinType::empty_rest_arg(), Self::parse),
                    Self::parse_return(node.named_child(3).unwrap()),
                );
                Field {
                    name,
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

    fn parse_return(node: TSNode<'tree>) -> ReturnType<ThinType> {
        if node.text() == "Void" {
            ReturnType::Void
        } else {
            ReturnType::Type(Self::parse(node))
        }
    }
}

impl_ast!(AstReturnType);
impl<'tree> AstReturnType<'tree> {
    //noinspection DuplicatedCode
    pub fn of_annotation(scope: &ScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_annotation"]);
        node.mark();
        Self::new(scope, node.named_child(0).unwrap())
    }

    pub fn new(scope: &ScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, [
            "nominal_type_identifier",
            "parenthesized_nominal_type",
            "generic_nominal_type",
            "object_nominal_type",
            "array_nominal_type",
            "tuple_nominal_type",
            "function_nominal_type",
            "nullable_nominal_type",
        ]);
        let thin = AstType::parse_return(node);
        Self {
            node,
            shape: RlReturnType::new(scope, thin),
        }
    }
}

impl_ast_local_value_binding!(AstParameter);
impl<'tree> AstParameter<'tree> {
    pub fn formal(scope: &ScopePtr<'tree>, node: TSNode<'tree>, is_arrow: bool) -> Self {
        assert_kind!(node, ["required_parameter", "optional_parameter"]);
        let name_node = node.named_child(0).unwrap();
        let type_ = node.field_child("nominal_type")
            .map(|node| AstType::of_annotation(scope, node));
        Self {
            node,
            name: AstValueIdent::new(name_node),
            is_this_param: name_node.kind() == "this",
            is_rest_param: name_node.kind() == "rest_pattern",
            is_optional: node.field_child("is_optional").is_some(),
            type_,
            value: node.field_child("value"),
            backwards_hole: if is_arrow {
                Some(Lazy::new(|| ResolvedLazy::resolved(FatType::hole())))
            } else {
                None
            },
        }
    }

    pub fn single_arrow(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["identifier"]);
        Self {
            node,
            name: AstValueIdent::new(node),
            is_this_param: false,
            is_rest_param: false,
            is_optional: false,
            type_: None,
            value: None,
            backwards_hole: Some(Lazy::new(|| ResolvedLazy::resolved(FatType::hole()))),
        }
    }

    /// Whether this is a parameter of an arrow function. Arrow functions use backwards inference
    /// if the parameter type is not specified, whereas other functions assign `Any` type.
    pub fn is_arrow(&self) -> bool {
        self.backwards_hole.is_some()
    }

    /// Gets the actual thin type, which does not lookup the default value or assign a hole.
    ///
    /// **Important:** this returns `ThinType::NEVER` if the parameter is not typed,
    /// since parameters are contravariant, unless it's a declaration, in which case no
    /// explicit annotation implies `Any`.
    pub fn thin(&self) -> ThinType {
        self.type_.as_ref().map(|param_type| param_type.shape.thin.clone())
            .unwrap_or(if self.is_arrow() { ThinType::NEVER } else { ThinType::Any })
    }

    /// Like [AstParameter::thin] but returns an optional type (using `is_optional`)
    pub fn optional_thin(&self) -> OptionalType<ThinType> {
        OptionalType {
            optionality: Optionality::from(self.is_optional),
            type_: self.thin(),
        }
    }
}

impl<'tree> TypedAstNode<'tree> for AstParameter<'tree> {
    fn infer_type<'a>(&'a self, typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        match (&self.type_, self.value, typed_exprs) {
            (Some(type_), _, _) => &type_.shape,
            (None, Some(value), Some(typed_exprs)) => typed_exprs.get(value)
                .map(|determined_type| &determined_type.type_)
                .unwrap_or(RlType::ANY_REF),
            (None, _, _) => self.backwards_hole.as_ref()
                .map(|hole| Lazy::force(hole))
                .unwrap_or(RlType::ANY_REF),
        }
    }
}

impl<'tree> HoistedValueBinding<'tree> for AstParameter<'tree> {}

impl_ast!(AstReturn);
impl<'tree> AstReturn<'tree> {
    pub fn new(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["return_statement"]);
        Self {
            node,
            returned_value: node.last_named_child(),
        }
    }
}

impl_ast!(AstThrow);
impl<'tree> AstThrow<'tree> {
    pub fn new(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["throw_statement"]);
        Self {
            node,
            thrown_value: node.last_named_child(),
        }
    }
}

impl_ast_local_value_binding!(AstValueDecl);
impl<'tree> AstValueDecl<'tree> {
    pub fn new(scope: &ScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["variable_declarator"]);
        let type_ = node.field_child("nominal_type")
            .map(|node| AstType::of_annotation(scope, node));
        Self {
            node,
            name: AstValueIdent::new(node.named_child(0).unwrap()),
            type_,
            value: node.field_child("value"),
        }
    }
}

impl<'tree> TypedAstNode<'tree> for AstValueDecl<'tree> {
    fn infer_type<'a>(&'a self, typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        match (&self.type_, self.value, typed_exprs) {
            (Some(type_), _, _) => &type_.shape,
            (None, Some(value), Some(typed_exprs)) => typed_exprs.get(value)
                .map(|determined_type| &determined_type.type_)
                .unwrap_or(RlType::ANY_REF),
            (None, _, _) => RlType::ANY_REF
        }
    }
}

impl_ast_local_value_binding!(AstFunctionDecl);
impl<'tree> AstFunctionDecl<'tree> {
    pub fn new(scope: &ScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, [
            "function_declaration",
            "generator_function_declaration",
            "function_signature"
        ]);
        let nominal_params = Self::nominal_params(scope, node);
        let formal_params = node.field_child("parameters").unwrap()
            .named_children(&mut node.walk())
            .map(|node| AstParameter::formal(scope, node, false))
            .collect::<Vec<_>>();
        let return_type = node.field_child("nominal_return_type")
            .map(|node| AstReturnType::of_annotation(scope, node));
        let fn_type = ResolvedLazy::new(scope, ThinType::func(
            nominal_params.iter().map(|param| param.shape.thin.clone()),
            formal_params.iter().find(|param| param.is_this_param)
                .map(AstParameter::thin)
                .unwrap_or_default(),
            formal_params.iter().filter(|param| !param.is_this_param && !param.is_rest_param)
                .map(AstParameter::optional_thin),
            formal_params.iter().find(|param| param.is_rest_param)
                .map(AstParameter::thin)
                .unwrap_or(ThinType::empty_rest_arg()),
            return_type.as_ref().map(|return_type| return_type.shape.thin.clone())
                .unwrap_or_default()
        ));
        Self {
            node,
            name: AstValueIdent::new(node.named_child(0).unwrap()),
            nominal_params,
            formal_params,
            return_type,
            fn_type,
        }
    }

    pub fn nominal_params(scope: &ScopePtr<'tree>, node: TSNode<'tree>) -> Vec<AstTypeParameter<'tree>> {
        node.field_child("type_parameters").map(|node| {
            node.named_children(&mut node.walk())
                .filter(|node| node.kind() == "nominal_type_parameter")
                .map(|node| AstTypeParameter::new(scope, node))
                .collect()
        }).unwrap_or_default()
    }
}

impl<'tree> TypedAstNode<'tree> for AstFunctionDecl<'tree> {
    fn infer_type<'a>(&'a self, _typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        &self.fn_type
    }
}

impl_ast!(AstNominalGuard);
impl<'tree> AstNominalGuard<'tree> {
    pub fn new(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_guard"]);
        let parameter = AstValueIdent::new(node.named_child(0).unwrap());
        let body = node.named_child(1).unwrap();
        let shape = NominalGuard {
            param: parameter.name.clone(),
            body: body.to_subtree(),
        };
        Self {
            node,
            parameter,
            body,
            shape
        }
    }
}

impl_ast_type_binding!(AstTypeDecl name Local);
impl<'tree> AstTypeDecl<'tree> {
    pub fn new(scope: &ScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_declaration"]);
        node.mark();
        let name = AstTypeIdent::new(node.named_child(0).unwrap());
        let nominal_params = node.field_child("nominal_type_parameters").map(|x| {
            x.named_children(&mut node.walk())
                .map(|node| AstTypeParameter::new(scope, node))
                .collect::<Vec<_>>()
        }).unwrap_or_default();
        //noinspection DuplicatedCode
        let nominal_supertypes = ast_nominal_supertypes(scope, node.field_child("nominal_supertypes"));
        let typescript_supertype = node.field_child("type");
        let guard = node.field_child("guard").map(AstNominalGuard::new);
        let thin = ThinTypeDecl {
            name: name.name.clone(),
            type_params: nominal_params.iter().map(|x| x.shape.thin.clone()).collect(),
            supertypes: vec![],
            typescript_supertype: None,
            guard: None,
        };
        Self {
            node,
            name,
            nominal_params,
            nominal_supertypes,
            typescript_supertype,
            guard,
            shape: RlTypeDecl::new(scope, thin),
        }
    }
}

impl_ast_value_binding!(AstValueImportSpecifier alias Imported);
impl<'tree> AstValueImportSpecifier<'tree> {
    pub fn new(
        scope: &ScopePtr,
        import_path_idx: usize,
        node: TSNode<'tree>,
    ) -> Self {
        assert_kind!(node, ["import_specifier"]);
        let original_name = AstValueIdent::new(node.named_child(0).unwrap());
        let alias = node.named_child(1).map_or_else(
            || original_name.clone(),
            AstValueIdent::new
        );
        let thin = ScopeValueImportIdx {
            import_path_idx,
            imported_name: original_name.name.clone()
        };
        Self {
            node,
            original_name,
            alias,
            shape: RlImportedValueType::new(scope, thin),
        }
    }
}

impl<'tree> TypedAstNode<'tree> for AstValueImportSpecifier<'tree> {
    fn infer_type<'a>(&'a self, _typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        &self.shape
    }
}

impl_ast_type_binding!(AstTypeImportSpecifier alias Imported);
impl<'tree> AstTypeImportSpecifier<'tree> {
    pub fn new(
        scope: &ScopePtr,
        import_path_idx: usize,
        node: TSNode<'tree>,
    ) -> Self {
        assert_kind!(node, ["import_specifier"]);
        node.mark();
        let original_name = AstTypeIdent::new(node.named_child(0).unwrap());
        let alias = node.named_child(1).map_or_else(
            || original_name.clone(),
            AstTypeIdent::new
        );
        let thin = ScopeTypeImportIdx {
            import_path_idx,
            imported_name: original_name.name.clone()
        };
        Self {
            node,
            original_name,
            alias,
            shape: RlImportedTypeDecl::new(scope, thin),
        }
    }
}

impl_ast!(AstImportPath);
impl<'tree> AstImportPath<'tree> {
    pub fn new(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["string"]);
        let module_path = ImportPath::from(unquote(node.text()).expect("import path is not a well-formed string literal"));
        Self {
            node,
            module_path,
        }
    }
}

impl_ast!(AstImportStatement);
impl<'tree> AstImportStatement<'tree> {
    pub fn new(
        scope: &ScopePtr,
        import_path_idx: usize,
        node: TSNode<'tree>,
    ) -> Self {
        assert_kind!(node, ["import_statement"]);
        let path = AstImportPath::new(node.named_child(1).unwrap());
        let import_container = node.named_child(0).unwrap();
        let imported_values = import_container
            .named_children(&mut node.walk())
            .filter(|node| node.named_child(0).unwrap().kind() == "identifier")
            .map(|node| AstValueImportSpecifier::new(scope, import_path_idx, node))
            .collect();
        let imported_types = import_container
            .named_children(&mut node.walk())
            .filter(|node| node.named_child(0).unwrap().kind() == "nominal_type_identifier")
            .map(|node| AstTypeImportSpecifier::new(scope, import_path_idx, node))
            .collect();
        if let Some(invalid_specifier) = import_container
            .named_children(&mut node.walk())
            .find(|node| node.named_child(0).unwrap().kind() != "identifier" && node.named_child(0).unwrap().kind() != "nominal_type_identifier") {
            panic!("import statement contains an invalid import specifier {:?} of kind {:?}", invalid_specifier, invalid_specifier.kind());
        }
        Self {
            node,
            path,
            imported_values,
            imported_types,
            import_path_idx,
        }
    }
}

fn ast_nominal_supertypes<'tree>(scope: &ScopePtr<'tree>, node: Option<TSNode<'tree>>) -> Vec<AstType<'tree>> {
    node.map(|x| {
        x.named_children(&mut x.walk())
            .map(|node| AstType::of_annotation(scope, node))
            .collect::<Vec<_>>()
    }).unwrap_or_default()
}