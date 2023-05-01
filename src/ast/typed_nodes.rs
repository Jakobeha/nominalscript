use std::rc::Rc;
use enquote::unquote;
use once_cell::unsync::{Lazy, OnceCell};

use crate::analyses::bindings::{DynValueBinding, FieldName, HoistedValueBinding, Locality, LocalTypeBinding, LocalValueBinding, TypeBinding, TypeIdent, TypeName, ValueBinding, ValueIdent, ValueName};
use crate::analyses::scopes::{ExprTypeMap, InactiveScopePtr, ScopeTypeImportIdx, ScopeValueImportIdx};
use crate::analyses::types::{DeterminedType, DynRlType, DynRlTypeDecl, FatType, Field, HasNullability, NominalGuard, Optionality, OptionalType, ResolveCtx, ResolvedLazy, ReturnType, RlImportedTypeDecl, RlImportedValueType, RlReturnType, RlType, RlTypeDecl, RlTypeParam, ThinType, ThinTypeDecl, TypeParam, Variance};
use crate::ast::ann::Ann;
use crate::ast::tree_sitter::TSNode;
use crate::{impl_has_ann_record_struct, impl_has_ann_wrapper_struct};
use crate::import_export::export::ImportPath;

pub trait AstNode<'tree> {
    fn node(&self) -> TSNode<'tree>;
}

pub trait TypedAstNode<'tree>: AstNode<'tree> {
    fn type_annotation(&self) -> Option<&AstType<'tree>>;
    fn infer_type<'a>(&'a self, typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType;
}

pub trait AstValueBinding<'tree>: TypedAstNode<'tree> + ValueBinding<'tree> {
    /// Upcast on stable Rust
    fn up(&self) -> &DynValueBinding<'tree>;
    /// Downcast on stable Rust
    fn down_to_fn_decl(&self) -> Option<&AstFunctionDecl<'tree>>;
}
pub type DynAstValueBinding<'tree> = dyn AstValueBinding<'tree> + 'tree;

pub trait AstTypeBinding<'tree>: AstNode<'tree> + TypeBinding {}
pub type DynAstTypeBinding<'tree> = dyn AstTypeBinding<'tree> + 'tree;

/// Arbitrary expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueExpr<'tree>(TSNode<'tree>);

/// Formal parameter
#[derive(Debug)]
pub struct ValueParameter<'tree> {
    pub ann: Ann<'tree>,
    pub name: ValueIdent<'tree>,
    pub is_this_param: bool,
    pub is_rest_param: bool,
    pub is_optional: bool,
    pub type_: Option<ValueParameterType<'tree>>,
    pub value: Option<ValueExpr<'tree>>,
}
impl_has_ann_record_struct!(ValueParameter);

/// Formal parameter's type, which is different from [RlType] because it sometimes it's inferred
#[derive(Debug)]
pub enum ValueParameterType<'tree> {
    /// Explicitly provided type
    Explicit { type_: RlType<'tree>> },
    /// Backwards-inferred hole
    Hole { hole: Lazy<RlType<'tree>> }
}

/// "Parameter" bound in a `catch` clause
#[derive(Debug)]
pub struct CatchParameter<'tree> {
    pub name: AstValueIdent<'tree>,
}
impl_has_ann_wrapper_struct!(CatchParameter by name);

/// Return statement
#[derive(Debug, Clone, Copy)]
pub struct Return<'tree> {
    pub ann: Ann<'tree>,
    pub returned_value: Option<ValueExpr<'tree>>,
}
impl_has_ann_record_struct!(Return);

/// Throw statement
#[derive(Debug, Clone, Copy)]
pub struct Throw<'tree> {
    pub ann: Ann<'tree>,
    pub thrown_value: Option<ValueExpr<'tree>>,
}
impl_has_ann_record_struct!(Throw);

/// Value declaration
#[derive(Debug)]
pub struct ValueDecl<'tree> {
    pub ann: Ann<'tree>,
    pub name: ValueIdent<'tree>,
    pub type_: Option<RlType<'tree>>,
    pub value: Option<ValueExpr<'tree>>,
}

/// Function declaration
#[derive(Debug)]
pub struct FunctionDecl<'tree> {
    pub ann: Ann<'tree>,
    pub name: ValueIdent<'tree>,
    pub nominal_params: Vec<RlTypeParameter<'tree>>,
    pub formal_params: Vec<Rc<ValueParameter<'tree>>>,
    pub return_type: Option<RlReturnType<'tree>>,
    fn_type: RlType<'tree>,
    custom_inferred_fn_type: OnceCell<RlType<'tree>>,
}

// TODO: Should we refactor the import specifier into RlImportedValueType?
//   Probably, but wait until we have a better idea of this design
/// Value import specifier (`bar as baz` in `import {bar as baz, ;Bar as Baz} from 'foo'`)
#[derive(Debug)]
pub struct ValueImportSpecifier<'tree> {
    pub ann: Ann<'tree>,
    pub original_name: ValueIdent<'tree>,
    pub alias: ValueIdent<'tree>,
    pub shape: RlImportedValueType<'tree>,
}

/// Type import specifier (`Bar as Baz` in `import {bar as baz, ;Bar as Baz} from 'foo'`)
#[derive(Debug)]
pub struct TypeImportSpecifier<'tree> {
    pub ann: Ann<'tree>,
    pub original_name: TypeIdent<'tree>,
    pub alias: TypeIdent<'tree>,
    pub shape: RlImportedTypeDecl<'tree>,
}

/// Import path (`'foo'` in `import {bar as baz, ;Bar as Baz} from 'foo'`)
#[derive(Debug)]
pub struct ImportPathAst<'tree> {
    pub ann: Ann<'tree>,
    pub module_path: ImportPath,
}

/// Import statement (the entire thing `import {bar as baz, ;Bar as Baz} from 'foo'`)
#[derive(Debug)]
pub struct ImportStatement<'tree> {
    pub ann: Ann<'tree>,
    pub path: ImportPathAst<'tree>,
    pub imported_values: Vec<ValueImportSpecifier<'tree>>,
    pub imported_types: Vec<TypeImportSpecifier<'tree>>,
    pub(crate) import_path_idx: usize
}

macro_rules! impl_ast1 {
    (AstCatchParameter) => {
impl<'tree> AstNode<'tree> for AstCatchParameter<'tree> {
    fn node(&self) -> TSNode<'tree> {
        self.name.node()
    }
}
    };
    ($Type:ident) => {
impl<'tree> AstNode<'tree> for $Type<'tree> {
    fn node(&self) -> TSNode<'tree> {
        self.node
    }
}
    }
}

macro_rules! impl_ast {
    ($($Type:ident),+) => {
        $(
            impl_ast1!($Type);
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

macro_rules! impl_down_to_fn_decl {
    (AstFunctionDecl) => {
        impl<'tree> AstFunctionDecl<'tree> {
            fn _down_to_fn_decl(&self) -> Option<&AstFunctionDecl<'tree>> {
                Some(self)
            }
        }
    };
    ($Type:ident) => {
        impl<'tree> $Type<'tree> {
            fn _down_to_fn_decl(&self) -> Option<&AstFunctionDecl<'tree>> {
                None
            }
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

                fn infer_type_det(&self, typed_exprs: Option<&ExprTypeMap<'tree>>, ctx: &ResolveCtx<'_>) -> DeterminedType<'tree> {
                    DeterminedType {
                        type_: <Self as TypedAstNode>::infer_type(self, typed_exprs).normalize_clone(ctx),
                        defined_value: Some(self.node()),
                        explicit_type: self.type_annotation().map(|t| t.node()),
                    }
                }
            }

            impl<'tree> AstValueBinding<'tree> for $Type<'tree> {
                fn up(&self) -> &DynValueBinding<'tree> {
                    self
                }

                fn down_to_fn_decl(&self) -> Option<&AstFunctionDecl<'tree>> {
                    self._down_to_fn_decl()
                }
            }

            impl_down_to_fn_decl!($Type);
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
    pub fn new(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
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
    pub fn of_annotation(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_annotation"]);
        node.mark();
        Self::new(scope, node.named_child(0).unwrap())
    }

    pub fn new(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
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
            "nominal_type_identifier" => ThinType::ident2(node.text(), node.to_ptr()),
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
                    node.field_child("nominal_type_parameters").map(|x| {
                        x.named_children(&mut node.walk()).map(AstTypeParameter::parse).collect::<Vec<_>>()
                    }).unwrap_or_default(),
                    this_param.map_or(ThinType::Any, Self::parse),
                    parameters
                        .named_children(&mut node.walk())
                        .filter(|x| Some(x) != this_param.as_ref() && Some(x) != rest_param.as_ref())
                        .map(Self::parse_optional),
                    rest_param.map_or(ThinType::EMPTY_REST_ARG, Self::parse),
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
            "nullable_nominal_type" => {
                let mut type_ = Self::parse(node.named_child(0).unwrap());
                type_.make_nullable();
                type_
            },
            _ => panic!("unhandled node kind: {}", node.kind()),
        }
    }

    fn parse_field(node: TSNode<'tree>) -> Field<OptionalType<ThinType>> {
        match node.kind() {
            "nominal_property_signature" => {
                let name = FieldName::new(node.named_child(0).unwrap().text());
                let is_optional = node.field_child("is_optional").is_some();
                let type_ = Self::parse(node.named_child(1).unwrap().named_child(0).unwrap());
                Field {
                    name,
                    type_: OptionalType::new(type_, is_optional),
                }
            }
            "nominal_method_signature" => {
                let name = FieldName::new(node.named_child(0).unwrap().text().to_string());
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
                    rest_param.map_or(ThinType::EMPTY_REST_ARG, Self::parse),
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
    pub fn of_annotation(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_annotation"]);
        node.mark();
        Self::new(scope, node.named_child(0).unwrap())
    }

    pub fn new(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
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
    pub fn formal(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>, is_arrow: bool) -> Self {
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
    fn type_annotation(&self) -> Option<&AstType<'tree>> {
        self.type_.as_ref()
    }
    
    fn infer_type<'a>(&'a self, typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        match (&self.type_, self.value, typed_exprs) {
            (Some(type_), _, _) => &type_.shape,
            (None, Some(value), Some(typed_exprs)) => typed_exprs.get(value)
                .map(|determined_type| &determined_type.type_)
                .unwrap_or(RlType::any_ref()),
            (None, _, _) => self.backwards_hole.as_ref()
                .map(|hole| Lazy::force(hole))
                .unwrap_or(RlType::any_ref()),
        }
    }
}

impl<'tree> HoistedValueBinding<'tree> for AstParameter<'tree> {}

impl_ast_local_value_binding!(AstCatchParameter);
impl<'tree> AstCatchParameter<'tree> {
    pub fn new(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["identifier"]);
        Self {
            name: AstValueIdent::new(node)
        }
    }
}

impl<'tree> TypedAstNode<'tree> for AstCatchParameter<'tree> {
    fn type_annotation(&self) -> Option<&AstType<'tree>> {
        None
    }

    fn infer_type<'a>(&'a self, _typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        RlType::any_ref()
    }
}

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
    pub fn new(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
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
    fn type_annotation(&self) -> Option<&AstType<'tree>> {
        self.type_.as_ref()
    }
    
    fn infer_type<'a>(&'a self, typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        match (&self.type_, self.value, typed_exprs) {
            (Some(type_), _, _) => &type_.shape,
            (None, Some(value), Some(typed_exprs)) => typed_exprs.get(value)
                .map(|determined_type| &determined_type.type_)
                .unwrap_or(RlType::any_ref()),
            (None, _, _) => RlType::any_ref()
        }
    }
}

impl_ast_local_value_binding!(AstFunctionDecl);
impl<'tree> AstFunctionDecl<'tree> {
    pub fn new(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, [
            "function_declaration",
            "generator_function_declaration",
            "function_signature"
        ]);
        let (
            nominal_params,
            formal_params,
            return_type,
            fn_type
        ) = Self::parse_common(scope, node);
        Self {
            node,
            name: AstValueIdent::new(node.named_child(0).unwrap()),
            nominal_params,
            formal_params,
            return_type,
            fn_type,
            custom_inferred_fn_type: OnceCell::new()
        }
    }

    /// Extract common information within both function declarations *and* function expressions
    pub fn parse_common(
        scope: &InactiveScopePtr<'tree>,
        node: TSNode<'tree>
    ) -> (Vec<AstTypeParameter<'tree>>, Vec<Rc<AstParameter<'tree>>>, Option<AstReturnType<'tree>>, RlType) {
        let nominal_params = Self::nominal_params_of(scope, node);
        let formal_params = match node.field_child("parameters") {
            None => vec![Rc::new(AstParameter::single_arrow(node.field_child("parameter").unwrap()))],
            Some(parameters) => parameters
                .named_children(&mut node.walk())
                .map(|node| Rc::new(AstParameter::formal(scope, node, false)))
                .collect::<Vec<_>>()
        };
        let return_type = node.field_child("nominal_return_type")
            .map(|node| AstReturnType::of_annotation(scope, node));
        let fn_type = ResolvedLazy::new(scope, ThinType::func(
            nominal_params.iter().map(|param| param.shape.thin.clone()),
            formal_params.iter().find(|param| param.is_this_param)
                .map(|p| p.thin())
                .unwrap_or_default(),
            formal_params.iter().filter(|param| !param.is_this_param && !param.is_rest_param)
                .map(|p| p.optional_thin()),
            formal_params.iter().find(|param| param.is_rest_param)
                .map(|p| p.thin())
                .unwrap_or(ThinType::EMPTY_REST_ARG),
            return_type.as_ref().map(|return_type| return_type.shape.thin.clone())
                .unwrap_or_default()
        ));
        (nominal_params, formal_params, return_type, fn_type)
    }

    fn nominal_params_of(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Vec<AstTypeParameter<'tree>> {
        node.field_child("type_parameters").map(|node| {
            node.named_children(&mut node.walk())
                .filter(|node| node.kind() == "nominal_type_parameter")
                .map(|node| AstTypeParameter::new(scope, node))
                .collect()
        }).unwrap_or_default()
    }

    pub fn set_custom_inferred_return_type(&self, return_type: RlReturnType, ctx: &ResolveCtx<'_>) {
        self.custom_inferred_fn_type.set(self.fn_type.clone().with_return_type(return_type, ctx)).expect("custom_infer_return_type called twice");
    }
}

impl<'tree> TypedAstNode<'tree> for AstFunctionDecl<'tree> {
    fn type_annotation(&self) -> Option<&AstType<'tree>> {
        None
    }
    
    fn infer_type<'a>(&'a self, _typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        self.custom_inferred_fn_type.get().unwrap_or(&self.fn_type)
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
    pub fn new(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
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
        scope: &InactiveScopePtr,
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
    fn type_annotation(&self) -> Option<&AstType<'tree>> {
        None
    }
    
    fn infer_type<'a>(&'a self, _typed_exprs: Option<&'a ExprTypeMap<'tree>>) -> &'a DynRlType {
        &self.shape
    }
}

impl_ast_type_binding!(AstTypeImportSpecifier alias Imported);
impl<'tree> AstTypeImportSpecifier<'tree> {
    pub fn new(
        scope: &InactiveScopePtr,
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

impl_ast!(ImportPathAst);
impl<'tree> ImportPathAst<'tree> {
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
        scope: &InactiveScopePtr,
        import_path_idx: usize,
        node: TSNode<'tree>,
    ) -> Self {
        assert_kind!(node, ["import_statement"]);
        let path = ImportPathAst::new(node.named_child(1).unwrap());
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

fn ast_nominal_supertypes<'tree>(scope: &InactiveScopePtr<'tree>, node: Option<TSNode<'tree>>) -> Vec<AstType<'tree>> {
    node.map(|x| {
        x.mark();
        x.named_children(&mut x.walk())
            .map(|node| AstType::new(scope, node))
            .collect::<Vec<_>>()
    }).unwrap_or_default()
}