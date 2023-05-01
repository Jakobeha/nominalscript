use enquote::unquote;
use once_cell::unsync::OnceCell;

use crate::{impl_has_ann_record_struct, impl_has_ann_wrapper_struct};
use crate::analyses::bindings::{DynValueBinding, FieldIdent, FieldNameStr, HoistedValueBinding, Locality, LocalTypeBinding, LocalValueBinding, TypeBinding, TypeIdent, TypeNameStr, ValueBinding, ValueIdent, ValueNameStr};
use crate::analyses::scopes::{ExprTypeMap, InactiveScopePtr, ScopeTypeImportIdx, ScopeValueImportIdx};
use crate::analyses::types::{DeterminedType, DynRlType, DynRlTypeDecl, Field, HasNullability, InferrableThinType, NominalGuard, Optionality, OptionalType, ResolveCtx, ResolvedLazy, ResolveInto, ReturnType, RlInferrableType, RlReturnType, RlType, RlTypeDecl, RlTypeParam, StatementBlock, ThinType, ThinTypeDecl, TypeParam, TypescriptType, Variance};
use crate::ast::ann::{Ann, HasAnn};
use crate::ast::tree_sitter::TSNode;
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
    pub type_: Option<RlInferrableType<'tree>>,
    pub value: Option<ValueExpr<'tree>>,
}
impl_has_ann_record_struct!(ValueParameter);

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
    pub formal_params: Vec<ValueParameter<'tree>>,
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
    pub idx: ScopeValueImportIdx<'tree>,
    pub alias: ValueIdent<'tree>,
}

/// Type import specifier (`Bar as Baz` in `import {bar as baz, ;Bar as Baz} from 'foo'`)
#[derive(Debug)]
pub struct TypeImportSpecifier<'tree> {
    pub ann: Ann<'tree>,
    pub idx: ScopeTypeImportIdx<'tree>,
    pub alias: TypeIdent<'tree>,
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
            impl<'tree> ValueBinding<'tree> for $Type<'tree> {
                fn name(&self) -> &ValueIdent<'tree> {
                    &self.$name
                }

                fn value_type(&self) -> &DynRlType<'tree> {
                    <Self as _ValueBinding<'tree>>::value_type(self)
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
        )+
    }
}

macro_rules! impl_ast_type_binding {
    ($($Type:ident $name:ident $locality:ident),+) => {
        $(
            impl<'tree> TypeBinding<'tree> for $Type<'tree> {
                fn name(&self) -> &TypeIdent<'tree> {
                    &self.$name
                }

                fn type_decl(&self) -> &DynRlTypeDecl<'tree> {
                    <Self as _TypeBinding<'tree>>::type_decl(self)
                }

                fn locality(&self) -> Locality {
                    Locality::$locality
                }
            }
        )+
    }
}

trait _ValueBinding<'tree> {
    fn value_type(&self) -> &DynRlType<'tree>;
}

trait _TypeBinding<'tree> {
    fn type_decl(&self) -> &DynRlTypeDecl<'tree>;
}

impl_ast_type_binding!(RlTypeParam name Local);
impl<'tree> _TypeBinding<'tree> for RlTypeParam<'tree> {
    fn type_decl(&self) -> &DynRlTypeDecl<'tree> {
        self as &DynRlTypeDecl<'tree>
    }
}

impl_ast_value_binding!(ValueParameter name Local);

pub trait ParseableWithScope<'tree> {
    fn parse(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self;
}

pub trait ParseableAnnotationWithScope<'tree> {
    fn parse_annotation(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self;
}

pub trait Parseable<'tree> {
    fn parse(node: TSNode<'tree>) -> Self;
}

pub trait ParseableAnnotation<'tree>: Parseable<'tree> {
    fn parse_annotation(node: TSNode<'tree>) -> Self where Self: Sized {
        assert_kind!(node, ["nominal_type_annotation"]);
        node.mark();
        Self::parse(node.named_child(0).unwrap())
    }
}

impl<'tree, Thin: Parseable<'tree> + ResolveInto<'tree, Fat>, Fat> ParseableWithScope<'tree> for ResolvedLazy<Thin, Fat> {
    fn parse(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        ResolvedLazy::new(scope, Thin::parse(node))
    }
}

impl<'tree, Thin: ParseableAnnotation<'tree> + ResolveInto<'tree, Fat>, Fat> ParseableAnnotationWithScope<'tree> for ResolvedLazy<Thin, Fat> {
    fn parse_annotation(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        ResolvedLazy::new(scope, Thin::parse_annotation(node))
    }
}

impl<'tree> Parseable<'tree> for ValueIdent<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["identifier"]);
        Self {
            ann: Ann::DirectSource { loc: node },
            name: ValueNameStr::of(node.text()),
        }
    }
}

impl<'tree> Parseable<'tree> for TypeIdent<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_identifier"]);
        Self {
            ann: Ann::DirectSource { loc: node },
            name: TypeNameStr::of(node.text()),
        }
    }
}

impl<'tree> Parseable<'tree> for NominalGuard<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_guard"]);
        Self {
            ann: Ann::DirectSource { loc: node },
            param: ValueIdent::parse(node.named_child(0).unwrap()),
            body: StatementBlock::parse(node.named_child(1).unwrap()),
        }
    }
}

impl<'tree> Parseable<'tree> for StatementBlock<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["statement_block"]);
        Self(node)
    }
}

impl<'tree> Parseable<'tree> for TypescriptType<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        TypescriptType(node)
    }
}

impl<'tree> Parseable<'tree> for ValueExpr<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        ValueExpr(node)
    }
}

impl<'tree> Parseable<'tree> for FieldIdent<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["identifier"]);
        Self {
            ann: Ann::DirectSource { loc: node },
            name: FieldNameStr::of(node.text()),
        }
    }
}

impl<'tree> Parseable<'tree> for TypeParam<'tree, ThinType<'tree>> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_parameter"]);
        TypeParam {
            ann: Ann::DirectSource { loc: node },
            variance_bound: node.field_child("variance")
                .map(Variance::parse)
                .unwrap_or_default(),
            name: TypeIdent::parse(node.named_child(0).unwrap()),
            supers: parse_nominal_supertypes(node.named_child(1)),
        }
    }
}

impl<'tree> Parseable<'tree> for Variance {
    fn parse(node: TSNode<'tree>) -> Self {
        match node.text() {
            "biv" => Variance::Bivariant,
            "cov" => Variance::Covariant,
            "con" => Variance::Contravariant,
            "inv" => Variance::Invariant,
            _ => panic!("Invalid variance: {}", node.text()),
        }
    }
}

impl<'tree> Parseable<'tree> for ThinType<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        let ann = Ann::DirectSource { loc: node };
        match node.kind() {
            "nominal_type_identifier" => ThinType::ident2(ann, node.text()),
            "parenthesized_nominal_type" => Self::parse(node.named_child(0).unwrap()),
            "generic_nominal_type" => ThinType::generic2(
                ann,
                node.named_child(0).unwrap().text(),
                node.named_child(1).unwrap().named_children(&mut node.walk()).map(Self::parse),
            ),
            "function_nominal_type" => {
                let parameters = node.field_child("parameters").unwrap();
                let this_param = parameters.field_child("this_param");
                let rest_param = parameters.field_child("rest_param");
                ThinType::func(
                    ann,
                    node.field_child("nominal_type_parameters").map(|x| {
                        x.named_children(&mut node.walk()).map(AstTypeParameter::parse).collect::<Vec<_>>()
                    }).unwrap_or_default(),
                    this_param.map_or(ThinType::Any, Self::parse),
                    parameters
                        .named_children(&mut node.walk())
                        .filter(|x| Some(x) != this_param.as_ref() && Some(x) != rest_param.as_ref())
                        .map(OptionalType::parse),
                    rest_param.map_or(ThinType::EMPTY_REST_ARG, Self::parse),
                    ReturnType::parse(node.field_child("return_type").unwrap()),
                )
            }
            "array_nominal_type" => ThinType::array(
                ann,
                Self::parse(node.named_child(0).unwrap()),
            ),
            "tuple_nominal_type" => ThinType::tuple(
                ann,
                node.named_children(&mut node.walk())
                    .map(OptionalType::parse),
            ),
            "object_nominal_type" => ThinType::object(
                ann,
                node.named_children(&mut node.walk()).map(Field::parse)
            ),
            "nullable_nominal_type" => {
                let mut type_ = Self::parse(node.named_child(0).unwrap());
                type_.make_nullable();
                type_
            },
            _ => panic!("unhandled node kind: {}", node.kind()),
        }
    }
}
impl<'tree> ParseableAnnotation<'tree> for ThinType<'tree> {}

impl<'tree> Parseable<'tree> for InferrableThinType<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        InferrableThinType::Explicit(ThinType::parse(node))
    }
}

impl<'tree> ParseableAnnotation<'tree> for InferrableThinType<'tree> {}

impl<'tree> Parseable<'tree> for Field<'tree, OptionalType<'tree, ThinType<'tree>>> {
    fn parse(node: TSNode<'tree>) -> Self {
        let ann = Ann::DirectSource { loc: node };
        match node.kind() {
            "nominal_property_signature" => {
                let name = FieldIdent::parse(node.named_child(0).unwrap());
                let is_optional = node.field_child("is_optional").is_some();
                let type_ = ThinType::parse(node.named_child(1).unwrap().named_child(0).unwrap());
                Field {
                    ann,
                    name,
                    type_: OptionalType::new(ann, type_, is_optional),
                }
            }
            "nominal_method_signature" => {
                let name = FieldIdent::parse(node.named_child(0).unwrap());
                let is_optional = node.field_child("is_optional").is_some();
                let parameters = node.named_child(2).unwrap();
                let this_param = parameters.field_child("this_param");
                let rest_param = parameters.field_child("rest_param");
                let method_type = ThinType::func(
                    ann,
                    node.named_child(1).unwrap()
                        .named_children(&mut node.walk())
                        .map(AstTypeParameter::parse),
                    this_param.map_or(ThinType::Any, ThinType::parse),
                    node.named_child(2).unwrap()
                        .named_children(&mut node.walk())
                        .filter(|x| Some(x) != this_param.as_ref() && Some(x) != rest_param.as_ref())
                        .map(OptionalType::parse),
                    rest_param.map_or(ThinType::EMPTY_REST_ARG, ThinType::parse),
                    ReturnType::parse(node.named_child(3).unwrap()),
                );
                Field {
                    ann,
                    name,
                    type_: OptionalType::new(ann, method_type, is_optional),
                }
            }
            _ => panic!("unhandled node kind: {}", node.kind()),
        }
    }
}

impl<'tree> Parseable<'tree> for OptionalType<'tree, ThinType<'tree>> {
    fn parse(node: TSNode<'tree>) -> Self {
        let ann = Ann::DirectSource { loc: node };
        if node.kind() == "optional_nominal_type" {
            OptionalType::optional(ann, ThinType::parse(node.named_child(0).unwrap()))
        } else {
            OptionalType::required(ann, ThinType::parse(node))
        }
    }
}

impl<'tree> Parseable<'tree> for ReturnType<'tree, ThinType<'tree>> {
    fn parse(node: TSNode<'tree>) -> Self {
        if node.text() == "Void" {
            ReturnType::Void { ann: Ann::DirectSource { loc: node } }
        } else {
            ReturnType::Type(ThinType::parse(node))
        }
    }
}
impl<'tree> ParseableAnnotation<'tree> for ReturnType<'tree, ThinType<'tree>> {}

impl<'tree> ValueParameter<'tree> {
    pub fn parse_formal(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>, is_arrow: bool) -> Self {
        assert_kind!(node, ["required_parameter", "optional_parameter"]);
        let ann = Ann::DirectSource { loc: node };
        let name_node = node.named_child(0).unwrap();
        let type_ = node.field_child("nominal_type")
            .map(|node| ResolvedLazy::parse_annotation(scope, node))
            .or_else(|| match is_arrow {
                false => None,
                true => Some(ResolvedLazy::new(scope, InferrableThinType::Hole { ann }))
            });
        Self {
            ann,
            name: ValueIdent::parse(name_node),
            is_this_param: name_node.kind() == "this",
            is_rest_param: name_node.kind() == "rest_pattern",
            is_optional: node.field_child("is_optional").is_some(),
            type_,
            value: node.field_child("value").map(ValueExpr::parse),
        }
    }

    pub fn parse_single_arrow(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["identifier"]);
        let ann = Ann::DirectSource { loc: node };
        Self {
            ann,
            name: AstValueIdent::new(node),
            is_this_param: false,
            is_rest_param: false,
            is_optional: false,
            type_: Some(ResolvedLazy::new(scope, InferrableThinType::Hole { ann })),
            value: None,
        }
    }

    /// Whether this is a parameter of an arrow function. Arrow functions use backwards inference
    /// if the parameter type is not specified, whereas other functions assign `Any` type.
    pub fn is_arrow(&self) -> bool {
        matches!(self.type_, Some(type_) if type_.thin.is_hole());
    }

    /// Gets the actual thin type, which does not lookup the default value or assign a hole.
    pub fn thin(&self) -> InferrableThinType<'tree> {
        self.type_.as_ref().map_or(
            InferrableThinType::Explicit(ThinType::Any { ann: self.ann }),
            |param_type| param_type.thin.clone()
        )
    }

    /// Like [AstParameter::thin] but returns an optional type (using `is_optional`)
    pub fn optional_thin(&self) -> OptionalType<'tree, InferrableThinType<'tree>> {
        OptionalType {
            ann: self.ann,
            optionality: Optionality::from(self.is_optional),
            type_: self.thin(),
        }
    }
}

impl_ast_value_binding!(CatchParameter name Local);
impl<'tree> Parseable<'tree> for CatchParameter<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["identifier"]);
        Self { name: ValueIdent::parse(node) }
    }
}

impl<'tree> Parseable<'tree> for Return<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["return_statement"]);
        Self {
            ann: Ann::DirectSource { loc: node },
            returned_value: node.last_named_child().map(ValueExpr::parse),
        }
    }
}

impl<'tree> Parseable<'tree> for Throw<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["throw_statement"]);
        Self {
            ann: Ann::DirectSource { loc: node },
            thrown_value: node.last_named_child().map(ValueExpr::parse),
        }
    }
}

impl_ast_value_binding!(ValueDecl name Local);
impl<'tree> ParseableWithScope<'tree> for ValueDecl<'tree> {
    fn parse(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["variable_declarator"]);
        let type_ = node.field_child("nominal_type")
            .map(|node| RlType::parse_annotation(scope, node));
        Self {
            ann: Ann::DirectSource { loc: node },
            name: ValueIdent::parse(node.named_child(0).unwrap()),
            type_,
            value: node.field_child("value").map(ValueExpr::parse),
        }
    }
}

impl_ast_value_binding!(FunctionDecl name Local);
impl<'tree> ParseableWithScope<'tree> for FunctionDecl<'tree> {
    fn parse(scope: &InactiveScopePtr<'tree>, node: TSNode<'tree>) -> Self {
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
            ann: Ann::DirectSource { loc: node },
            name: ValueIdent::parse(node.named_child(0).unwrap()),
            nominal_params,
            formal_params,
            return_type,
            fn_type,
            custom_inferred_fn_type: OnceCell::new()
        }
    }
}

impl<'tree> FunctionDecl<'tree> {
    /// Extract common information within both function declarations *and* function expressions
    pub fn parse_common(
        scope: &InactiveScopePtr<'tree>,
        node: TSNode<'tree>
    ) -> (Vec<RlTypeParam<'tree>>, Vec<ValueParameter<'tree>>, Option<RlReturnType<'tree>>, RlType) {
        let ann = Ann::DirectSource { loc: node };
        let nominal_params = Self::parse_nominal_params_of(node);
        let formal_params = match node.field_child("parameters") {
            None => vec![ValueParameter::parse_single_arrow(scope, node.field_child("parameter").unwrap())],
            Some(parameters) => parameters
                .named_children(&mut node.walk())
                .map(|node| ValueParameter::parse_formal(scope, node, false))
                .collect::<Vec<_>>()
        };
        let return_type = node.field_child("nominal_return_type")
            .map(ReturnType::<ThinType<'tree>>::parse_annotation);
        let fn_type = ResolvedLazy::new(scope, ThinType::func(
            ann,
            nominal_params.iter().map(|param| param.clone()),
            formal_params.iter().find(|param| param.is_this_param)
                .map(|p| p.thin().collapse_contravariant())
                .unwrap_or_default(),
            formal_params.iter().filter(|param| !param.is_this_param && !param.is_rest_param)
                .map(|p| p.optional_thin().map(|t| t.collapse_contravariant())),
            formal_params.iter().find(|param| param.is_rest_param)
                .map(|p| p.thin().collapse_contravariant())
                .unwrap_or(ThinType::EMPTY_REST_ARG),
            return_type.as_ref().map(|return_type| return_type.clone())
                .unwrap_or_default()
        ));
        (nominal_params, formal_params, return_type, fn_type)
    }

    fn parse_nominal_params_of(node: TSNode<'tree>) -> Vec<TypeParam<'tree, ThinType<'tree>>> {
        node.field_child("type_parameters").map(|node| {
            node.named_children(&mut node.walk())
                .filter(|node| node.kind() == "nominal_type_parameter")
                .map(TypeParam::parse)
                .collect()
        }).unwrap_or_default()
    }

    pub fn set_custom_inferred_return_type(&self, return_type: RlReturnType<'tree>, ctx: &ResolveCtx<'_, 'tree>) {
        self.custom_inferred_fn_type.set(self.fn_type.clone().with_return_type(return_type, ctx)).expect("custom_infer_return_type called twice");
    }
}

impl_ast_type_binding!(RlTypeDecl name Local);
impl<'tree> Parseable<'tree> for ThinTypeDecl<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["nominal_type_declaration"]);
        node.mark();
        Self {
            ann: Ann::DirectSource { loc: node },
            name: TypeIdent::parse(node.named_child(0).unwrap()),
            type_params: node.field_child("nominal_type_parameters").map(|x| {
                x.named_children(&mut node.walk())
                    .map(TypeParam::<'tree, ThinType<'tree>>::parse)
                    .collect::<Vec<_>>()
            }).unwrap_or_default(),
            supertypes: parse_nominal_supertypes(node.field_child("nominal_supertypes")),
            typescript_supertype: node.field_child("type").map(TypescriptType::parse).into_iter().collect::<Vec<_>>(),
            guard: node.field_child("guard").map(NominalGuard::parse).into_iter().collect::<Vec<_>>(),
        }
    }
}

impl_ast_value_binding!(ValueImportSpecifier alias Imported);
impl<'tree> ValueImportSpecifier<'tree> {
    pub fn parse(
        import_path_idx: usize,
        node: TSNode<'tree>,
    ) -> Self {
        assert_kind!(node, ["import_specifier"]);
        let imported_name = ValueIdent::parse(node.named_child(0).unwrap());
        let alias = node.named_child(1).map_or_else(
            || imported_name.clone(),
            ValueIdent::parse
        );
        let idx = ScopeValueImportIdx {
            import_path_idx,
            imported_name,
        };
        Self {
            ann: Ann::DirectSource { loc: node },
            idx,
            alias,
        }
    }
}

impl_ast_type_binding!(TypeImportSpecifier alias Imported);
impl<'tree> TypeImportSpecifier<'tree> {
    pub fn parse(
        import_path_idx: usize,
        node: TSNode<'tree>,
    ) -> Self {
        assert_kind!(node, ["import_specifier"]);
        node.mark();
        let imported_name = TypeIdent::parse(node.named_child(0).unwrap());
        let alias = node.named_child(1).map_or_else(
            || imported_name.clone(),
            TypeIdent::parse
        );
        let idx = ScopeTypeImportIdx {
            import_path_idx,
            imported_name,
        };
        Self {
            ann: Ann::DirectSource { loc: node },
            idx,
            alias,
        }
    }
}

impl<'tree> Parseable<'tree> for ImportPathAst<'tree> {
    fn parse(node: TSNode<'tree>) -> Self {
        assert_kind!(node, ["string"]);
        Self {
            ann: Ann::DirectSource { loc: node },
            module_path: ImportPath::from(unquote(node.text()).expect("import path is not a well-formed string literal")),
        }
    }
}

impl<'tree> ImportStatement<'tree> {
    pub fn parse(
        import_path_idx: usize,
        node: TSNode<'tree>,
    ) -> Self {
        assert_kind!(node, ["import_statement"]);
        let path = ImportPathAst::parse(node.named_child(1).unwrap());
        let import_container = node.named_child(0).unwrap();
        let imported_values = import_container
            .named_children(&mut node.walk())
            .filter(|node| node.named_child(0).unwrap().kind() == "identifier")
            .map(|node| ValueImportSpecifier::parse(import_path_idx, node))
            .collect();
        let imported_types = import_container
            .named_children(&mut node.walk())
            .filter(|node| node.named_child(0).unwrap().kind() == "nominal_type_identifier")
            .map(|node| TypeImportSpecifier::parse(import_path_idx, node))
            .collect();
        if let Some(invalid_specifier) = import_container
            .named_children(&mut node.walk())
            .find(|node| node.named_child(0).unwrap().kind() != "identifier" && node.named_child(0).unwrap().kind() != "nominal_type_identifier") {
            panic!("import statement contains an invalid import specifier {:?} of kind {:?}", invalid_specifier, invalid_specifier.kind());
        }
        Self {
            ann: Ann::DirectSource { loc: node },
            path,
            imported_values,
            imported_types,
            import_path_idx,
        }
    }
}

fn parse_nominal_supertypes(node: Option<TSNode<'_>>) -> Vec<ThinType<'_>> {
    node.map(|x| {
        x.mark();
        x.named_children(&mut x.walk())
            .map(ThinType::parse)
            .collect::<Vec<_>>()
    }).unwrap_or_default()
}