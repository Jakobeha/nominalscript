#[allow(non_upper_case_globals)]
static __Tags__: type_sitter_lib::gen_internal::TypedQueryOnceBox<tree_sitter::Query> =
    type_sitter_lib::gen_internal::TypedQueryOnceBox::new();
#[allow(non_snake_case)]
fn __Mk__Tags() -> Box<tree_sitter::Query> {
    # [allow (unused_mut)] let mut query = tree_sitter :: Query :: new (tree_sitter_nominalscript :: language_nominalscript () , "(function_signature\n  name: (identifier) @name) @definition.function\n\n(method_signature\n  name: (property_identifier) @name) @definition.method\n\n(abstract_method_signature\n  name: (property_identifier) @name) @definition.method\n\n(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type\n\n(module\n  name: (identifier) @name) @definition.module\n\n(interface_declaration\n  name: (type_identifier) @name) @definition.interface\n\n(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type\n\n(new_expression\n  constructor: (identifier) @name) @reference.class\n") . expect ("query parsed at compile-time but failed at runtime. Is the language 'tree_sitter_nominalscript' correct, and did you use the same tree-sitter / tree_sitter_nominalscript version?") ;
    Box::new(query)
}
#[doc = "Typed version of the query:\n\n```sexp\n(function_signature\n  name: (identifier) @name) @definition.function\n\n(method_signature\n  name: (property_identifier) @name) @definition.method\n\n(abstract_method_signature\n  name: (property_identifier) @name) @definition.method\n\n(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type\n\n(module\n  name: (identifier) @name) @definition.module\n\n(interface_declaration\n  name: (type_identifier) @name) @definition.interface\n\n(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type\n\n(new_expression\n  constructor: (identifier) @name) @reference.class\n\n```"]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub struct Tags;
#[doc = "Matches returned by a query cursor running the query [Tags]:\n\n```sexp\n(function_signature\n  name: (identifier) @name) @definition.function\n\n(method_signature\n  name: (property_identifier) @name) @definition.method\n\n(abstract_method_signature\n  name: (property_identifier) @name) @definition.method\n\n(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type\n\n(module\n  name: (identifier) @name) @definition.module\n\n(interface_declaration\n  name: (type_identifier) @name) @definition.interface\n\n(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type\n\n(new_expression\n  constructor: (identifier) @name) @reference.class\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type TagsMatches<'cursor, 'tree> = type_sitter_lib::TypedQueryMatches<'cursor, 'tree, Tags>;
#[doc = "Captures returned by a query cursor running the query [Tags]:\n\n```sexp\n(function_signature\n  name: (identifier) @name) @definition.function\n\n(method_signature\n  name: (property_identifier) @name) @definition.method\n\n(abstract_method_signature\n  name: (property_identifier) @name) @definition.method\n\n(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type\n\n(module\n  name: (identifier) @name) @definition.module\n\n(interface_declaration\n  name: (type_identifier) @name) @definition.interface\n\n(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type\n\n(new_expression\n  constructor: (identifier) @name) @reference.class\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type TagsCaptures<'cursor, 'tree> = type_sitter_lib::TypedQueryCaptures<'cursor, 'tree, Tags>;
#[doc = "A match returned by the query [Tags]:\n\n```sexp\n(function_signature\n  name: (identifier) @name) @definition.function\n\n(method_signature\n  name: (property_identifier) @name) @definition.method\n\n(abstract_method_signature\n  name: (property_identifier) @name) @definition.method\n\n(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type\n\n(module\n  name: (identifier) @name) @definition.module\n\n(interface_declaration\n  name: (type_identifier) @name) @definition.interface\n\n(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type\n\n(new_expression\n  constructor: (identifier) @name) @reference.class\n\n```"]
pub struct TagsMatch<'cursor, 'tree> {
    match_: tree_sitter::QueryMatch<'cursor, 'tree>,
    tree: &'tree yak_sitter::Tree,
}
#[doc = "A capture returned by the query [Tags]:\n\n```sexp\n(function_signature\n  name: (identifier) @name) @definition.function\n\n(method_signature\n  name: (property_identifier) @name) @definition.method\n\n(abstract_method_signature\n  name: (property_identifier) @name) @definition.method\n\n(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type\n\n(module\n  name: (identifier) @name) @definition.module\n\n(interface_declaration\n  name: (type_identifier) @name) @definition.interface\n\n(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type\n\n(new_expression\n  constructor: (identifier) @name) @reference.class\n\n```"]
pub enum TagsCapture<'cursor, 'tree> {
    #[doc = "A `name` ([anon_unions::Name])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(identifier) @name"]
    #[doc = "(property_identifier) @name"]
    #[doc = "(property_identifier) @name"]
    #[doc = "(nominal_type_identifier) @name"]
    #[doc = "(identifier) @name"]
    #[doc = "(type_identifier) @name"]
    #[doc = "(nominal_type_identifier) @name"]
    #[doc = "(identifier) @name"]
    #[doc = "```"]
    Name {
        node: anon_unions::Name<'tree>,
        match_: Option<TagsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `definition.function` ([crate::syntax::nodes::FunctionSignature])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(function_signature\n  name: (identifier) @name) @definition.function"]
    #[doc = "```"]
    DefinitionFunction {
        node: crate::syntax::nodes::FunctionSignature<'tree>,
        match_: Option<TagsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `definition.method` ([anon_unions::DefinitionMethod])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(method_signature\n  name: (property_identifier) @name) @definition.method"]
    #[doc = "(abstract_method_signature\n  name: (property_identifier) @name) @definition.method"]
    #[doc = "```"]
    DefinitionMethod {
        node: anon_unions::DefinitionMethod<'tree>,
        match_: Option<TagsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `definition.type` ([crate::syntax::nodes::NominalTypeDeclaration])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type"]
    #[doc = "```"]
    DefinitionType {
        node: crate::syntax::nodes::NominalTypeDeclaration<'tree>,
        match_: Option<TagsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `definition.module` ([crate::syntax::nodes::Module])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(module\n  name: (identifier) @name) @definition.module"]
    #[doc = "```"]
    DefinitionModule {
        node: crate::syntax::nodes::Module<'tree>,
        match_: Option<TagsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `definition.interface` ([crate::syntax::nodes::InterfaceDeclaration])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(interface_declaration\n  name: (type_identifier) @name) @definition.interface"]
    #[doc = "```"]
    DefinitionInterface {
        node: crate::syntax::nodes::InterfaceDeclaration<'tree>,
        match_: Option<TagsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `reference.type` ([crate::syntax::nodes::NominalTypeAnnotation])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type"]
    #[doc = "```"]
    ReferenceType {
        node: crate::syntax::nodes::NominalTypeAnnotation<'tree>,
        match_: Option<TagsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `reference.class` ([crate::syntax::nodes::NewExpression])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(new_expression\n  constructor: (identifier) @name) @reference.class"]
    #[doc = "```"]
    ReferenceClass {
        node: crate::syntax::nodes::NewExpression<'tree>,
        match_: Option<TagsMatch<'cursor, 'tree>>,
    },
}
#[automatically_derived]
impl type_sitter_lib::TypedQuery for Tags {
    type Match<'cursor, 'tree: 'cursor> = TagsMatch<'cursor, 'tree>;
    type Capture<'cursor, 'tree: 'cursor> = TagsCapture<'cursor, 'tree>;
    fn query_str(&self) -> &'static str {
        "(function_signature\n  name: (identifier) @name) @definition.function\n\n(method_signature\n  name: (property_identifier) @name) @definition.method\n\n(abstract_method_signature\n  name: (property_identifier) @name) @definition.method\n\n(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type\n\n(module\n  name: (identifier) @name) @definition.module\n\n(interface_declaration\n  name: (type_identifier) @name) @definition.interface\n\n(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type\n\n(new_expression\n  constructor: (identifier) @name) @reference.class\n"
    }
    fn query(&self) -> &'static tree_sitter::Query {
        __Tags__.get_or_init(__Mk__Tags)
    }
    #[inline]
    unsafe fn wrap_match<'cursor, 'tree>(
        &self,
        match_: tree_sitter::QueryMatch<'cursor, 'tree>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Match<'cursor, 'tree> {
        Self::Match { match_, tree }
    }
    #[inline]
    unsafe fn wrap_capture<'cursor, 'tree>(
        &self,
        capture: tree_sitter::QueryCapture<'tree>,
        match_: Option<Self::Match<'cursor, 'tree>>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Capture<'cursor, 'tree> {
        match capture . index as usize { 0usize => Self :: Capture :: Name { node : < anon_unions :: Name < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 1usize => Self :: Capture :: DefinitionFunction { node : < crate :: syntax :: nodes :: FunctionSignature < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 2usize => Self :: Capture :: DefinitionMethod { node : < anon_unions :: DefinitionMethod < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 3usize => Self :: Capture :: DefinitionType { node : < crate :: syntax :: nodes :: NominalTypeDeclaration < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 4usize => Self :: Capture :: DefinitionModule { node : < crate :: syntax :: nodes :: Module < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 5usize => Self :: Capture :: DefinitionInterface { node : < crate :: syntax :: nodes :: InterfaceDeclaration < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 6usize => Self :: Capture :: ReferenceType { node : < crate :: syntax :: nodes :: NominalTypeAnnotation < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 7usize => Self :: Capture :: ReferenceClass { node : < crate :: syntax :: nodes :: NewExpression < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , capture_index => unreachable ! ("Invalid capture index: {}" , capture_index) }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> TagsMatch<'cursor, 'tree> {
    #[doc = "Returns an iterator over the nodes captured by `name` ([anon_unions::Name])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(identifier) @name"]
    #[doc = "(property_identifier) @name"]
    #[doc = "(property_identifier) @name"]
    #[doc = "(nominal_type_identifier) @name"]
    #[doc = "(identifier) @name"]
    #[doc = "(type_identifier) @name"]
    #[doc = "(nominal_type_identifier) @name"]
    #[doc = "(identifier) @name"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn name(&self) -> Option<anon_unions::Name<'tree>> {
        { [0u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < anon_unions :: Name < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `definition.function` ([crate::syntax::nodes::FunctionSignature])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(function_signature\n  name: (identifier) @name) @definition.function"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_function(&self) -> Option<crate::syntax::nodes::FunctionSignature<'tree>> {
        { [1u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: FunctionSignature < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `definition.method` ([anon_unions::DefinitionMethod])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(method_signature\n  name: (property_identifier) @name) @definition.method"]
    #[doc = "(abstract_method_signature\n  name: (property_identifier) @name) @definition.method"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_method(&self) -> Option<anon_unions::DefinitionMethod<'tree>> {
        { [2u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < anon_unions :: DefinitionMethod < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `definition.type` ([crate::syntax::nodes::NominalTypeDeclaration])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_type(&self) -> Option<crate::syntax::nodes::NominalTypeDeclaration<'tree>> {
        {
            [3u32]
                .into_iter()
                .flat_map(|i| self.match_.nodes_for_capture_index(i))
                .map(|n| unsafe {
                    <crate::syntax::nodes::NominalTypeDeclaration<'tree> as type_sitter_lib::TypedNode<
                        'tree,
                    >>::from_node_unchecked(yak_sitter::Node::new(n, self.tree))
                })
        }
        .next()
    }
    #[doc = "Returns an iterator over the nodes captured by `definition.module` ([crate::syntax::nodes::Module])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(module\n  name: (identifier) @name) @definition.module"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_module(&self) -> Option<crate::syntax::nodes::Module<'tree>> {
        { [4u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: Module < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `definition.interface` ([crate::syntax::nodes::InterfaceDeclaration])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(interface_declaration\n  name: (type_identifier) @name) @definition.interface"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_interface(&self) -> Option<crate::syntax::nodes::InterfaceDeclaration<'tree>> {
        {
            [5u32]
                .into_iter()
                .flat_map(|i| self.match_.nodes_for_capture_index(i))
                .map(|n| unsafe {
                    <crate::syntax::nodes::InterfaceDeclaration<'tree> as type_sitter_lib::TypedNode<
                        'tree,
                    >>::from_node_unchecked(yak_sitter::Node::new(n, self.tree))
                })
        }
        .next()
    }
    #[doc = "Returns an iterator over the nodes captured by `reference.type` ([crate::syntax::nodes::NominalTypeAnnotation])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn reference_type(&self) -> Option<crate::syntax::nodes::NominalTypeAnnotation<'tree>> {
        {
            [6u32]
                .into_iter()
                .flat_map(|i| self.match_.nodes_for_capture_index(i))
                .map(|n| unsafe {
                    <crate::syntax::nodes::NominalTypeAnnotation<'tree> as type_sitter_lib::TypedNode<
                        'tree,
                    >>::from_node_unchecked(yak_sitter::Node::new(n, self.tree))
                })
        }
        .next()
    }
    #[doc = "Returns an iterator over the nodes captured by `reference.class` ([crate::syntax::nodes::NewExpression])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(new_expression\n  constructor: (identifier) @name) @reference.class"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn reference_class(&self) -> Option<crate::syntax::nodes::NewExpression<'tree>> {
        { [7u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: NewExpression < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
}
#[automatically_derived]
impl<'cursor, 'tree> std::fmt::Debug for TagsMatch<'cursor, 'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(TagsMatch))
            .field("match_", &self.match_)
            .finish()
    }
}
#[automatically_derived]
impl<'cursor, 'tree> type_sitter_lib::TypedQueryMatch<'cursor, 'tree>
    for TagsMatch<'cursor, 'tree>
{
    type Query = Tags;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Tags
    }
    #[inline]
    fn tree(&self) -> &'tree yak_sitter::Tree {
        self.tree
    }
    #[inline]
    fn raw(&self) -> &tree_sitter::QueryMatch<'cursor, 'tree> {
        &self.match_
    }
    #[inline]
    fn into_raw(self) -> tree_sitter::QueryMatch<'cursor, 'tree> {
        self.match_
    }
}
#[automatically_derived]
impl<'cursor, 'tree> TagsCapture<'cursor, 'tree> {
    #[doc = "Try to interpret this capture as a `name` ([anon_unions::Name])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(identifier) @name"]
    #[doc = "(property_identifier) @name"]
    #[doc = "(property_identifier) @name"]
    #[doc = "(nominal_type_identifier) @name"]
    #[doc = "(identifier) @name"]
    #[doc = "(type_identifier) @name"]
    #[doc = "(nominal_type_identifier) @name"]
    #[doc = "(identifier) @name"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn name(&self) -> Option<&anon_unions::Name<'tree>> {
        match self {
            Self::Name { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `definition.function` ([crate::syntax::nodes::FunctionSignature])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(function_signature\n  name: (identifier) @name) @definition.function"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_function(&self) -> Option<&crate::syntax::nodes::FunctionSignature<'tree>> {
        match self {
            Self::DefinitionFunction { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `definition.method` ([anon_unions::DefinitionMethod])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(method_signature\n  name: (property_identifier) @name) @definition.method"]
    #[doc = "(abstract_method_signature\n  name: (property_identifier) @name) @definition.method"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_method(&self) -> Option<&anon_unions::DefinitionMethod<'tree>> {
        match self {
            Self::DefinitionMethod { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `definition.type` ([crate::syntax::nodes::NominalTypeDeclaration])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(nominal_type_declaration\n  name: (nominal_type_identifier) @name) @definition.type"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_type(&self) -> Option<&crate::syntax::nodes::NominalTypeDeclaration<'tree>> {
        match self {
            Self::DefinitionType { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `definition.module` ([crate::syntax::nodes::Module])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(module\n  name: (identifier) @name) @definition.module"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_module(&self) -> Option<&crate::syntax::nodes::Module<'tree>> {
        match self {
            Self::DefinitionModule { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `definition.interface` ([crate::syntax::nodes::InterfaceDeclaration])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(interface_declaration\n  name: (type_identifier) @name) @definition.interface"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn definition_interface(&self) -> Option<&crate::syntax::nodes::InterfaceDeclaration<'tree>> {
        match self {
            Self::DefinitionInterface { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `reference.type` ([crate::syntax::nodes::NominalTypeAnnotation])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(nominal_type_annotation\n  (nominal_type_identifier) @name) @reference.type"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn reference_type(&self) -> Option<&crate::syntax::nodes::NominalTypeAnnotation<'tree>> {
        match self {
            Self::ReferenceType { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `reference.class` ([crate::syntax::nodes::NewExpression])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(new_expression\n  constructor: (identifier) @name) @reference.class"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn reference_class(&self) -> Option<&crate::syntax::nodes::NewExpression<'tree>> {
        match self {
            Self::ReferenceClass { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> std::fmt::Debug for TagsCapture<'cursor, 'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Name { node, .. } => f
                .debug_struct(concat!(stringify!(TagsCapture), "::", stringify!(Name)))
                .field("node", node)
                .finish(),
            Self::DefinitionFunction { node, .. } => f
                .debug_struct(concat!(
                    stringify!(TagsCapture),
                    "::",
                    stringify!(DefinitionFunction)
                ))
                .field("node", node)
                .finish(),
            Self::DefinitionMethod { node, .. } => f
                .debug_struct(concat!(
                    stringify!(TagsCapture),
                    "::",
                    stringify!(DefinitionMethod)
                ))
                .field("node", node)
                .finish(),
            Self::DefinitionType { node, .. } => f
                .debug_struct(concat!(
                    stringify!(TagsCapture),
                    "::",
                    stringify!(DefinitionType)
                ))
                .field("node", node)
                .finish(),
            Self::DefinitionModule { node, .. } => f
                .debug_struct(concat!(
                    stringify!(TagsCapture),
                    "::",
                    stringify!(DefinitionModule)
                ))
                .field("node", node)
                .finish(),
            Self::DefinitionInterface { node, .. } => f
                .debug_struct(concat!(
                    stringify!(TagsCapture),
                    "::",
                    stringify!(DefinitionInterface)
                ))
                .field("node", node)
                .finish(),
            Self::ReferenceType { node, .. } => f
                .debug_struct(concat!(
                    stringify!(TagsCapture),
                    "::",
                    stringify!(ReferenceType)
                ))
                .field("node", node)
                .finish(),
            Self::ReferenceClass { node, .. } => f
                .debug_struct(concat!(
                    stringify!(TagsCapture),
                    "::",
                    stringify!(ReferenceClass)
                ))
                .field("node", node)
                .finish(),
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> Clone for TagsCapture<'cursor, 'tree> {
    fn clone(&self) -> Self {
        match self {
            Self::Name { node, .. } => Self::Name {
                node: *node,
                match_: None,
            },
            Self::DefinitionFunction { node, .. } => Self::DefinitionFunction {
                node: *node,
                match_: None,
            },
            Self::DefinitionMethod { node, .. } => Self::DefinitionMethod {
                node: *node,
                match_: None,
            },
            Self::DefinitionType { node, .. } => Self::DefinitionType {
                node: *node,
                match_: None,
            },
            Self::DefinitionModule { node, .. } => Self::DefinitionModule {
                node: *node,
                match_: None,
            },
            Self::DefinitionInterface { node, .. } => Self::DefinitionInterface {
                node: *node,
                match_: None,
            },
            Self::ReferenceType { node, .. } => Self::ReferenceType {
                node: *node,
                match_: None,
            },
            Self::ReferenceClass { node, .. } => Self::ReferenceClass {
                node: *node,
                match_: None,
            },
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> type_sitter_lib::TypedQueryCapture<'cursor, 'tree>
    for TagsCapture<'cursor, 'tree>
{
    type Query = Tags;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Tags
    }
    #[inline]
    fn match_(
        &self,
    ) -> Option<&<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {
            Self::Name { match_, .. } => match_.as_ref(),
            Self::DefinitionFunction { match_, .. } => match_.as_ref(),
            Self::DefinitionMethod { match_, .. } => match_.as_ref(),
            Self::DefinitionType { match_, .. } => match_.as_ref(),
            Self::DefinitionModule { match_, .. } => match_.as_ref(),
            Self::DefinitionInterface { match_, .. } => match_.as_ref(),
            Self::ReferenceType { match_, .. } => match_.as_ref(),
            Self::ReferenceClass { match_, .. } => match_.as_ref(),
        }
    }
    #[inline]
    fn into_match(
        self,
    ) -> Option<<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {
            Self::Name { match_, .. } => match_,
            Self::DefinitionFunction { match_, .. } => match_,
            Self::DefinitionMethod { match_, .. } => match_,
            Self::DefinitionType { match_, .. } => match_,
            Self::DefinitionModule { match_, .. } => match_,
            Self::DefinitionInterface { match_, .. } => match_,
            Self::ReferenceType { match_, .. } => match_,
            Self::ReferenceClass { match_, .. } => match_,
        }
    }
    #[inline]
    fn to_raw(&self) -> yak_sitter::QueryCapture<'static, 'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {
            Self::Name { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 0usize,
                name: "name",
            },
            Self::DefinitionFunction { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 1usize,
                name: "definition.function",
            },
            Self::DefinitionMethod { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 2usize,
                name: "definition.method",
            },
            Self::DefinitionType { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 3usize,
                name: "definition.type",
            },
            Self::DefinitionModule { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 4usize,
                name: "definition.module",
            },
            Self::DefinitionInterface { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 5usize,
                name: "definition.interface",
            },
            Self::ReferenceType { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 6usize,
                name: "reference.type",
            },
            Self::ReferenceClass { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 7usize,
                name: "reference.class",
            },
        }
    }
    #[inline]
    fn node(&self) -> &yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {
            Self::Name { node, .. } => node.node(),
            Self::DefinitionFunction { node, .. } => node.node(),
            Self::DefinitionMethod { node, .. } => node.node(),
            Self::DefinitionType { node, .. } => node.node(),
            Self::DefinitionModule { node, .. } => node.node(),
            Self::DefinitionInterface { node, .. } => node.node(),
            Self::ReferenceType { node, .. } => node.node(),
            Self::ReferenceClass { node, .. } => node.node(),
        }
    }
    #[inline]
    fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {
            Self::Name { node, .. } => node.node_mut(),
            Self::DefinitionFunction { node, .. } => node.node_mut(),
            Self::DefinitionMethod { node, .. } => node.node_mut(),
            Self::DefinitionType { node, .. } => node.node_mut(),
            Self::DefinitionModule { node, .. } => node.node_mut(),
            Self::DefinitionInterface { node, .. } => node.node_mut(),
            Self::ReferenceType { node, .. } => node.node_mut(),
            Self::ReferenceClass { node, .. } => node.node_mut(),
        }
    }
    #[inline]
    fn name(&self) -> &'static str {
        match self {
            Self::Name { .. } => "name",
            Self::DefinitionFunction { .. } => "definition.function",
            Self::DefinitionMethod { .. } => "definition.method",
            Self::DefinitionType { .. } => "definition.type",
            Self::DefinitionModule { .. } => "definition.module",
            Self::DefinitionInterface { .. } => "definition.interface",
            Self::ReferenceType { .. } => "reference.type",
            Self::ReferenceClass { .. } => "reference.class",
        }
    }
    #[inline]
    fn index(&self) -> usize {
        match self {
            Self::Name { .. } => 0usize,
            Self::DefinitionFunction { .. } => 1usize,
            Self::DefinitionMethod { .. } => 2usize,
            Self::DefinitionType { .. } => 3usize,
            Self::DefinitionModule { .. } => 4usize,
            Self::DefinitionInterface { .. } => 5usize,
            Self::ReferenceType { .. } => 6usize,
            Self::ReferenceClass { .. } => 7usize,
        }
    }
}
#[allow(non_upper_case_globals)]
static __Locals__: type_sitter_lib::gen_internal::TypedQueryOnceBox<tree_sitter::Query> =
    type_sitter_lib::gen_internal::TypedQueryOnceBox::new();
#[allow(non_snake_case)]
fn __Mk__Locals() -> Box<tree_sitter::Query> {
    # [allow (unused_mut)] let mut query = tree_sitter :: Query :: new (tree_sitter_nominalscript :: language_nominalscript () , "; inherits: typescript\n") . expect ("query parsed at compile-time but failed at runtime. Is the language 'tree_sitter_nominalscript' correct, and did you use the same tree-sitter / tree_sitter_nominalscript version?") ;
    Box::new(query)
}
#[doc = "Typed version of the query:\n\n```sexp\n; inherits: typescript\n\n```"]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub struct Locals;
#[doc = "Matches returned by a query cursor running the query [Locals]:\n\n```sexp\n; inherits: typescript\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type LocalsMatches<'cursor, 'tree> = type_sitter_lib::TypedQueryMatches<'cursor, 'tree, Locals>;
#[doc = "Captures returned by a query cursor running the query [Locals]:\n\n```sexp\n; inherits: typescript\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type LocalsCaptures<'cursor, 'tree> =
    type_sitter_lib::TypedQueryCaptures<'cursor, 'tree, Locals>;
#[doc = "A match returned by the query [Locals]:\n\n```sexp\n; inherits: typescript\n\n```"]
pub struct LocalsMatch<'cursor, 'tree> {
    match_: tree_sitter::QueryMatch<'cursor, 'tree>,
    tree: &'tree yak_sitter::Tree,
}
#[doc = "A capture returned by the query [Locals]:\n\n```sexp\n; inherits: typescript\n\n```"]
pub enum LocalsCapture {}
#[automatically_derived]
impl type_sitter_lib::TypedQuery for Locals {
    type Match<'cursor, 'tree: 'cursor> = LocalsMatch<'cursor, 'tree>;
    type Capture<'cursor, 'tree: 'cursor> = LocalsCapture;
    fn query_str(&self) -> &'static str {
        "; inherits: typescript\n"
    }
    fn query(&self) -> &'static tree_sitter::Query {
        __Locals__.get_or_init(__Mk__Locals)
    }
    #[inline]
    unsafe fn wrap_match<'cursor, 'tree>(
        &self,
        match_: tree_sitter::QueryMatch<'cursor, 'tree>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Match<'cursor, 'tree> {
        Self::Match { match_, tree }
    }
    #[inline]
    unsafe fn wrap_capture<'cursor, 'tree>(
        &self,
        capture: tree_sitter::QueryCapture<'tree>,
        match_: Option<Self::Match<'cursor, 'tree>>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Capture<'cursor, 'tree> {
        match capture.index as usize {
            capture_index => unreachable!("Invalid capture index: {}", capture_index),
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> LocalsMatch<'cursor, 'tree> {}
#[automatically_derived]
impl<'cursor, 'tree> std::fmt::Debug for LocalsMatch<'cursor, 'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(LocalsMatch))
            .field("match_", &self.match_)
            .finish()
    }
}
#[automatically_derived]
impl<'cursor, 'tree> type_sitter_lib::TypedQueryMatch<'cursor, 'tree>
    for LocalsMatch<'cursor, 'tree>
{
    type Query = Locals;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Locals
    }
    #[inline]
    fn tree(&self) -> &'tree yak_sitter::Tree {
        self.tree
    }
    #[inline]
    fn raw(&self) -> &tree_sitter::QueryMatch<'cursor, 'tree> {
        &self.match_
    }
    #[inline]
    fn into_raw(self) -> tree_sitter::QueryMatch<'cursor, 'tree> {
        self.match_
    }
}
#[automatically_derived]
impl LocalsCapture {}
#[automatically_derived]
impl std::fmt::Debug for LocalsCapture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {}
    }
}
#[automatically_derived]
impl Clone for LocalsCapture {
    fn clone(&self) -> Self {
        match self {}
    }
}
#[automatically_derived]
impl<'cursor, 'tree: 'cursor> type_sitter_lib::TypedQueryCapture<'cursor, 'tree>
    for LocalsCapture
{
    type Query = Locals;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Locals
    }
    #[inline]
    fn match_(
        &self,
    ) -> Option<&<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {}
    }
    #[inline]
    fn into_match(
        self,
    ) -> Option<<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {}
    }
    #[inline]
    fn to_raw(&self) -> yak_sitter::QueryCapture<'static, 'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {}
    }
    #[inline]
    fn node(&self) -> &yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {}
    }
    #[inline]
    fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {}
    }
    #[inline]
    fn name(&self) -> &'static str {
        match self {}
    }
    #[inline]
    fn index(&self) -> usize {
        match self {}
    }
}
#[allow(non_upper_case_globals)]
static __Indents__: type_sitter_lib::gen_internal::TypedQueryOnceBox<tree_sitter::Query> =
    type_sitter_lib::gen_internal::TypedQueryOnceBox::new();
#[allow(non_snake_case)]
fn __Mk__Indents() -> Box<tree_sitter::Query> {
    # [allow (unused_mut)] let mut query = tree_sitter :: Query :: new (tree_sitter_nominalscript :: language_nominalscript () , "; inherits: typescript\n[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin\n") . expect ("query parsed at compile-time but failed at runtime. Is the language 'tree_sitter_nominalscript' correct, and did you use the same tree-sitter / tree_sitter_nominalscript version?") ;
    Box::new(query)
}
#[doc = "Typed version of the query:\n\n```sexp\n; inherits: typescript\n[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin\n\n```"]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub struct Indents;
#[doc = "Matches returned by a query cursor running the query [Indents]:\n\n```sexp\n; inherits: typescript\n[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type IndentsMatches<'cursor, 'tree> =
    type_sitter_lib::TypedQueryMatches<'cursor, 'tree, Indents>;
#[doc = "Captures returned by a query cursor running the query [Indents]:\n\n```sexp\n; inherits: typescript\n[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type IndentsCaptures<'cursor, 'tree> =
    type_sitter_lib::TypedQueryCaptures<'cursor, 'tree, Indents>;
#[doc = "A match returned by the query [Indents]:\n\n```sexp\n; inherits: typescript\n[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin\n\n```"]
pub struct IndentsMatch<'cursor, 'tree> {
    match_: tree_sitter::QueryMatch<'cursor, 'tree>,
    tree: &'tree yak_sitter::Tree,
}
#[doc = "A capture returned by the query [Indents]:\n\n```sexp\n; inherits: typescript\n[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin\n\n```"]
pub enum IndentsCapture<'cursor, 'tree> {
    #[doc = "A `indent.begin` ([anon_unions::IndentBegin])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin"]
    #[doc = "```"]
    IndentBegin {
        node: anon_unions::IndentBegin<'tree>,
        match_: Option<IndentsMatch<'cursor, 'tree>>,
    },
}
#[automatically_derived]
impl type_sitter_lib::TypedQuery for Indents {
    type Match<'cursor, 'tree: 'cursor> = IndentsMatch<'cursor, 'tree>;
    type Capture<'cursor, 'tree: 'cursor> = IndentsCapture<'cursor, 'tree>;
    fn query_str(&self) -> &'static str {
        "; inherits: typescript\n[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin\n"
    }
    fn query(&self) -> &'static tree_sitter::Query {
        __Indents__.get_or_init(__Mk__Indents)
    }
    #[inline]
    unsafe fn wrap_match<'cursor, 'tree>(
        &self,
        match_: tree_sitter::QueryMatch<'cursor, 'tree>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Match<'cursor, 'tree> {
        Self::Match { match_, tree }
    }
    #[inline]
    unsafe fn wrap_capture<'cursor, 'tree>(
        &self,
        capture: tree_sitter::QueryCapture<'tree>,
        match_: Option<Self::Match<'cursor, 'tree>>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Capture<'cursor, 'tree> {
        match capture . index as usize { 0usize => Self :: Capture :: IndentBegin { node : < anon_unions :: IndentBegin < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , capture_index => unreachable ! ("Invalid capture index: {}" , capture_index) }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> IndentsMatch<'cursor, 'tree> {
    #[doc = "Returns an iterator over the nodes captured by `indent.begin` ([anon_unions::IndentBegin])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn indent_begin(&self) -> anon_unions::IndentBegin<'tree> {
        let result = { [0u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < anon_unions :: IndentBegin < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next () . expect ("one quantifier returned nothing") ;
        debug_assert ! ({ [0u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < anon_unions :: IndentBegin < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next () . is_none () , "one quantifier returned more than one item");
        result
    }
}
#[automatically_derived]
impl<'cursor, 'tree> std::fmt::Debug for IndentsMatch<'cursor, 'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(IndentsMatch))
            .field("match_", &self.match_)
            .finish()
    }
}
#[automatically_derived]
impl<'cursor, 'tree> type_sitter_lib::TypedQueryMatch<'cursor, 'tree>
    for IndentsMatch<'cursor, 'tree>
{
    type Query = Indents;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Indents
    }
    #[inline]
    fn tree(&self) -> &'tree yak_sitter::Tree {
        self.tree
    }
    #[inline]
    fn raw(&self) -> &tree_sitter::QueryMatch<'cursor, 'tree> {
        &self.match_
    }
    #[inline]
    fn into_raw(self) -> tree_sitter::QueryMatch<'cursor, 'tree> {
        self.match_
    }
}
#[automatically_derived]
impl<'cursor, 'tree> IndentsCapture<'cursor, 'tree> {
    #[doc = "Try to interpret this capture as a `indent.begin` ([anon_unions::IndentBegin])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "[\n (nominal_type_declaration)\n (nominal_type_guard)\n] @indent.begin"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn indent_begin(&self) -> Option<&anon_unions::IndentBegin<'tree>> {
        match self {
            Self::IndentBegin { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> std::fmt::Debug for IndentsCapture<'cursor, 'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IndentBegin { node, .. } => f
                .debug_struct(concat!(
                    stringify!(IndentsCapture),
                    "::",
                    stringify!(IndentBegin)
                ))
                .field("node", node)
                .finish(),
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> Clone for IndentsCapture<'cursor, 'tree> {
    fn clone(&self) -> Self {
        match self {
            Self::IndentBegin { node, .. } => Self::IndentBegin {
                node: *node,
                match_: None,
            },
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> type_sitter_lib::TypedQueryCapture<'cursor, 'tree>
    for IndentsCapture<'cursor, 'tree>
{
    type Query = Indents;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Indents
    }
    #[inline]
    fn match_(
        &self,
    ) -> Option<&<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {
            Self::IndentBegin { match_, .. } => match_.as_ref(),
        }
    }
    #[inline]
    fn into_match(
        self,
    ) -> Option<<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {
            Self::IndentBegin { match_, .. } => match_,
        }
    }
    #[inline]
    fn to_raw(&self) -> yak_sitter::QueryCapture<'static, 'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {
            Self::IndentBegin { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 0usize,
                name: "indent.begin",
            },
        }
    }
    #[inline]
    fn node(&self) -> &yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {
            Self::IndentBegin { node, .. } => node.node(),
        }
    }
    #[inline]
    fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {
            Self::IndentBegin { node, .. } => node.node_mut(),
        }
    }
    #[inline]
    fn name(&self) -> &'static str {
        match self {
            Self::IndentBegin { .. } => "indent.begin",
        }
    }
    #[inline]
    fn index(&self) -> usize {
        match self {
            Self::IndentBegin { .. } => 0usize,
        }
    }
}
#[allow(non_upper_case_globals)]
static __Highlights__: type_sitter_lib::gen_internal::TypedQueryOnceBox<tree_sitter::Query> =
    type_sitter_lib::gen_internal::TypedQueryOnceBox::new();
#[allow(non_snake_case)]
fn __Mk__Highlights() -> Box<tree_sitter::Query> {
    # [allow (unused_mut)] let mut query = tree_sitter :: Query :: new (tree_sitter_nominalscript :: language_nominalscript () , "; inherits typescript\n; Keywords\n\n\"guard\" @keyword\n\n; Punctuation\n\n(\"<\" . \";\") @operator\n\n\";\" @punctuation.delimiter\n(nullable_nominal_type \"?\" @punctuation.delimiter)\n(optional_nominal_type \"?\" @punctuation.special)\n(nominal_property_signature \"?\" @punctuation.special)\n(nominal_method_signature \"?\" @punctuation.special)\n\n; (nominal_wrap_expression \";\" @punctuation.special)\n; (nominal_wrap_unchecked_expression [\"!\" \";\"] @punctuation.special)\n\n; (nominal_type_annotation \";\" @punctuation.delimiter)\n\n; Types\n\n(nominal_type_identifier) @type\n\n(nominal_type_arguments\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_type_parameters\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_formal_parameters \"this\" @variable.builtin)\n\n; Parameters\n(nominal_type_guard\n  bound: (identifier) @parameter)\n\n; Method signatures\n(nominal_method_signature name: (_) @method)\n\n; Property signatures\n(property_signature\n  name: (property_identifier) @method\n  nominal_type: (nominal_type_annotation \n                  [(function_nominal_type)\n                   (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n(nominal_property_signature\n  name: (property_identifier) @method\n  type: (nominal_type_annotation \n          [(function_nominal_type)\n           (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n") . expect ("query parsed at compile-time but failed at runtime. Is the language 'tree_sitter_nominalscript' correct, and did you use the same tree-sitter / tree_sitter_nominalscript version?") ;
    Box::new(query)
}
#[doc = "Typed version of the query:\n\n```sexp\n; inherits typescript\n; Keywords\n\n\"guard\" @keyword\n\n; Punctuation\n\n(\"<\" . \";\") @operator\n\n\";\" @punctuation.delimiter\n(nullable_nominal_type \"?\" @punctuation.delimiter)\n(optional_nominal_type \"?\" @punctuation.special)\n(nominal_property_signature \"?\" @punctuation.special)\n(nominal_method_signature \"?\" @punctuation.special)\n\n; (nominal_wrap_expression \";\" @punctuation.special)\n; (nominal_wrap_unchecked_expression [\"!\" \";\"] @punctuation.special)\n\n; (nominal_type_annotation \";\" @punctuation.delimiter)\n\n; Types\n\n(nominal_type_identifier) @type\n\n(nominal_type_arguments\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_type_parameters\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_formal_parameters \"this\" @variable.builtin)\n\n; Parameters\n(nominal_type_guard\n  bound: (identifier) @parameter)\n\n; Method signatures\n(nominal_method_signature name: (_) @method)\n\n; Property signatures\n(property_signature\n  name: (property_identifier) @method\n  nominal_type: (nominal_type_annotation \n                  [(function_nominal_type)\n                   (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n(nominal_property_signature\n  name: (property_identifier) @method\n  type: (nominal_type_annotation \n          [(function_nominal_type)\n           (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n```"]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub struct Highlights;
#[doc = "Matches returned by a query cursor running the query [Highlights]:\n\n```sexp\n; inherits typescript\n; Keywords\n\n\"guard\" @keyword\n\n; Punctuation\n\n(\"<\" . \";\") @operator\n\n\";\" @punctuation.delimiter\n(nullable_nominal_type \"?\" @punctuation.delimiter)\n(optional_nominal_type \"?\" @punctuation.special)\n(nominal_property_signature \"?\" @punctuation.special)\n(nominal_method_signature \"?\" @punctuation.special)\n\n; (nominal_wrap_expression \";\" @punctuation.special)\n; (nominal_wrap_unchecked_expression [\"!\" \";\"] @punctuation.special)\n\n; (nominal_type_annotation \";\" @punctuation.delimiter)\n\n; Types\n\n(nominal_type_identifier) @type\n\n(nominal_type_arguments\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_type_parameters\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_formal_parameters \"this\" @variable.builtin)\n\n; Parameters\n(nominal_type_guard\n  bound: (identifier) @parameter)\n\n; Method signatures\n(nominal_method_signature name: (_) @method)\n\n; Property signatures\n(property_signature\n  name: (property_identifier) @method\n  nominal_type: (nominal_type_annotation \n                  [(function_nominal_type)\n                   (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n(nominal_property_signature\n  name: (property_identifier) @method\n  type: (nominal_type_annotation \n          [(function_nominal_type)\n           (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type HighlightsMatches<'cursor, 'tree> =
    type_sitter_lib::TypedQueryMatches<'cursor, 'tree, Highlights>;
#[doc = "Captures returned by a query cursor running the query [Highlights]:\n\n```sexp\n; inherits typescript\n; Keywords\n\n\"guard\" @keyword\n\n; Punctuation\n\n(\"<\" . \";\") @operator\n\n\";\" @punctuation.delimiter\n(nullable_nominal_type \"?\" @punctuation.delimiter)\n(optional_nominal_type \"?\" @punctuation.special)\n(nominal_property_signature \"?\" @punctuation.special)\n(nominal_method_signature \"?\" @punctuation.special)\n\n; (nominal_wrap_expression \";\" @punctuation.special)\n; (nominal_wrap_unchecked_expression [\"!\" \";\"] @punctuation.special)\n\n; (nominal_type_annotation \";\" @punctuation.delimiter)\n\n; Types\n\n(nominal_type_identifier) @type\n\n(nominal_type_arguments\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_type_parameters\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_formal_parameters \"this\" @variable.builtin)\n\n; Parameters\n(nominal_type_guard\n  bound: (identifier) @parameter)\n\n; Method signatures\n(nominal_method_signature name: (_) @method)\n\n; Property signatures\n(property_signature\n  name: (property_identifier) @method\n  nominal_type: (nominal_type_annotation \n                  [(function_nominal_type)\n                   (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n(nominal_property_signature\n  name: (property_identifier) @method\n  type: (nominal_type_annotation \n          [(function_nominal_type)\n           (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type HighlightsCaptures<'cursor, 'tree> =
    type_sitter_lib::TypedQueryCaptures<'cursor, 'tree, Highlights>;
#[doc = "A match returned by the query [Highlights]:\n\n```sexp\n; inherits typescript\n; Keywords\n\n\"guard\" @keyword\n\n; Punctuation\n\n(\"<\" . \";\") @operator\n\n\";\" @punctuation.delimiter\n(nullable_nominal_type \"?\" @punctuation.delimiter)\n(optional_nominal_type \"?\" @punctuation.special)\n(nominal_property_signature \"?\" @punctuation.special)\n(nominal_method_signature \"?\" @punctuation.special)\n\n; (nominal_wrap_expression \";\" @punctuation.special)\n; (nominal_wrap_unchecked_expression [\"!\" \";\"] @punctuation.special)\n\n; (nominal_type_annotation \";\" @punctuation.delimiter)\n\n; Types\n\n(nominal_type_identifier) @type\n\n(nominal_type_arguments\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_type_parameters\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_formal_parameters \"this\" @variable.builtin)\n\n; Parameters\n(nominal_type_guard\n  bound: (identifier) @parameter)\n\n; Method signatures\n(nominal_method_signature name: (_) @method)\n\n; Property signatures\n(property_signature\n  name: (property_identifier) @method\n  nominal_type: (nominal_type_annotation \n                  [(function_nominal_type)\n                   (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n(nominal_property_signature\n  name: (property_identifier) @method\n  type: (nominal_type_annotation \n          [(function_nominal_type)\n           (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n```"]
pub struct HighlightsMatch<'cursor, 'tree> {
    match_: tree_sitter::QueryMatch<'cursor, 'tree>,
    tree: &'tree yak_sitter::Tree,
}
#[doc = "A capture returned by the query [Highlights]:\n\n```sexp\n; inherits typescript\n; Keywords\n\n\"guard\" @keyword\n\n; Punctuation\n\n(\"<\" . \";\") @operator\n\n\";\" @punctuation.delimiter\n(nullable_nominal_type \"?\" @punctuation.delimiter)\n(optional_nominal_type \"?\" @punctuation.special)\n(nominal_property_signature \"?\" @punctuation.special)\n(nominal_method_signature \"?\" @punctuation.special)\n\n; (nominal_wrap_expression \";\" @punctuation.special)\n; (nominal_wrap_unchecked_expression [\"!\" \";\"] @punctuation.special)\n\n; (nominal_type_annotation \";\" @punctuation.delimiter)\n\n; Types\n\n(nominal_type_identifier) @type\n\n(nominal_type_arguments\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_type_parameters\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_formal_parameters \"this\" @variable.builtin)\n\n; Parameters\n(nominal_type_guard\n  bound: (identifier) @parameter)\n\n; Method signatures\n(nominal_method_signature name: (_) @method)\n\n; Property signatures\n(property_signature\n  name: (property_identifier) @method\n  nominal_type: (nominal_type_annotation \n                  [(function_nominal_type)\n                   (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n(nominal_property_signature\n  name: (property_identifier) @method\n  type: (nominal_type_annotation \n          [(function_nominal_type)\n           (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n```"]
pub enum HighlightsCapture<'cursor, 'tree> {
    #[doc = "A `keyword` ([crate::syntax::nodes::unnamed::Guard])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"guard\" @keyword"]
    #[doc = "```"]
    Keyword {
        node: crate::syntax::nodes::unnamed::Guard<'tree>,
        match_: Option<HighlightsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `operator` ([crate::syntax::nodes::symbols::Lt])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(\"<\" . \";\") @operator"]
    #[doc = "```"]
    Operator {
        node: crate::syntax::nodes::symbols::Lt<'tree>,
        match_: Option<HighlightsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `punctuation.delimiter` ([anon_unions::PunctuationDelimiter])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\";\" @punctuation.delimiter"]
    #[doc = "\"?\" @punctuation.delimiter"]
    #[doc = "```"]
    PunctuationDelimiter {
        node: anon_unions::PunctuationDelimiter<'tree>,
        match_: Option<HighlightsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `punctuation.special` ([anon_unions::PunctuationSpecial])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"?\" @punctuation.special"]
    #[doc = "\"?\" @punctuation.special"]
    #[doc = "\"?\" @punctuation.special"]
    #[doc = "```"]
    PunctuationSpecial {
        node: crate::syntax::nodes::symbols::Question<'tree>,
        match_: Option<HighlightsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `type` ([crate::syntax::nodes::NominalTypeIdentifier])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(nominal_type_identifier) @type"]
    #[doc = "```"]
    Type {
        node: crate::syntax::nodes::NominalTypeIdentifier<'tree>,
        match_: Option<HighlightsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `punctuation.bracket` ([anon_unions::PunctuationBracket])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"<\" @punctuation.bracket"]
    #[doc = "\">\" @punctuation.bracket"]
    #[doc = "\"<\" @punctuation.bracket"]
    #[doc = "\">\" @punctuation.bracket"]
    #[doc = "```"]
    PunctuationBracket {
        node: anon_unions::PunctuationBracket<'tree>,
        match_: Option<HighlightsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `variable.builtin` ([crate::syntax::nodes::unnamed::This])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"this\" @variable.builtin"]
    #[doc = "```"]
    VariableBuiltin {
        node: crate::syntax::nodes::unnamed::This<'tree>,
        match_: Option<HighlightsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `parameter` ([crate::syntax::nodes::Identifier])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(identifier) @parameter"]
    #[doc = "```"]
    Parameter {
        node: crate::syntax::nodes::Identifier<'tree>,
        match_: Option<HighlightsMatch<'cursor, 'tree>>,
    },
    #[doc = "A `method` ([type_sitter_lib::UntypedNamedNode])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(_) @method"]
    #[doc = "(property_identifier) @method"]
    #[doc = "(property_identifier) @method"]
    #[doc = "```"]
    Method {
        node: type_sitter_lib::UntypedNamedNode<'tree>,
        match_: Option<HighlightsMatch<'cursor, 'tree>>,
    },
}
#[automatically_derived]
impl type_sitter_lib::TypedQuery for Highlights {
    type Match<'cursor, 'tree: 'cursor> = HighlightsMatch<'cursor, 'tree>;
    type Capture<'cursor, 'tree: 'cursor> = HighlightsCapture<'cursor, 'tree>;
    fn query_str(&self) -> &'static str {
        "; inherits typescript\n; Keywords\n\n\"guard\" @keyword\n\n; Punctuation\n\n(\"<\" . \";\") @operator\n\n\";\" @punctuation.delimiter\n(nullable_nominal_type \"?\" @punctuation.delimiter)\n(optional_nominal_type \"?\" @punctuation.special)\n(nominal_property_signature \"?\" @punctuation.special)\n(nominal_method_signature \"?\" @punctuation.special)\n\n; (nominal_wrap_expression \";\" @punctuation.special)\n; (nominal_wrap_unchecked_expression [\"!\" \";\"] @punctuation.special)\n\n; (nominal_type_annotation \";\" @punctuation.delimiter)\n\n; Types\n\n(nominal_type_identifier) @type\n\n(nominal_type_arguments\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_type_parameters\n  \"<\" @punctuation.bracket\n  \">\" @punctuation.bracket)\n(nominal_formal_parameters \"this\" @variable.builtin)\n\n; Parameters\n(nominal_type_guard\n  bound: (identifier) @parameter)\n\n; Method signatures\n(nominal_method_signature name: (_) @method)\n\n; Property signatures\n(property_signature\n  name: (property_identifier) @method\n  nominal_type: (nominal_type_annotation \n                  [(function_nominal_type)\n                   (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n\n(nominal_property_signature\n  name: (property_identifier) @method\n  type: (nominal_type_annotation \n          [(function_nominal_type)\n           (nullable_nominal_type (parenthesized_nominal_type (function_nominal_type)))]))\n"
    }
    fn query(&self) -> &'static tree_sitter::Query {
        __Highlights__.get_or_init(__Mk__Highlights)
    }
    #[inline]
    unsafe fn wrap_match<'cursor, 'tree>(
        &self,
        match_: tree_sitter::QueryMatch<'cursor, 'tree>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Match<'cursor, 'tree> {
        Self::Match { match_, tree }
    }
    #[inline]
    unsafe fn wrap_capture<'cursor, 'tree>(
        &self,
        capture: tree_sitter::QueryCapture<'tree>,
        match_: Option<Self::Match<'cursor, 'tree>>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Capture<'cursor, 'tree> {
        match capture . index as usize { 0usize => Self :: Capture :: Keyword { node : < crate :: syntax :: nodes :: unnamed :: Guard < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 1usize => Self :: Capture :: Operator { node : < crate :: syntax :: nodes :: symbols :: Lt < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 2usize => Self :: Capture :: PunctuationDelimiter { node : < anon_unions :: PunctuationDelimiter < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 3usize => Self :: Capture :: PunctuationSpecial { node : < crate :: syntax :: nodes :: symbols :: Question < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 4usize => Self :: Capture :: Type { node : < crate :: syntax :: nodes :: NominalTypeIdentifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 5usize => Self :: Capture :: PunctuationBracket { node : < anon_unions :: PunctuationBracket < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 6usize => Self :: Capture :: VariableBuiltin { node : < crate :: syntax :: nodes :: unnamed :: This < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 7usize => Self :: Capture :: Parameter { node : < crate :: syntax :: nodes :: Identifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 8usize => Self :: Capture :: Method { node : < type_sitter_lib :: UntypedNamedNode < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , capture_index => unreachable ! ("Invalid capture index: {}" , capture_index) }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> HighlightsMatch<'cursor, 'tree> {
    #[doc = "Returns an iterator over the nodes captured by `keyword` ([crate::syntax::nodes::unnamed::Guard])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"guard\" @keyword"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn keyword(&self) -> Option<crate::syntax::nodes::unnamed::Guard<'tree>> {
        { [0u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: unnamed :: Guard < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `operator` ([crate::syntax::nodes::symbols::Lt])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(\"<\" . \";\") @operator"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn operator(&self) -> Option<crate::syntax::nodes::symbols::Lt<'tree>> {
        { [1u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: symbols :: Lt < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `punctuation.delimiter` ([anon_unions::PunctuationDelimiter])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\";\" @punctuation.delimiter"]
    #[doc = "\"?\" @punctuation.delimiter"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn punctuation_delimiter(&self) -> Option<anon_unions::PunctuationDelimiter<'tree>> {
        {
            [2u32]
                .into_iter()
                .flat_map(|i| self.match_.nodes_for_capture_index(i))
                .map(|n| unsafe {
                    <anon_unions::PunctuationDelimiter<'tree> as type_sitter_lib::TypedNode<
                        'tree,
                    >>::from_node_unchecked(yak_sitter::Node::new(n, self.tree))
                })
        }
        .next()
    }
    #[doc = "Returns an iterator over the nodes captured by `punctuation.special` ([anon_unions::PunctuationSpecial])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"?\" @punctuation.special"]
    #[doc = "\"?\" @punctuation.special"]
    #[doc = "\"?\" @punctuation.special"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn punctuation_special(&self) -> Option<crate::syntax::nodes::symbols::Question<'tree>> {
        { [3u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: symbols :: Question < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `type` ([crate::syntax::nodes::NominalTypeIdentifier])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(nominal_type_identifier) @type"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn r#type(&self) -> Option<crate::syntax::nodes::NominalTypeIdentifier<'tree>> {
        {
            [4u32]
                .into_iter()
                .flat_map(|i| self.match_.nodes_for_capture_index(i))
                .map(|n| unsafe {
                    <crate::syntax::nodes::NominalTypeIdentifier<'tree> as type_sitter_lib::TypedNode<
                        'tree,
                    >>::from_node_unchecked(yak_sitter::Node::new(n, self.tree))
                })
        }
        .next()
    }
    #[doc = "Returns an iterator over the nodes captured by `punctuation.bracket` ([anon_unions::PunctuationBracket])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"<\" @punctuation.bracket"]
    #[doc = "\">\" @punctuation.bracket"]
    #[doc = "\"<\" @punctuation.bracket"]
    #[doc = "\">\" @punctuation.bracket"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn punctuation_bracket(&self) -> Option<anon_unions::PunctuationBracket<'tree>> {
        { [5u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < anon_unions :: PunctuationBracket < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `variable.builtin` ([crate::syntax::nodes::unnamed::This])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"this\" @variable.builtin"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn variable_builtin(&self) -> Option<crate::syntax::nodes::unnamed::This<'tree>> {
        { [6u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: unnamed :: This < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `parameter` ([crate::syntax::nodes::Identifier])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(identifier) @parameter"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn parameter(&self) -> Option<crate::syntax::nodes::Identifier<'tree>> {
        { [7u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: Identifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
    }
    #[doc = "Returns an iterator over the nodes captured by `method` ([type_sitter_lib::UntypedNamedNode])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(_) @method"]
    #[doc = "(property_identifier) @method"]
    #[doc = "(property_identifier) @method"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn method(&self) -> impl Iterator<Item = type_sitter_lib::UntypedNamedNode<'tree>> + '_ {
        {
            [8u32]
                .into_iter()
                .flat_map(|i| self.match_.nodes_for_capture_index(i))
                .map(|n| unsafe {
                    <type_sitter_lib::UntypedNamedNode<'tree> as type_sitter_lib::TypedNode<
                        'tree,
                    >>::from_node_unchecked(yak_sitter::Node::new(n, self.tree))
                })
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> std::fmt::Debug for HighlightsMatch<'cursor, 'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(HighlightsMatch))
            .field("match_", &self.match_)
            .finish()
    }
}
#[automatically_derived]
impl<'cursor, 'tree> type_sitter_lib::TypedQueryMatch<'cursor, 'tree>
    for HighlightsMatch<'cursor, 'tree>
{
    type Query = Highlights;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Highlights
    }
    #[inline]
    fn tree(&self) -> &'tree yak_sitter::Tree {
        self.tree
    }
    #[inline]
    fn raw(&self) -> &tree_sitter::QueryMatch<'cursor, 'tree> {
        &self.match_
    }
    #[inline]
    fn into_raw(self) -> tree_sitter::QueryMatch<'cursor, 'tree> {
        self.match_
    }
}
#[automatically_derived]
impl<'cursor, 'tree> HighlightsCapture<'cursor, 'tree> {
    #[doc = "Try to interpret this capture as a `keyword` ([crate::syntax::nodes::unnamed::Guard])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"guard\" @keyword"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn keyword(&self) -> Option<&crate::syntax::nodes::unnamed::Guard<'tree>> {
        match self {
            Self::Keyword { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `operator` ([crate::syntax::nodes::symbols::Lt])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(\"<\" . \";\") @operator"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn operator(&self) -> Option<&crate::syntax::nodes::symbols::Lt<'tree>> {
        match self {
            Self::Operator { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `punctuation.delimiter` ([anon_unions::PunctuationDelimiter])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\";\" @punctuation.delimiter"]
    #[doc = "\"?\" @punctuation.delimiter"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn punctuation_delimiter(&self) -> Option<&anon_unions::PunctuationDelimiter<'tree>> {
        match self {
            Self::PunctuationDelimiter { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `punctuation.special` ([anon_unions::PunctuationSpecial])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"?\" @punctuation.special"]
    #[doc = "\"?\" @punctuation.special"]
    #[doc = "\"?\" @punctuation.special"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn punctuation_special(&self) -> Option<&crate::syntax::nodes::symbols::Question<'tree>> {
        match self {
            Self::PunctuationSpecial { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `type` ([crate::syntax::nodes::NominalTypeIdentifier])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(nominal_type_identifier) @type"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn r#type(&self) -> Option<&crate::syntax::nodes::NominalTypeIdentifier<'tree>> {
        match self {
            Self::Type { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `punctuation.bracket` ([anon_unions::PunctuationBracket])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"<\" @punctuation.bracket"]
    #[doc = "\">\" @punctuation.bracket"]
    #[doc = "\"<\" @punctuation.bracket"]
    #[doc = "\">\" @punctuation.bracket"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn punctuation_bracket(&self) -> Option<&anon_unions::PunctuationBracket<'tree>> {
        match self {
            Self::PunctuationBracket { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `variable.builtin` ([crate::syntax::nodes::unnamed::This])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "\"this\" @variable.builtin"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn variable_builtin(&self) -> Option<&crate::syntax::nodes::unnamed::This<'tree>> {
        match self {
            Self::VariableBuiltin { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `parameter` ([crate::syntax::nodes::Identifier])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(identifier) @parameter"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn parameter(&self) -> Option<&crate::syntax::nodes::Identifier<'tree>> {
        match self {
            Self::Parameter { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
    #[doc = "Try to interpret this capture as a `method` ([type_sitter_lib::UntypedNamedNode])"]
    #[doc = ""]
    #[doc = "The full capture including pattern(s) is:"]
    #[doc = "```sexp"]
    #[doc = "(_) @method"]
    #[doc = "(property_identifier) @method"]
    #[doc = "(property_identifier) @method"]
    #[doc = "```"]
    #[inline]
    #[allow(unused, non_snake_case)]
    pub fn method(&self) -> Option<&type_sitter_lib::UntypedNamedNode<'tree>> {
        match self {
            Self::Method { node, .. } => Some(node),
            #[allow(unreachable_patterns)]
            _ => None,
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> std::fmt::Debug for HighlightsCapture<'cursor, 'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Keyword { node, .. } => f
                .debug_struct(concat!(
                    stringify!(HighlightsCapture),
                    "::",
                    stringify!(Keyword)
                ))
                .field("node", node)
                .finish(),
            Self::Operator { node, .. } => f
                .debug_struct(concat!(
                    stringify!(HighlightsCapture),
                    "::",
                    stringify!(Operator)
                ))
                .field("node", node)
                .finish(),
            Self::PunctuationDelimiter { node, .. } => f
                .debug_struct(concat!(
                    stringify!(HighlightsCapture),
                    "::",
                    stringify!(PunctuationDelimiter)
                ))
                .field("node", node)
                .finish(),
            Self::PunctuationSpecial { node, .. } => f
                .debug_struct(concat!(
                    stringify!(HighlightsCapture),
                    "::",
                    stringify!(PunctuationSpecial)
                ))
                .field("node", node)
                .finish(),
            Self::Type { node, .. } => f
                .debug_struct(concat!(
                    stringify!(HighlightsCapture),
                    "::",
                    stringify!(Type)
                ))
                .field("node", node)
                .finish(),
            Self::PunctuationBracket { node, .. } => f
                .debug_struct(concat!(
                    stringify!(HighlightsCapture),
                    "::",
                    stringify!(PunctuationBracket)
                ))
                .field("node", node)
                .finish(),
            Self::VariableBuiltin { node, .. } => f
                .debug_struct(concat!(
                    stringify!(HighlightsCapture),
                    "::",
                    stringify!(VariableBuiltin)
                ))
                .field("node", node)
                .finish(),
            Self::Parameter { node, .. } => f
                .debug_struct(concat!(
                    stringify!(HighlightsCapture),
                    "::",
                    stringify!(Parameter)
                ))
                .field("node", node)
                .finish(),
            Self::Method { node, .. } => f
                .debug_struct(concat!(
                    stringify!(HighlightsCapture),
                    "::",
                    stringify!(Method)
                ))
                .field("node", node)
                .finish(),
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> Clone for HighlightsCapture<'cursor, 'tree> {
    fn clone(&self) -> Self {
        match self {
            Self::Keyword { node, .. } => Self::Keyword {
                node: *node,
                match_: None,
            },
            Self::Operator { node, .. } => Self::Operator {
                node: *node,
                match_: None,
            },
            Self::PunctuationDelimiter { node, .. } => Self::PunctuationDelimiter {
                node: *node,
                match_: None,
            },
            Self::PunctuationSpecial { node, .. } => Self::PunctuationSpecial {
                node: *node,
                match_: None,
            },
            Self::Type { node, .. } => Self::Type {
                node: *node,
                match_: None,
            },
            Self::PunctuationBracket { node, .. } => Self::PunctuationBracket {
                node: *node,
                match_: None,
            },
            Self::VariableBuiltin { node, .. } => Self::VariableBuiltin {
                node: *node,
                match_: None,
            },
            Self::Parameter { node, .. } => Self::Parameter {
                node: *node,
                match_: None,
            },
            Self::Method { node, .. } => Self::Method {
                node: *node,
                match_: None,
            },
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> type_sitter_lib::TypedQueryCapture<'cursor, 'tree>
    for HighlightsCapture<'cursor, 'tree>
{
    type Query = Highlights;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Highlights
    }
    #[inline]
    fn match_(
        &self,
    ) -> Option<&<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {
            Self::Keyword { match_, .. } => match_.as_ref(),
            Self::Operator { match_, .. } => match_.as_ref(),
            Self::PunctuationDelimiter { match_, .. } => match_.as_ref(),
            Self::PunctuationSpecial { match_, .. } => match_.as_ref(),
            Self::Type { match_, .. } => match_.as_ref(),
            Self::PunctuationBracket { match_, .. } => match_.as_ref(),
            Self::VariableBuiltin { match_, .. } => match_.as_ref(),
            Self::Parameter { match_, .. } => match_.as_ref(),
            Self::Method { match_, .. } => match_.as_ref(),
        }
    }
    #[inline]
    fn into_match(
        self,
    ) -> Option<<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {
            Self::Keyword { match_, .. } => match_,
            Self::Operator { match_, .. } => match_,
            Self::PunctuationDelimiter { match_, .. } => match_,
            Self::PunctuationSpecial { match_, .. } => match_,
            Self::Type { match_, .. } => match_,
            Self::PunctuationBracket { match_, .. } => match_,
            Self::VariableBuiltin { match_, .. } => match_,
            Self::Parameter { match_, .. } => match_,
            Self::Method { match_, .. } => match_,
        }
    }
    #[inline]
    fn to_raw(&self) -> yak_sitter::QueryCapture<'static, 'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {
            Self::Keyword { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 0usize,
                name: "keyword",
            },
            Self::Operator { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 1usize,
                name: "operator",
            },
            Self::PunctuationDelimiter { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 2usize,
                name: "punctuation.delimiter",
            },
            Self::PunctuationSpecial { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 3usize,
                name: "punctuation.special",
            },
            Self::Type { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 4usize,
                name: "type",
            },
            Self::PunctuationBracket { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 5usize,
                name: "punctuation.bracket",
            },
            Self::VariableBuiltin { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 6usize,
                name: "variable.builtin",
            },
            Self::Parameter { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 7usize,
                name: "parameter",
            },
            Self::Method { node, .. } => yak_sitter::QueryCapture {
                node: *node.node(),
                index: 8usize,
                name: "method",
            },
        }
    }
    #[inline]
    fn node(&self) -> &yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {
            Self::Keyword { node, .. } => node.node(),
            Self::Operator { node, .. } => node.node(),
            Self::PunctuationDelimiter { node, .. } => node.node(),
            Self::PunctuationSpecial { node, .. } => node.node(),
            Self::Type { node, .. } => node.node(),
            Self::PunctuationBracket { node, .. } => node.node(),
            Self::VariableBuiltin { node, .. } => node.node(),
            Self::Parameter { node, .. } => node.node(),
            Self::Method { node, .. } => node.node(),
        }
    }
    #[inline]
    fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {
            Self::Keyword { node, .. } => node.node_mut(),
            Self::Operator { node, .. } => node.node_mut(),
            Self::PunctuationDelimiter { node, .. } => node.node_mut(),
            Self::PunctuationSpecial { node, .. } => node.node_mut(),
            Self::Type { node, .. } => node.node_mut(),
            Self::PunctuationBracket { node, .. } => node.node_mut(),
            Self::VariableBuiltin { node, .. } => node.node_mut(),
            Self::Parameter { node, .. } => node.node_mut(),
            Self::Method { node, .. } => node.node_mut(),
        }
    }
    #[inline]
    fn name(&self) -> &'static str {
        match self {
            Self::Keyword { .. } => "keyword",
            Self::Operator { .. } => "operator",
            Self::PunctuationDelimiter { .. } => "punctuation.delimiter",
            Self::PunctuationSpecial { .. } => "punctuation.special",
            Self::Type { .. } => "type",
            Self::PunctuationBracket { .. } => "punctuation.bracket",
            Self::VariableBuiltin { .. } => "variable.builtin",
            Self::Parameter { .. } => "parameter",
            Self::Method { .. } => "method",
        }
    }
    #[inline]
    fn index(&self) -> usize {
        match self {
            Self::Keyword { .. } => 0usize,
            Self::Operator { .. } => 1usize,
            Self::PunctuationDelimiter { .. } => 2usize,
            Self::PunctuationSpecial { .. } => 3usize,
            Self::Type { .. } => 4usize,
            Self::PunctuationBracket { .. } => 5usize,
            Self::VariableBuiltin { .. } => 6usize,
            Self::Parameter { .. } => 7usize,
            Self::Method { .. } => 8usize,
        }
    }
}
pub mod semantic {
    #[allow(non_upper_case_globals)]
    static __Decls__: type_sitter_lib::gen_internal::TypedQueryOnceBox<tree_sitter::Query> =
        type_sitter_lib::gen_internal::TypedQueryOnceBox::new();
    #[allow(non_snake_case)]
    fn __Mk__Decls() -> Box<tree_sitter::Query> {
        # [allow (unused_mut)] let mut query = tree_sitter :: Query :: new (tree_sitter_nominalscript :: language_nominalscript () , "; Exports\n(export_specifier (nominal_type_identifier) @nominal.export_id alias: (identifier)? @alias.export_id)\n(export_specifier (identifier) @value.export_id alias: (identifier)? @alias.export_id)\n(export_statement (nominal_type_declaration (nominal_type_identifier) @nominal.export_id))\n(export_statement (function_declaration (identifier) @value.export_id))\n(export_statement (function_signature (identifier) @value.export_id))\n(export_statement (lexical_declaration (variable_declarator (identifier) @value.export_id)))\n(export_statement (variable_declaration (variable_declarator (identifier) @value.export_id)))\n\n; Imports\n(import_statement) @import\n\n; Nominal type declarations (local and module-root)\n(nominal_type_declaration) @nominal_type.decl\n\n; Function declarations (local and module-root)\n(function_declaration) @function.decl\n(generator_function_declaration) @function.decl\n(function_signature) @function.decl\n\n; Value declarations (local and module-root)\n(variable_declarator) @value.decl\n") . expect ("query parsed at compile-time but failed at runtime. Is the language 'tree_sitter_nominalscript' correct, and did you use the same tree-sitter / tree_sitter_nominalscript version?") ;
        Box::new(query)
    }
    #[doc = "Typed version of the query:\n\n```sexp\n; Exports\n(export_specifier (nominal_type_identifier) @nominal.export_id alias: (identifier)? @alias.export_id)\n(export_specifier (identifier) @value.export_id alias: (identifier)? @alias.export_id)\n(export_statement (nominal_type_declaration (nominal_type_identifier) @nominal.export_id))\n(export_statement (function_declaration (identifier) @value.export_id))\n(export_statement (function_signature (identifier) @value.export_id))\n(export_statement (lexical_declaration (variable_declarator (identifier) @value.export_id)))\n(export_statement (variable_declaration (variable_declarator (identifier) @value.export_id)))\n\n; Imports\n(import_statement) @import\n\n; Nominal type declarations (local and module-root)\n(nominal_type_declaration) @nominal_type.decl\n\n; Function declarations (local and module-root)\n(function_declaration) @function.decl\n(generator_function_declaration) @function.decl\n(function_signature) @function.decl\n\n; Value declarations (local and module-root)\n(variable_declarator) @value.decl\n\n```"]
    #[allow(non_camel_case_types)]
    #[derive(Debug, Clone, Copy)]
    pub struct Decls;
    #[doc = "Matches returned by a query cursor running the query [Decls]:\n\n```sexp\n; Exports\n(export_specifier (nominal_type_identifier) @nominal.export_id alias: (identifier)? @alias.export_id)\n(export_specifier (identifier) @value.export_id alias: (identifier)? @alias.export_id)\n(export_statement (nominal_type_declaration (nominal_type_identifier) @nominal.export_id))\n(export_statement (function_declaration (identifier) @value.export_id))\n(export_statement (function_signature (identifier) @value.export_id))\n(export_statement (lexical_declaration (variable_declarator (identifier) @value.export_id)))\n(export_statement (variable_declaration (variable_declarator (identifier) @value.export_id)))\n\n; Imports\n(import_statement) @import\n\n; Nominal type declarations (local and module-root)\n(nominal_type_declaration) @nominal_type.decl\n\n; Function declarations (local and module-root)\n(function_declaration) @function.decl\n(generator_function_declaration) @function.decl\n(function_signature) @function.decl\n\n; Value declarations (local and module-root)\n(variable_declarator) @value.decl\n\n```"]
    #[allow(unused, non_camel_case_types)]
    pub type DeclsMatches<'cursor, 'tree> =
        type_sitter_lib::TypedQueryMatches<'cursor, 'tree, Decls>;
    #[doc = "Captures returned by a query cursor running the query [Decls]:\n\n```sexp\n; Exports\n(export_specifier (nominal_type_identifier) @nominal.export_id alias: (identifier)? @alias.export_id)\n(export_specifier (identifier) @value.export_id alias: (identifier)? @alias.export_id)\n(export_statement (nominal_type_declaration (nominal_type_identifier) @nominal.export_id))\n(export_statement (function_declaration (identifier) @value.export_id))\n(export_statement (function_signature (identifier) @value.export_id))\n(export_statement (lexical_declaration (variable_declarator (identifier) @value.export_id)))\n(export_statement (variable_declaration (variable_declarator (identifier) @value.export_id)))\n\n; Imports\n(import_statement) @import\n\n; Nominal type declarations (local and module-root)\n(nominal_type_declaration) @nominal_type.decl\n\n; Function declarations (local and module-root)\n(function_declaration) @function.decl\n(generator_function_declaration) @function.decl\n(function_signature) @function.decl\n\n; Value declarations (local and module-root)\n(variable_declarator) @value.decl\n\n```"]
    #[allow(unused, non_camel_case_types)]
    pub type DeclsCaptures<'cursor, 'tree> =
        type_sitter_lib::TypedQueryCaptures<'cursor, 'tree, Decls>;
    #[doc = "A match returned by the query [Decls]:\n\n```sexp\n; Exports\n(export_specifier (nominal_type_identifier) @nominal.export_id alias: (identifier)? @alias.export_id)\n(export_specifier (identifier) @value.export_id alias: (identifier)? @alias.export_id)\n(export_statement (nominal_type_declaration (nominal_type_identifier) @nominal.export_id))\n(export_statement (function_declaration (identifier) @value.export_id))\n(export_statement (function_signature (identifier) @value.export_id))\n(export_statement (lexical_declaration (variable_declarator (identifier) @value.export_id)))\n(export_statement (variable_declaration (variable_declarator (identifier) @value.export_id)))\n\n; Imports\n(import_statement) @import\n\n; Nominal type declarations (local and module-root)\n(nominal_type_declaration) @nominal_type.decl\n\n; Function declarations (local and module-root)\n(function_declaration) @function.decl\n(generator_function_declaration) @function.decl\n(function_signature) @function.decl\n\n; Value declarations (local and module-root)\n(variable_declarator) @value.decl\n\n```"]
    pub struct DeclsMatch<'cursor, 'tree> {
        match_: tree_sitter::QueryMatch<'cursor, 'tree>,
        tree: &'tree yak_sitter::Tree,
    }
    #[doc = "A capture returned by the query [Decls]:\n\n```sexp\n; Exports\n(export_specifier (nominal_type_identifier) @nominal.export_id alias: (identifier)? @alias.export_id)\n(export_specifier (identifier) @value.export_id alias: (identifier)? @alias.export_id)\n(export_statement (nominal_type_declaration (nominal_type_identifier) @nominal.export_id))\n(export_statement (function_declaration (identifier) @value.export_id))\n(export_statement (function_signature (identifier) @value.export_id))\n(export_statement (lexical_declaration (variable_declarator (identifier) @value.export_id)))\n(export_statement (variable_declaration (variable_declarator (identifier) @value.export_id)))\n\n; Imports\n(import_statement) @import\n\n; Nominal type declarations (local and module-root)\n(nominal_type_declaration) @nominal_type.decl\n\n; Function declarations (local and module-root)\n(function_declaration) @function.decl\n(generator_function_declaration) @function.decl\n(function_signature) @function.decl\n\n; Value declarations (local and module-root)\n(variable_declarator) @value.decl\n\n```"]
    pub enum DeclsCapture<'cursor, 'tree> {
        #[doc = "A `nominal.export_id` ([anon_unions::NominalExportId])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(nominal_type_identifier) @nominal.export_id"]
        #[doc = "(nominal_type_identifier) @nominal.export_id"]
        #[doc = "```"]
        NominalExportId {
            node: crate::syntax::nodes::NominalTypeIdentifier<'tree>,
            match_: Option<DeclsMatch<'cursor, 'tree>>,
        },
        #[doc = "A `alias.export_id` ([anon_unions::AliasExportId])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(identifier) @alias.export_id"]
        #[doc = "(identifier) @alias.export_id"]
        #[doc = "```"]
        AliasExportId {
            node: crate::syntax::nodes::Identifier<'tree>,
            match_: Option<DeclsMatch<'cursor, 'tree>>,
        },
        #[doc = "A `value.export_id` ([anon_unions::ValueExportId])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "```"]
        ValueExportId {
            node: crate::syntax::nodes::Identifier<'tree>,
            match_: Option<DeclsMatch<'cursor, 'tree>>,
        },
        #[doc = "A `import` ([crate::syntax::nodes::ImportStatement])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(import_statement) @import"]
        #[doc = "```"]
        Import {
            node: crate::syntax::nodes::ImportStatement<'tree>,
            match_: Option<DeclsMatch<'cursor, 'tree>>,
        },
        #[doc = "A `nominal_type.decl` ([crate::syntax::nodes::NominalTypeDeclaration])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(nominal_type_declaration) @nominal_type.decl"]
        #[doc = "```"]
        NominalTypeDecl {
            node: crate::syntax::nodes::NominalTypeDeclaration<'tree>,
            match_: Option<DeclsMatch<'cursor, 'tree>>,
        },
        #[doc = "A `function.decl` ([anon_unions::FunctionDecl])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(function_declaration) @function.decl"]
        #[doc = "(generator_function_declaration) @function.decl"]
        #[doc = "(function_signature) @function.decl"]
        #[doc = "```"]
        FunctionDecl {
            node: anon_unions::FunctionDecl<'tree>,
            match_: Option<DeclsMatch<'cursor, 'tree>>,
        },
        #[doc = "A `value.decl` ([crate::syntax::nodes::VariableDeclarator])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(variable_declarator) @value.decl"]
        #[doc = "```"]
        ValueDecl {
            node: crate::syntax::nodes::VariableDeclarator<'tree>,
            match_: Option<DeclsMatch<'cursor, 'tree>>,
        },
    }
    #[automatically_derived]
    impl type_sitter_lib::TypedQuery for Decls {
        type Match<'cursor, 'tree: 'cursor> = DeclsMatch<'cursor, 'tree>;
        type Capture<'cursor, 'tree: 'cursor> = DeclsCapture<'cursor, 'tree>;
        fn query_str(&self) -> &'static str {
            "; Exports\n(export_specifier (nominal_type_identifier) @nominal.export_id alias: (identifier)? @alias.export_id)\n(export_specifier (identifier) @value.export_id alias: (identifier)? @alias.export_id)\n(export_statement (nominal_type_declaration (nominal_type_identifier) @nominal.export_id))\n(export_statement (function_declaration (identifier) @value.export_id))\n(export_statement (function_signature (identifier) @value.export_id))\n(export_statement (lexical_declaration (variable_declarator (identifier) @value.export_id)))\n(export_statement (variable_declaration (variable_declarator (identifier) @value.export_id)))\n\n; Imports\n(import_statement) @import\n\n; Nominal type declarations (local and module-root)\n(nominal_type_declaration) @nominal_type.decl\n\n; Function declarations (local and module-root)\n(function_declaration) @function.decl\n(generator_function_declaration) @function.decl\n(function_signature) @function.decl\n\n; Value declarations (local and module-root)\n(variable_declarator) @value.decl\n"
        }
        fn query(&self) -> &'static tree_sitter::Query {
            __Decls__.get_or_init(__Mk__Decls)
        }
        #[inline]
        unsafe fn wrap_match<'cursor, 'tree>(
            &self,
            match_: tree_sitter::QueryMatch<'cursor, 'tree>,
            tree: &'tree yak_sitter::Tree,
        ) -> Self::Match<'cursor, 'tree> {
            Self::Match { match_, tree }
        }
        #[inline]
        unsafe fn wrap_capture<'cursor, 'tree>(
            &self,
            capture: tree_sitter::QueryCapture<'tree>,
            match_: Option<Self::Match<'cursor, 'tree>>,
            tree: &'tree yak_sitter::Tree,
        ) -> Self::Capture<'cursor, 'tree> {
            match capture . index as usize { 0usize => Self :: Capture :: NominalExportId { node : < crate :: syntax :: nodes :: NominalTypeIdentifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 1usize => Self :: Capture :: AliasExportId { node : < crate :: syntax :: nodes :: Identifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 2usize => Self :: Capture :: ValueExportId { node : < crate :: syntax :: nodes :: Identifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 3usize => Self :: Capture :: Import { node : < crate :: syntax :: nodes :: ImportStatement < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 4usize => Self :: Capture :: NominalTypeDecl { node : < crate :: syntax :: nodes :: NominalTypeDeclaration < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 5usize => Self :: Capture :: FunctionDecl { node : < anon_unions :: FunctionDecl < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , 6usize => Self :: Capture :: ValueDecl { node : < crate :: syntax :: nodes :: VariableDeclarator < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (unsafe { yak_sitter :: Node :: new (capture . node , tree) }) , match_ } , capture_index => unreachable ! ("Invalid capture index: {}" , capture_index) }
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> DeclsMatch<'cursor, 'tree> {
        #[doc = "Returns an iterator over the nodes captured by `nominal.export_id` ([anon_unions::NominalExportId])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(nominal_type_identifier) @nominal.export_id"]
        #[doc = "(nominal_type_identifier) @nominal.export_id"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn nominal_export_id(&self) -> Option<crate::syntax::nodes::NominalTypeIdentifier<'tree>> {
            {
                [0u32]
                    .into_iter()
                    .flat_map(|i| self.match_.nodes_for_capture_index(i))
                    .map(|n| unsafe {
                        <crate::syntax::nodes::NominalTypeIdentifier<'tree> as type_sitter_lib::TypedNode<
                            'tree,
                        >>::from_node_unchecked(yak_sitter::Node::new(
                            n, self.tree,
                        ))
                    })
            }
            .next()
        }
        #[doc = "Returns an iterator over the nodes captured by `alias.export_id` ([anon_unions::AliasExportId])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(identifier) @alias.export_id"]
        #[doc = "(identifier) @alias.export_id"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn alias_export_id(&self) -> Option<crate::syntax::nodes::Identifier<'tree>> {
            { [1u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: Identifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
        }
        #[doc = "Returns an iterator over the nodes captured by `value.export_id` ([anon_unions::ValueExportId])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn value_export_id(&self) -> Option<crate::syntax::nodes::Identifier<'tree>> {
            { [2u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: Identifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
        }
        #[doc = "Returns an iterator over the nodes captured by `import` ([crate::syntax::nodes::ImportStatement])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(import_statement) @import"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn import(&self) -> Option<crate::syntax::nodes::ImportStatement<'tree>> {
            {
                [3u32]
                    .into_iter()
                    .flat_map(|i| self.match_.nodes_for_capture_index(i))
                    .map(|n| unsafe {
                        <crate::syntax::nodes::ImportStatement<'tree> as type_sitter_lib::TypedNode<
                            'tree,
                        >>::from_node_unchecked(yak_sitter::Node::new(
                            n, self.tree,
                        ))
                    })
            }
            .next()
        }
        #[doc = "Returns an iterator over the nodes captured by `nominal_type.decl` ([crate::syntax::nodes::NominalTypeDeclaration])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(nominal_type_declaration) @nominal_type.decl"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn nominal_type_decl(&self) -> Option<crate::syntax::nodes::NominalTypeDeclaration<'tree>> {
            { [4u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < crate :: syntax :: nodes :: NominalTypeDeclaration < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
        }
        #[doc = "Returns an iterator over the nodes captured by `function.decl` ([anon_unions::FunctionDecl])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(function_declaration) @function.decl"]
        #[doc = "(generator_function_declaration) @function.decl"]
        #[doc = "(function_signature) @function.decl"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn function_decl(&self) -> Option<anon_unions::FunctionDecl<'tree>> {
            { [5u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < anon_unions :: FunctionDecl < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next ()
        }
        #[doc = "Returns an iterator over the nodes captured by `value.decl` ([crate::syntax::nodes::VariableDeclarator])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(variable_declarator) @value.decl"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn value_decl(&self) -> Option<crate::syntax::nodes::VariableDeclarator<'tree>> {
            {
                [6u32]
                    .into_iter()
                    .flat_map(|i| self.match_.nodes_for_capture_index(i))
                    .map(|n| unsafe {
                        <crate::syntax::nodes::VariableDeclarator<'tree> as type_sitter_lib::TypedNode<
                            'tree,
                        >>::from_node_unchecked(yak_sitter::Node::new(
                            n, self.tree,
                        ))
                    })
            }
            .next()
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> std::fmt::Debug for DeclsMatch<'cursor, 'tree> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct(stringify!(DeclsMatch))
                .field("match_", &self.match_)
                .finish()
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> type_sitter_lib::TypedQueryMatch<'cursor, 'tree>
        for DeclsMatch<'cursor, 'tree>
    {
        type Query = Decls;
        #[inline]
        fn query(&self) -> &'cursor Self::Query {
            &Decls
        }
        #[inline]
        fn tree(&self) -> &'tree yak_sitter::Tree {
            self.tree
        }
        #[inline]
        fn raw(&self) -> &tree_sitter::QueryMatch<'cursor, 'tree> {
            &self.match_
        }
        #[inline]
        fn into_raw(self) -> tree_sitter::QueryMatch<'cursor, 'tree> {
            self.match_
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> DeclsCapture<'cursor, 'tree> {
        #[doc = "Try to interpret this capture as a `nominal.export_id` ([anon_unions::NominalExportId])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(nominal_type_identifier) @nominal.export_id"]
        #[doc = "(nominal_type_identifier) @nominal.export_id"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn nominal_export_id(&self) -> Option<&crate::syntax::nodes::NominalTypeIdentifier<'tree>> {
            match self {
                Self::NominalExportId { node, .. } => Some(node),
                #[allow(unreachable_patterns)]
                _ => None,
            }
        }
        #[doc = "Try to interpret this capture as a `alias.export_id` ([anon_unions::AliasExportId])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(identifier) @alias.export_id"]
        #[doc = "(identifier) @alias.export_id"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn alias_export_id(&self) -> Option<&crate::syntax::nodes::Identifier<'tree>> {
            match self {
                Self::AliasExportId { node, .. } => Some(node),
                #[allow(unreachable_patterns)]
                _ => None,
            }
        }
        #[doc = "Try to interpret this capture as a `value.export_id` ([anon_unions::ValueExportId])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "(identifier) @value.export_id"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn value_export_id(&self) -> Option<&crate::syntax::nodes::Identifier<'tree>> {
            match self {
                Self::ValueExportId { node, .. } => Some(node),
                #[allow(unreachable_patterns)]
                _ => None,
            }
        }
        #[doc = "Try to interpret this capture as a `import` ([crate::syntax::nodes::ImportStatement])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(import_statement) @import"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn import(&self) -> Option<&crate::syntax::nodes::ImportStatement<'tree>> {
            match self {
                Self::Import { node, .. } => Some(node),
                #[allow(unreachable_patterns)]
                _ => None,
            }
        }
        #[doc = "Try to interpret this capture as a `nominal_type.decl` ([crate::syntax::nodes::NominalTypeDeclaration])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(nominal_type_declaration) @nominal_type.decl"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn nominal_type_decl(&self) -> Option<&crate::syntax::nodes::NominalTypeDeclaration<'tree>> {
            match self {
                Self::NominalTypeDecl { node, .. } => Some(node),
                #[allow(unreachable_patterns)]
                _ => None,
            }
        }
        #[doc = "Try to interpret this capture as a `function.decl` ([anon_unions::FunctionDecl])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(function_declaration) @function.decl"]
        #[doc = "(generator_function_declaration) @function.decl"]
        #[doc = "(function_signature) @function.decl"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn function_decl(&self) -> Option<&anon_unions::FunctionDecl<'tree>> {
            match self {
                Self::FunctionDecl { node, .. } => Some(node),
                #[allow(unreachable_patterns)]
                _ => None,
            }
        }
        #[doc = "Try to interpret this capture as a `value.decl` ([crate::syntax::nodes::VariableDeclarator])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(variable_declarator) @value.decl"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn value_decl(&self) -> Option<&crate::syntax::nodes::VariableDeclarator<'tree>> {
            match self {
                Self::ValueDecl { node, .. } => Some(node),
                #[allow(unreachable_patterns)]
                _ => None,
            }
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> std::fmt::Debug for DeclsCapture<'cursor, 'tree> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::NominalExportId { node, .. } => f
                    .debug_struct(concat!(
                        stringify!(DeclsCapture),
                        "::",
                        stringify!(NominalExportId)
                    ))
                    .field("node", node)
                    .finish(),
                Self::AliasExportId { node, .. } => f
                    .debug_struct(concat!(
                        stringify!(DeclsCapture),
                        "::",
                        stringify!(AliasExportId)
                    ))
                    .field("node", node)
                    .finish(),
                Self::ValueExportId { node, .. } => f
                    .debug_struct(concat!(
                        stringify!(DeclsCapture),
                        "::",
                        stringify!(ValueExportId)
                    ))
                    .field("node", node)
                    .finish(),
                Self::Import { node, .. } => f
                    .debug_struct(concat!(stringify!(DeclsCapture), "::", stringify!(Import)))
                    .field("node", node)
                    .finish(),
                Self::NominalTypeDecl { node, .. } => f
                    .debug_struct(concat!(
                        stringify!(DeclsCapture),
                        "::",
                        stringify!(NominalTypeDecl)
                    ))
                    .field("node", node)
                    .finish(),
                Self::FunctionDecl { node, .. } => f
                    .debug_struct(concat!(
                        stringify!(DeclsCapture),
                        "::",
                        stringify!(FunctionDecl)
                    ))
                    .field("node", node)
                    .finish(),
                Self::ValueDecl { node, .. } => f
                    .debug_struct(concat!(
                        stringify!(DeclsCapture),
                        "::",
                        stringify!(ValueDecl)
                    ))
                    .field("node", node)
                    .finish(),
            }
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> Clone for DeclsCapture<'cursor, 'tree> {
        fn clone(&self) -> Self {
            match self {
                Self::NominalExportId { node, .. } => Self::NominalExportId {
                    node: *node,
                    match_: None,
                },
                Self::AliasExportId { node, .. } => Self::AliasExportId {
                    node: *node,
                    match_: None,
                },
                Self::ValueExportId { node, .. } => Self::ValueExportId {
                    node: *node,
                    match_: None,
                },
                Self::Import { node, .. } => Self::Import {
                    node: *node,
                    match_: None,
                },
                Self::NominalTypeDecl { node, .. } => Self::NominalTypeDecl {
                    node: *node,
                    match_: None,
                },
                Self::FunctionDecl { node, .. } => Self::FunctionDecl {
                    node: *node,
                    match_: None,
                },
                Self::ValueDecl { node, .. } => Self::ValueDecl {
                    node: *node,
                    match_: None,
                },
            }
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> type_sitter_lib::TypedQueryCapture<'cursor, 'tree>
        for DeclsCapture<'cursor, 'tree>
    {
        type Query = Decls;
        #[inline]
        fn query(&self) -> &'cursor Self::Query {
            &Decls
        }
        #[inline]
        fn match_(
            &self,
        ) -> Option<&<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
            match self {
                Self::NominalExportId { match_, .. } => match_.as_ref(),
                Self::AliasExportId { match_, .. } => match_.as_ref(),
                Self::ValueExportId { match_, .. } => match_.as_ref(),
                Self::Import { match_, .. } => match_.as_ref(),
                Self::NominalTypeDecl { match_, .. } => match_.as_ref(),
                Self::FunctionDecl { match_, .. } => match_.as_ref(),
                Self::ValueDecl { match_, .. } => match_.as_ref(),
            }
        }
        #[inline]
        fn into_match(
            self,
        ) -> Option<<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
            match self {
                Self::NominalExportId { match_, .. } => match_,
                Self::AliasExportId { match_, .. } => match_,
                Self::ValueExportId { match_, .. } => match_,
                Self::Import { match_, .. } => match_,
                Self::NominalTypeDecl { match_, .. } => match_,
                Self::FunctionDecl { match_, .. } => match_,
                Self::ValueDecl { match_, .. } => match_,
            }
        }
        #[inline]
        fn to_raw(&self) -> yak_sitter::QueryCapture<'static, 'tree> {
            #[allow(unused_imports)]
            use type_sitter_lib::TypedNode;
            match self {
                Self::NominalExportId { node, .. } => yak_sitter::QueryCapture {
                    node: *node.node(),
                    index: 0usize,
                    name: "nominal.export_id",
                },
                Self::AliasExportId { node, .. } => yak_sitter::QueryCapture {
                    node: *node.node(),
                    index: 1usize,
                    name: "alias.export_id",
                },
                Self::ValueExportId { node, .. } => yak_sitter::QueryCapture {
                    node: *node.node(),
                    index: 2usize,
                    name: "value.export_id",
                },
                Self::Import { node, .. } => yak_sitter::QueryCapture {
                    node: *node.node(),
                    index: 3usize,
                    name: "import",
                },
                Self::NominalTypeDecl { node, .. } => yak_sitter::QueryCapture {
                    node: *node.node(),
                    index: 4usize,
                    name: "nominal_type.decl",
                },
                Self::FunctionDecl { node, .. } => yak_sitter::QueryCapture {
                    node: *node.node(),
                    index: 5usize,
                    name: "function.decl",
                },
                Self::ValueDecl { node, .. } => yak_sitter::QueryCapture {
                    node: *node.node(),
                    index: 6usize,
                    name: "value.decl",
                },
            }
        }
        #[inline]
        fn node(&self) -> &yak_sitter::Node<'tree> {
            #[allow(unused_imports)]
            use type_sitter_lib::TypedNode;
            match self {
                Self::NominalExportId { node, .. } => node.node(),
                Self::AliasExportId { node, .. } => node.node(),
                Self::ValueExportId { node, .. } => node.node(),
                Self::Import { node, .. } => node.node(),
                Self::NominalTypeDecl { node, .. } => node.node(),
                Self::FunctionDecl { node, .. } => node.node(),
                Self::ValueDecl { node, .. } => node.node(),
            }
        }
        #[inline]
        fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
            #[allow(unused_imports)]
            use type_sitter_lib::TypedNode;
            match self {
                Self::NominalExportId { node, .. } => node.node_mut(),
                Self::AliasExportId { node, .. } => node.node_mut(),
                Self::ValueExportId { node, .. } => node.node_mut(),
                Self::Import { node, .. } => node.node_mut(),
                Self::NominalTypeDecl { node, .. } => node.node_mut(),
                Self::FunctionDecl { node, .. } => node.node_mut(),
                Self::ValueDecl { node, .. } => node.node_mut(),
            }
        }
        #[inline]
        fn name(&self) -> &'static str {
            match self {
                Self::NominalExportId { .. } => "nominal.export_id",
                Self::AliasExportId { .. } => "alias.export_id",
                Self::ValueExportId { .. } => "value.export_id",
                Self::Import { .. } => "import",
                Self::NominalTypeDecl { .. } => "nominal_type.decl",
                Self::FunctionDecl { .. } => "function.decl",
                Self::ValueDecl { .. } => "value.decl",
            }
        }
        #[inline]
        fn index(&self) -> usize {
            match self {
                Self::NominalExportId { .. } => 0usize,
                Self::AliasExportId { .. } => 1usize,
                Self::ValueExportId { .. } => 2usize,
                Self::Import { .. } => 3usize,
                Self::NominalTypeDecl { .. } => 4usize,
                Self::FunctionDecl { .. } => 5usize,
                Self::ValueDecl { .. } => 6usize,
            }
        }
    }
    #[allow(non_upper_case_globals)]
    static __ProgramToExtractNominalType__: type_sitter_lib::gen_internal::TypedQueryOnceBox<
        tree_sitter::Query,
    > = type_sitter_lib::gen_internal::TypedQueryOnceBox::new();
    #[allow(non_snake_case)]
    fn __Mk__ProgramToExtractNominalType() -> Box<tree_sitter::Query> {
        # [allow (unused_mut)] let mut query = tree_sitter :: Query :: new (tree_sitter_nominalscript :: language_nominalscript () , "(program\n    (nominal_type_declaration\n        (nominal_supertypes ((_) @nominal_type .))))\n") . expect ("query parsed at compile-time but failed at runtime. Is the language 'tree_sitter_nominalscript' correct, and did you use the same tree-sitter / tree_sitter_nominalscript version?") ;
        Box::new(query)
    }
    #[doc = "Typed version of the query:\n\n```sexp\n(program\n    (nominal_type_declaration\n        (nominal_supertypes ((_) @nominal_type .))))\n\n```"]
    #[allow(non_camel_case_types)]
    #[derive(Debug, Clone, Copy)]
    pub struct ProgramToExtractNominalType;
    #[doc = "Matches returned by a query cursor running the query [ProgramToExtractNominalType]:\n\n```sexp\n(program\n    (nominal_type_declaration\n        (nominal_supertypes ((_) @nominal_type .))))\n\n```"]
    #[allow(unused, non_camel_case_types)]
    pub type ProgramToExtractNominalTypeMatches<'cursor, 'tree> =
        type_sitter_lib::TypedQueryMatches<'cursor, 'tree, ProgramToExtractNominalType>;
    #[doc = "Captures returned by a query cursor running the query [ProgramToExtractNominalType]:\n\n```sexp\n(program\n    (nominal_type_declaration\n        (nominal_supertypes ((_) @nominal_type .))))\n\n```"]
    #[allow(unused, non_camel_case_types)]
    pub type ProgramToExtractNominalTypeCaptures<'cursor, 'tree> =
        type_sitter_lib::TypedQueryCaptures<'cursor, 'tree, ProgramToExtractNominalType>;
    #[doc = "A match returned by the query [ProgramToExtractNominalType]:\n\n```sexp\n(program\n    (nominal_type_declaration\n        (nominal_supertypes ((_) @nominal_type .))))\n\n```"]
    pub struct ProgramToExtractNominalTypeMatch<'cursor, 'tree> {
        match_: tree_sitter::QueryMatch<'cursor, 'tree>,
        tree: &'tree yak_sitter::Tree,
    }
    #[doc = "A capture returned by the query [ProgramToExtractNominalType]:\n\n```sexp\n(program\n    (nominal_type_declaration\n        (nominal_supertypes ((_) @nominal_type .))))\n\n```"]
    pub enum ProgramToExtractNominalTypeCapture<'cursor, 'tree> {
        #[doc = "A `nominal_type` ([type_sitter_lib::UntypedNamedNode])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(_) @nominal_type"]
        #[doc = "```"]
        NominalType {
            node: type_sitter_lib::UntypedNamedNode<'tree>,
            match_: Option<ProgramToExtractNominalTypeMatch<'cursor, 'tree>>,
        },
    }
    #[automatically_derived]
    impl type_sitter_lib::TypedQuery for ProgramToExtractNominalType {
        type Match<'cursor, 'tree: 'cursor> = ProgramToExtractNominalTypeMatch<'cursor, 'tree>;
        type Capture<'cursor, 'tree: 'cursor> = ProgramToExtractNominalTypeCapture<'cursor, 'tree>;
        fn query_str(&self) -> &'static str {
            "(program\n    (nominal_type_declaration\n        (nominal_supertypes ((_) @nominal_type .))))\n"
        }
        fn query(&self) -> &'static tree_sitter::Query {
            __ProgramToExtractNominalType__.get_or_init(__Mk__ProgramToExtractNominalType)
        }
        #[inline]
        unsafe fn wrap_match<'cursor, 'tree>(
            &self,
            match_: tree_sitter::QueryMatch<'cursor, 'tree>,
            tree: &'tree yak_sitter::Tree,
        ) -> Self::Match<'cursor, 'tree> {
            Self::Match { match_, tree }
        }
        #[inline]
        unsafe fn wrap_capture<'cursor, 'tree>(
            &self,
            capture: tree_sitter::QueryCapture<'tree>,
            match_: Option<Self::Match<'cursor, 'tree>>,
            tree: &'tree yak_sitter::Tree,
        ) -> Self::Capture<'cursor, 'tree> {
            match capture.index as usize {
                0usize => Self::Capture::NominalType {
                    node:
                        <type_sitter_lib::UntypedNamedNode<'tree> as type_sitter_lib::TypedNode<
                            'tree,
                        >>::from_node_unchecked(unsafe {
                            yak_sitter::Node::new(capture.node, tree)
                        }),
                    match_,
                },
                capture_index => unreachable!("Invalid capture index: {}", capture_index),
            }
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> ProgramToExtractNominalTypeMatch<'cursor, 'tree> {
        #[doc = "Returns an iterator over the nodes captured by `nominal_type` ([type_sitter_lib::UntypedNamedNode])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(_) @nominal_type"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn nominal_type(&self) -> type_sitter_lib::UntypedNamedNode<'tree> {
            let result = {
                [0u32]
                    .into_iter()
                    .flat_map(|i| self.match_.nodes_for_capture_index(i))
                    .map(|n| unsafe {
                        <type_sitter_lib::UntypedNamedNode<'tree> as type_sitter_lib::TypedNode<
                            'tree,
                        >>::from_node_unchecked(yak_sitter::Node::new(
                            n, self.tree,
                        ))
                    })
            }
            .next()
            .expect("one quantifier returned nothing");
            debug_assert ! ({ [0u32] . into_iter () . flat_map (| i | self . match_ . nodes_for_capture_index (i)) . map (| n | unsafe { < type_sitter_lib :: UntypedNamedNode < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (yak_sitter :: Node :: new (n , self . tree)) }) } . next () . is_none () , "one quantifier returned more than one item");
            result
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> std::fmt::Debug for ProgramToExtractNominalTypeMatch<'cursor, 'tree> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct(stringify!(ProgramToExtractNominalTypeMatch))
                .field("match_", &self.match_)
                .finish()
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> type_sitter_lib::TypedQueryMatch<'cursor, 'tree>
        for ProgramToExtractNominalTypeMatch<'cursor, 'tree>
    {
        type Query = ProgramToExtractNominalType;
        #[inline]
        fn query(&self) -> &'cursor Self::Query {
            &ProgramToExtractNominalType
        }
        #[inline]
        fn tree(&self) -> &'tree yak_sitter::Tree {
            self.tree
        }
        #[inline]
        fn raw(&self) -> &tree_sitter::QueryMatch<'cursor, 'tree> {
            &self.match_
        }
        #[inline]
        fn into_raw(self) -> tree_sitter::QueryMatch<'cursor, 'tree> {
            self.match_
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> ProgramToExtractNominalTypeCapture<'cursor, 'tree> {
        #[doc = "Try to interpret this capture as a `nominal_type` ([type_sitter_lib::UntypedNamedNode])"]
        #[doc = ""]
        #[doc = "The full capture including pattern(s) is:"]
        #[doc = "```sexp"]
        #[doc = "(_) @nominal_type"]
        #[doc = "```"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn nominal_type(&self) -> Option<&type_sitter_lib::UntypedNamedNode<'tree>> {
            match self {
                Self::NominalType { node, .. } => Some(node),
                #[allow(unreachable_patterns)]
                _ => None,
            }
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> std::fmt::Debug for ProgramToExtractNominalTypeCapture<'cursor, 'tree> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::NominalType { node, .. } => f
                    .debug_struct(concat!(
                        stringify!(ProgramToExtractNominalTypeCapture),
                        "::",
                        stringify!(NominalType)
                    ))
                    .field("node", node)
                    .finish(),
            }
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> Clone for ProgramToExtractNominalTypeCapture<'cursor, 'tree> {
        fn clone(&self) -> Self {
            match self {
                Self::NominalType { node, .. } => Self::NominalType {
                    node: *node,
                    match_: None,
                },
            }
        }
    }
    #[automatically_derived]
    impl<'cursor, 'tree> type_sitter_lib::TypedQueryCapture<'cursor, 'tree>
        for ProgramToExtractNominalTypeCapture<'cursor, 'tree>
    {
        type Query = ProgramToExtractNominalType;
        #[inline]
        fn query(&self) -> &'cursor Self::Query {
            &ProgramToExtractNominalType
        }
        #[inline]
        fn match_(
            &self,
        ) -> Option<&<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
            match self {
                Self::NominalType { match_, .. } => match_.as_ref(),
            }
        }
        #[inline]
        fn into_match(
            self,
        ) -> Option<<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
            match self {
                Self::NominalType { match_, .. } => match_,
            }
        }
        #[inline]
        fn to_raw(&self) -> yak_sitter::QueryCapture<'static, 'tree> {
            #[allow(unused_imports)]
            use type_sitter_lib::TypedNode;
            match self {
                Self::NominalType { node, .. } => yak_sitter::QueryCapture {
                    node: *node.node(),
                    index: 0usize,
                    name: "nominal_type",
                },
            }
        }
        #[inline]
        fn node(&self) -> &yak_sitter::Node<'tree> {
            #[allow(unused_imports)]
            use type_sitter_lib::TypedNode;
            match self {
                Self::NominalType { node, .. } => node.node(),
            }
        }
        #[inline]
        fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
            #[allow(unused_imports)]
            use type_sitter_lib::TypedNode;
            match self {
                Self::NominalType { node, .. } => node.node_mut(),
            }
        }
        #[inline]
        fn name(&self) -> &'static str {
            match self {
                Self::NominalType { .. } => "nominal_type",
            }
        }
        #[inline]
        fn index(&self) -> usize {
            match self {
                Self::NominalType { .. } => 0usize,
            }
        }
    }
    pub mod anon_unions {
        #[allow(unused_imports)]
        use crate::syntax::nodes::*;
        #[doc = "one of `{function_declaration | function_signature | generator_function_declaration}`:\n- [FunctionDeclaration]\n- [FunctionSignature]\n- [GeneratorFunctionDeclaration]"]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[allow(non_camel_case_types)]
        pub enum FunctionDecl<'tree> {
            FunctionDeclaration(FunctionDeclaration<'tree>),
            FunctionSignature(FunctionSignature<'tree>),
            GeneratorFunctionDeclaration(GeneratorFunctionDeclaration<'tree>),
        }
        #[automatically_derived]
        impl<'tree> FunctionDecl<'tree> {
            #[doc = "Returns the node if it is of kind `function_declaration` ([FunctionDeclaration]), otherwise returns None"]
            #[inline]
            #[allow(unused, non_snake_case)]
            pub fn function_declaration(self) -> Option<FunctionDeclaration<'tree>> {
                match self {
                    Self::FunctionDeclaration(x) => Some(x),
                    _ => None,
                }
            }
            #[doc = "Returns the node if it is of kind `function_signature` ([FunctionSignature]), otherwise returns None"]
            #[inline]
            #[allow(unused, non_snake_case)]
            pub fn function_signature(self) -> Option<FunctionSignature<'tree>> {
                match self {
                    Self::FunctionSignature(x) => Some(x),
                    _ => None,
                }
            }
            #[doc = "Returns the node if it is of kind `generator_function_declaration` ([GeneratorFunctionDeclaration]), otherwise returns None"]
            #[inline]
            #[allow(unused, non_snake_case)]
            pub fn generator_function_declaration(
                self,
            ) -> Option<GeneratorFunctionDeclaration<'tree>> {
                match self {
                    Self::GeneratorFunctionDeclaration(x) => Some(x),
                    _ => None,
                }
            }
        }
        #[automatically_derived]
        impl<'tree> TryFrom<yak_sitter::Node<'tree>> for FunctionDecl<'tree> {
            type Error = type_sitter_lib::IncorrectKind<'tree>;
            #[inline]
            fn try_from(node: yak_sitter::Node<'tree>) -> Result<Self, Self::Error> {
                match node.kind() {
                    "function_declaration" => Ok(unsafe {
                        Self :: FunctionDeclaration (< FunctionDeclaration < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                    }),
                    "function_signature" => Ok(unsafe {
                        Self :: FunctionSignature (< FunctionSignature < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                    }),
                    "generator_function_declaration" => {
                        Ok(unsafe {
                            Self :: GeneratorFunctionDeclaration (< GeneratorFunctionDeclaration < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                        })
                    }
                    _ => Err(type_sitter_lib::IncorrectKind {
                        node,
                        kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
                    }),
                }
            }
        }
        #[automatically_derived]
        impl<'tree> type_sitter_lib::TypedNode<'tree> for FunctionDecl<'tree> {
            const KIND: &'static str =
                "{function_declaration | function_signature | generator_function_declaration}";
            #[inline]
            fn node(&self) -> &yak_sitter::Node<'tree> {
                match self {
                    Self::FunctionDeclaration(x) => x.node(),
                    Self::FunctionSignature(x) => x.node(),
                    Self::GeneratorFunctionDeclaration(x) => x.node(),
                }
            }
            #[inline]
            fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
                match self {
                    Self::FunctionDeclaration(x) => x.node_mut(),
                    Self::FunctionSignature(x) => x.node_mut(),
                    Self::GeneratorFunctionDeclaration(x) => x.node_mut(),
                }
            }
            #[inline]
            fn into_node(self) -> yak_sitter::Node<'tree> {
                match self {
                    Self::FunctionDeclaration(x) => x.into_node(),
                    Self::FunctionSignature(x) => x.into_node(),
                    Self::GeneratorFunctionDeclaration(x) => x.into_node(),
                }
            }
        }
    }
}
#[allow(non_upper_case_globals)]
static __Injections__: type_sitter_lib::gen_internal::TypedQueryOnceBox<tree_sitter::Query> =
    type_sitter_lib::gen_internal::TypedQueryOnceBox::new();
#[allow(non_snake_case)]
fn __Mk__Injections() -> Box<tree_sitter::Query> {
    # [allow (unused_mut)] let mut query = tree_sitter :: Query :: new (tree_sitter_nominalscript :: language_nominalscript () , "; inherits: typescript\n") . expect ("query parsed at compile-time but failed at runtime. Is the language 'tree_sitter_nominalscript' correct, and did you use the same tree-sitter / tree_sitter_nominalscript version?") ;
    Box::new(query)
}
#[doc = "Typed version of the query:\n\n```sexp\n; inherits: typescript\n\n```"]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy)]
pub struct Injections;
#[doc = "Matches returned by a query cursor running the query [Injections]:\n\n```sexp\n; inherits: typescript\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type InjectionsMatches<'cursor, 'tree> =
    type_sitter_lib::TypedQueryMatches<'cursor, 'tree, Injections>;
#[doc = "Captures returned by a query cursor running the query [Injections]:\n\n```sexp\n; inherits: typescript\n\n```"]
#[allow(unused, non_camel_case_types)]
pub type InjectionsCaptures<'cursor, 'tree> =
    type_sitter_lib::TypedQueryCaptures<'cursor, 'tree, Injections>;
#[doc = "A match returned by the query [Injections]:\n\n```sexp\n; inherits: typescript\n\n```"]
pub struct InjectionsMatch<'cursor, 'tree> {
    match_: tree_sitter::QueryMatch<'cursor, 'tree>,
    tree: &'tree yak_sitter::Tree,
}
#[doc = "A capture returned by the query [Injections]:\n\n```sexp\n; inherits: typescript\n\n```"]
pub enum InjectionsCapture {}
#[automatically_derived]
impl type_sitter_lib::TypedQuery for Injections {
    type Match<'cursor, 'tree: 'cursor> = InjectionsMatch<'cursor, 'tree>;
    type Capture<'cursor, 'tree: 'cursor> = InjectionsCapture;
    fn query_str(&self) -> &'static str {
        "; inherits: typescript\n"
    }
    fn query(&self) -> &'static tree_sitter::Query {
        __Injections__.get_or_init(__Mk__Injections)
    }
    #[inline]
    unsafe fn wrap_match<'cursor, 'tree>(
        &self,
        match_: tree_sitter::QueryMatch<'cursor, 'tree>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Match<'cursor, 'tree> {
        Self::Match { match_, tree }
    }
    #[inline]
    unsafe fn wrap_capture<'cursor, 'tree>(
        &self,
        capture: tree_sitter::QueryCapture<'tree>,
        match_: Option<Self::Match<'cursor, 'tree>>,
        tree: &'tree yak_sitter::Tree,
    ) -> Self::Capture<'cursor, 'tree> {
        match capture.index as usize {
            capture_index => unreachable!("Invalid capture index: {}", capture_index),
        }
    }
}
#[automatically_derived]
impl<'cursor, 'tree> InjectionsMatch<'cursor, 'tree> {}
#[automatically_derived]
impl<'cursor, 'tree> std::fmt::Debug for InjectionsMatch<'cursor, 'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(stringify!(InjectionsMatch))
            .field("match_", &self.match_)
            .finish()
    }
}
#[automatically_derived]
impl<'cursor, 'tree> type_sitter_lib::TypedQueryMatch<'cursor, 'tree>
    for InjectionsMatch<'cursor, 'tree>
{
    type Query = Injections;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Injections
    }
    #[inline]
    fn tree(&self) -> &'tree yak_sitter::Tree {
        self.tree
    }
    #[inline]
    fn raw(&self) -> &tree_sitter::QueryMatch<'cursor, 'tree> {
        &self.match_
    }
    #[inline]
    fn into_raw(self) -> tree_sitter::QueryMatch<'cursor, 'tree> {
        self.match_
    }
}
#[automatically_derived]
impl InjectionsCapture {}
#[automatically_derived]
impl std::fmt::Debug for InjectionsCapture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {}
    }
}
#[automatically_derived]
impl<'cursor, 'tree> Clone for InjectionsCapture {
    fn clone(&self) -> Self {
        match self {}
    }
}
#[automatically_derived]
impl<'cursor, 'tree: 'cursor> type_sitter_lib::TypedQueryCapture<'cursor, 'tree>
    for InjectionsCapture
{
    type Query = Injections;
    #[inline]
    fn query(&self) -> &'cursor Self::Query {
        &Injections
    }
    #[inline]
    fn match_(
        &self,
    ) -> Option<&<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {}
    }
    #[inline]
    fn into_match(
        self,
    ) -> Option<<Self::Query as type_sitter_lib::TypedQuery>::Match<'cursor, 'tree>> {
        match self {}
    }
    #[inline]
    fn to_raw(&self) -> yak_sitter::QueryCapture<'static, 'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {}
    }
    #[inline]
    fn node(&self) -> &yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {}
    }
    #[inline]
    fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
        #[allow(unused_imports)]
        use type_sitter_lib::TypedNode;
        match self {}
    }
    #[inline]
    fn name(&self) -> &'static str {
        match self {}
    }
    #[inline]
    fn index(&self) -> usize {
        match self {}
    }
}
pub mod anon_unions {
    #[allow(unused_imports)]
    use crate::syntax::nodes::*;
    #[doc = "one of `{identifier | nominal_type_identifier | property_identifier | type_identifier}`:\n- [Identifier]\n- [NominalTypeIdentifier]\n- [PropertyIdentifier]\n- [TypeIdentifier]"]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Name<'tree> {
        Identifier(Identifier<'tree>),
        NominalTypeIdentifier(NominalTypeIdentifier<'tree>),
        PropertyIdentifier(PropertyIdentifier<'tree>),
        TypeIdentifier(TypeIdentifier<'tree>),
    }
    #[automatically_derived]
    impl<'tree> Name<'tree> {
        #[doc = "Returns the node if it is of kind `identifier` ([Identifier]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn identifier(self) -> Option<Identifier<'tree>> {
            match self {
                Self::Identifier(x) => Some(x),
                _ => None,
            }
        }
        #[doc = "Returns the node if it is of kind `nominal_type_identifier` ([NominalTypeIdentifier]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn nominal_type_identifier(self) -> Option<NominalTypeIdentifier<'tree>> {
            match self {
                Self::NominalTypeIdentifier(x) => Some(x),
                _ => None,
            }
        }
        #[doc = "Returns the node if it is of kind `property_identifier` ([PropertyIdentifier]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn property_identifier(self) -> Option<PropertyIdentifier<'tree>> {
            match self {
                Self::PropertyIdentifier(x) => Some(x),
                _ => None,
            }
        }
        #[doc = "Returns the node if it is of kind `type_identifier` ([TypeIdentifier]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn type_identifier(self) -> Option<TypeIdentifier<'tree>> {
            match self {
                Self::TypeIdentifier(x) => Some(x),
                _ => None,
            }
        }
    }
    #[automatically_derived]
    impl<'tree> TryFrom<yak_sitter::Node<'tree>> for Name<'tree> {
        type Error = type_sitter_lib::IncorrectKind<'tree>;
        #[inline]
        fn try_from(node: yak_sitter::Node<'tree>) -> Result<Self, Self::Error> {
            match node.kind() {
                "identifier" => Ok(unsafe {
                    Self :: Identifier (< Identifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                }),
                "nominal_type_identifier" => Ok(unsafe {
                    Self :: NominalTypeIdentifier (< NominalTypeIdentifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                }),
                "property_identifier" => Ok(unsafe {
                    Self :: PropertyIdentifier (< PropertyIdentifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                }),
                "type_identifier" => {
                    Ok(unsafe {
                        Self :: TypeIdentifier (< TypeIdentifier < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                    })
                }
                _ => Err(type_sitter_lib::IncorrectKind {
                    node,
                    kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
                }),
            }
        }
    }
    #[automatically_derived]
    impl<'tree> type_sitter_lib::TypedNode<'tree> for Name<'tree> {
        const KIND: &'static str =
            "{identifier | nominal_type_identifier | property_identifier | type_identifier}";
        #[inline]
        fn node(&self) -> &yak_sitter::Node<'tree> {
            match self {
                Self::Identifier(x) => x.node(),
                Self::NominalTypeIdentifier(x) => x.node(),
                Self::PropertyIdentifier(x) => x.node(),
                Self::TypeIdentifier(x) => x.node(),
            }
        }
        #[inline]
        fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
            match self {
                Self::Identifier(x) => x.node_mut(),
                Self::NominalTypeIdentifier(x) => x.node_mut(),
                Self::PropertyIdentifier(x) => x.node_mut(),
                Self::TypeIdentifier(x) => x.node_mut(),
            }
        }
        #[inline]
        fn into_node(self) -> yak_sitter::Node<'tree> {
            match self {
                Self::Identifier(x) => x.into_node(),
                Self::NominalTypeIdentifier(x) => x.into_node(),
                Self::PropertyIdentifier(x) => x.into_node(),
                Self::TypeIdentifier(x) => x.into_node(),
            }
        }
    }
    #[doc = "one of `{abstract_method_signature | method_signature}`:\n- [AbstractMethodSignature]\n- [MethodSignature]"]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum DefinitionMethod<'tree> {
        AbstractMethodSignature(AbstractMethodSignature<'tree>),
        MethodSignature(MethodSignature<'tree>),
    }
    #[automatically_derived]
    impl<'tree> DefinitionMethod<'tree> {
        #[doc = "Returns the node if it is of kind `abstract_method_signature` ([AbstractMethodSignature]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn abstract_method_signature(self) -> Option<AbstractMethodSignature<'tree>> {
            match self {
                Self::AbstractMethodSignature(x) => Some(x),
                _ => None,
            }
        }
        #[doc = "Returns the node if it is of kind `method_signature` ([MethodSignature]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn method_signature(self) -> Option<MethodSignature<'tree>> {
            match self {
                Self::MethodSignature(x) => Some(x),
                _ => None,
            }
        }
    }
    #[automatically_derived]
    impl<'tree> TryFrom<yak_sitter::Node<'tree>> for DefinitionMethod<'tree> {
        type Error = type_sitter_lib::IncorrectKind<'tree>;
        #[inline]
        fn try_from(node: yak_sitter::Node<'tree>) -> Result<Self, Self::Error> {
            match node.kind() {
                "abstract_method_signature" => Ok(unsafe {
                    Self :: AbstractMethodSignature (< AbstractMethodSignature < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                }),
                "method_signature" => {
                    Ok(unsafe {
                        Self :: MethodSignature (< MethodSignature < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                    })
                }
                _ => Err(type_sitter_lib::IncorrectKind {
                    node,
                    kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
                }),
            }
        }
    }
    #[automatically_derived]
    impl<'tree> type_sitter_lib::TypedNode<'tree> for DefinitionMethod<'tree> {
        const KIND: &'static str = "{abstract_method_signature | method_signature}";
        #[inline]
        fn node(&self) -> &yak_sitter::Node<'tree> {
            match self {
                Self::AbstractMethodSignature(x) => x.node(),
                Self::MethodSignature(x) => x.node(),
            }
        }
        #[inline]
        fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
            match self {
                Self::AbstractMethodSignature(x) => x.node_mut(),
                Self::MethodSignature(x) => x.node_mut(),
            }
        }
        #[inline]
        fn into_node(self) -> yak_sitter::Node<'tree> {
            match self {
                Self::AbstractMethodSignature(x) => x.into_node(),
                Self::MethodSignature(x) => x.into_node(),
            }
        }
    }
    #[doc = "one of `{nominal_type_declaration | nominal_type_guard}`:\n- [NominalTypeDeclaration]\n- [NominalTypeGuard]"]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum IndentBegin<'tree> {
        NominalTypeDeclaration(NominalTypeDeclaration<'tree>),
        NominalTypeGuard(NominalTypeGuard<'tree>),
    }
    #[automatically_derived]
    impl<'tree> IndentBegin<'tree> {
        #[doc = "Returns the node if it is of kind `nominal_type_declaration` ([NominalTypeDeclaration]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn nominal_type_declaration(self) -> Option<NominalTypeDeclaration<'tree>> {
            match self {
                Self::NominalTypeDeclaration(x) => Some(x),
                _ => None,
            }
        }
        #[doc = "Returns the node if it is of kind `nominal_type_guard` ([NominalTypeGuard]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn nominal_type_guard(self) -> Option<NominalTypeGuard<'tree>> {
            match self {
                Self::NominalTypeGuard(x) => Some(x),
                _ => None,
            }
        }
    }
    #[automatically_derived]
    impl<'tree> TryFrom<yak_sitter::Node<'tree>> for IndentBegin<'tree> {
        type Error = type_sitter_lib::IncorrectKind<'tree>;
        #[inline]
        fn try_from(node: yak_sitter::Node<'tree>) -> Result<Self, Self::Error> {
            match node.kind() {
                "nominal_type_declaration" => Ok(unsafe {
                    Self :: NominalTypeDeclaration (< NominalTypeDeclaration < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                }),
                "nominal_type_guard" => Ok(unsafe {
                    Self :: NominalTypeGuard (< NominalTypeGuard < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                }),
                _ => Err(type_sitter_lib::IncorrectKind {
                    node,
                    kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
                }),
            }
        }
    }
    #[automatically_derived]
    impl<'tree> type_sitter_lib::TypedNode<'tree> for IndentBegin<'tree> {
        const KIND: &'static str = "{nominal_type_declaration | nominal_type_guard}";
        #[inline]
        fn node(&self) -> &yak_sitter::Node<'tree> {
            match self {
                Self::NominalTypeDeclaration(x) => x.node(),
                Self::NominalTypeGuard(x) => x.node(),
            }
        }
        #[inline]
        fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
            match self {
                Self::NominalTypeDeclaration(x) => x.node_mut(),
                Self::NominalTypeGuard(x) => x.node_mut(),
            }
        }
        #[inline]
        fn into_node(self) -> yak_sitter::Node<'tree> {
            match self {
                Self::NominalTypeDeclaration(x) => x.into_node(),
                Self::NominalTypeGuard(x) => x.into_node(),
            }
        }
    }
    #[doc = "one of `{; | ?}`:\n- [symbols::Semicolon]\n- [symbols::Question]"]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum PunctuationDelimiter<'tree> {
        Semicolon(symbols::Semicolon<'tree>),
        Question(symbols::Question<'tree>),
    }
    #[automatically_derived]
    impl<'tree> PunctuationDelimiter<'tree> {
        #[doc = "Returns the node if it is of kind `;` ([symbols::Semicolon]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn semicolon(self) -> Option<symbols::Semicolon<'tree>> {
            match self {
                Self::Semicolon(x) => Some(x),
                _ => None,
            }
        }
        #[doc = "Returns the node if it is of kind `?` ([symbols::Question]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn question(self) -> Option<symbols::Question<'tree>> {
            match self {
                Self::Question(x) => Some(x),
                _ => None,
            }
        }
    }
    #[automatically_derived]
    impl<'tree> TryFrom<yak_sitter::Node<'tree>> for PunctuationDelimiter<'tree> {
        type Error = type_sitter_lib::IncorrectKind<'tree>;
        #[inline]
        fn try_from(node: yak_sitter::Node<'tree>) -> Result<Self, Self::Error> {
            match node.kind() {
                ";" => Ok(unsafe {
                    Self::Semicolon(<symbols::Semicolon<'tree> as type_sitter_lib::TypedNode<
                        'tree,
                    >>::from_node_unchecked(node))
                }),
                "?" => Ok(unsafe {
                    Self::Question(<symbols::Question<'tree> as type_sitter_lib::TypedNode<
                        'tree,
                    >>::from_node_unchecked(node))
                }),
                _ => Err(type_sitter_lib::IncorrectKind {
                    node,
                    kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
                }),
            }
        }
    }
    #[automatically_derived]
    impl<'tree> type_sitter_lib::TypedNode<'tree> for PunctuationDelimiter<'tree> {
        const KIND: &'static str = "{; | ?}";
        #[inline]
        fn node(&self) -> &yak_sitter::Node<'tree> {
            match self {
                Self::Semicolon(x) => x.node(),
                Self::Question(x) => x.node(),
            }
        }
        #[inline]
        fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
            match self {
                Self::Semicolon(x) => x.node_mut(),
                Self::Question(x) => x.node_mut(),
            }
        }
        #[inline]
        fn into_node(self) -> yak_sitter::Node<'tree> {
            match self {
                Self::Semicolon(x) => x.into_node(),
                Self::Question(x) => x.into_node(),
            }
        }
    }
    #[doc = "one of `{< | >}`:\n- [symbols::Lt]\n- [symbols::Gt]"]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum PunctuationBracket<'tree> {
        Lt(symbols::Lt<'tree>),
        Gt(symbols::Gt<'tree>),
    }
    #[automatically_derived]
    impl<'tree> PunctuationBracket<'tree> {
        #[doc = "Returns the node if it is of kind `<` ([symbols::Lt]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn lt(self) -> Option<symbols::Lt<'tree>> {
            match self {
                Self::Lt(x) => Some(x),
                _ => None,
            }
        }
        #[doc = "Returns the node if it is of kind `>` ([symbols::Gt]), otherwise returns None"]
        #[inline]
        #[allow(unused, non_snake_case)]
        pub fn gt(self) -> Option<symbols::Gt<'tree>> {
            match self {
                Self::Gt(x) => Some(x),
                _ => None,
            }
        }
    }
    #[automatically_derived]
    impl<'tree> TryFrom<yak_sitter::Node<'tree>> for PunctuationBracket<'tree> {
        type Error = type_sitter_lib::IncorrectKind<'tree>;
        #[inline]
        fn try_from(node: yak_sitter::Node<'tree>) -> Result<Self, Self::Error> {
            match node.kind() {
                "<" => Ok(unsafe {
                    Self :: Lt (< symbols :: Lt < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                }),
                ">" => Ok(unsafe {
                    Self :: Gt (< symbols :: Gt < 'tree > as type_sitter_lib :: TypedNode < 'tree >> :: from_node_unchecked (node))
                }),
                _ => Err(type_sitter_lib::IncorrectKind {
                    node,
                    kind: <Self as type_sitter_lib::TypedNode<'tree>>::KIND,
                }),
            }
        }
    }
    #[automatically_derived]
    impl<'tree> type_sitter_lib::TypedNode<'tree> for PunctuationBracket<'tree> {
        const KIND: &'static str = "{< | >}";
        #[inline]
        fn node(&self) -> &yak_sitter::Node<'tree> {
            match self {
                Self::Lt(x) => x.node(),
                Self::Gt(x) => x.node(),
            }
        }
        #[inline]
        fn node_mut(&mut self) -> &mut yak_sitter::Node<'tree> {
            match self {
                Self::Lt(x) => x.node_mut(),
                Self::Gt(x) => x.node_mut(),
            }
        }
        #[inline]
        fn into_node(self) -> yak_sitter::Node<'tree> {
            match self {
                Self::Lt(x) => x.into_node(),
                Self::Gt(x) => x.into_node(),
            }
        }
    }
}
