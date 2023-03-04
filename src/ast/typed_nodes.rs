use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::types::FatType;
use crate::ast::tree_sitter::TSNode;
use crate::diagnostics::FileLogger;
use crate::misc::lazy::{Lazy, LazyError};

pub trait AstNode<'tree> {
    const NODE_KINDS: &'static [&'static str];

    fn _new(node: TSNode<'tree>) -> Self;

    fn new(node: TSNode<'tree>) -> Self where Self: Sized {
        assert!(Self::NODE_KINDS.contains(&node.kind()), "Node kind not in NODE_KINDS");
        Self::_new(node)
    }

    fn node(&self) -> TSNode<'tree>;
}

macro_rules! impl_ast_node_common {
    ($AstNode:ident, [ $($NODE_KIND:literal),* ]) => {
        impl<'tree> AstNode<'tree> for $AstNode<'tree> {
            const NODE_KINDS: &'static [&'static str] = &[$($NODE_KIND),*];

            fn node(&self) -> TSNode<'tree> {
                self.node
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

// TODO: Should this be a trait?
pub trait AstBinding<'tree>: TypedAstNode<'tree> {
    fn name(&self) -> &ValueIdent<'tree>;
    fn uses(&self) -> &[TSNode<'tree>];
    fn add_use(&mut self, use_: TSNode<'tree>);
    fn resolve_value(&self) -> Lazy<FatType, Self::InferTypeFn>;
}

/* export abstract class AstBinding extends TypedAstNode implements Binding {
  public readonly name: ValueIdent
  abstract readonly resolveValue: LazyPromise<NominalTypeShape | null>
  private readonly _uses: TSNode[] = []

  protected constructor (
    node: TSNode,
    requiredType: string | string[] | null,
    name: TSNode
  ) {
    super(node, requiredType)
    this.name = new ValueIdent(name)
  }

  get uses (): readonly TSNode[] {
    return this._uses
  }

  addUse (use: TSNode): void {
    const { hasCmpEq } = ArrayUtil.binaryInsert(
      this._uses,
      use,
      (a, b) => a.startIndex - b.startIndex
    )
    assert(!hasCmpEq)
  }
} */

pub struct ValueIdent<'tree> {
    pub node: TSNode<'tree>,
    pub name: ValueName,
}

impl_ast_node_common!(ValueIdent, ["identifier"]);
impl<'tree> ValueIdent<'tree> {
    pub fn _new(node: TSNode<'tree>) -> Self {
        Self {
            node,
            name: ValueName::new(node.text()),
        }
    }
}

pub struct TypeIdent<'tree> {
    pub node: TSNode<'tree>,
    pub name: TypeName,
}

impl_ast_node_common!(TypeIdent, ["nominal_type_identifier"]);
impl<'tree> TypeIdent<'tree> {
    pub fn _new(node: TSNode<'tree>) -> Self {
        Self {
            node,
            name: TypeName::new(node.text()),
        }
    }
}