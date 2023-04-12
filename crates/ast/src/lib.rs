#![allow(dead_code)]

mod ast;
pub use ast::{Ast, NodeId};
use token::{NumberId, StringId};
mod r#macro;

pub trait Node {
    fn from_node(node: &AstNode) -> &Self;
    fn from_node_mut(node: &mut AstNode) -> &mut Self;
    fn into_node(self) -> AstNode;
}

ast_node!(
    pub enum AstNode {
        Root(Root),
        Expr(Expr),
        PrimeExpr(PrimeExpr),
        Stmt(Stmt),
        StmtList(List<Stmt>),
        Binding(Binding),
        CaseList(CaseList),
        PropertyDefinition(PropertyDefinition),
        ObjectLiteral(ObjectLiteral),
        ObjectLiteralItem(List<PropertyDefinition>),
    }
);
const SIZE_ASSERT: [u8; 16] = [0u8; std::mem::size_of::<AstNode>()];

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Root {
    Undetermined,
    Script { statements: NodeId<List<Stmt>> },
    Module { statements: NodeId<List<Stmt>> },
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct List<T> {
    pub item: NodeId<T>,
    pub next: Option<NodeId<List<T>>>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct InlineList<T> {
    pub item: T,
    pub next: Option<NodeId<List<T>>>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct CaseList {
    /// Case expression, it is the default case if there is no expression.
    expr: Option<NodeId<Expr>>,
    body: Option<NodeId<List<Stmt>>>,
    next: Option<NodeId<CaseList>>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum VariableKind {
    Const,
    Var,
    Let,
}

pub enum Binding {
    Ident {
        label: StringId,
        initializer: Option<NodeId<Expr>>,
    },
    Object {},
    Array {},
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Stmt {
    Block {
        list: NodeId<List<Stmt>>,
    },
    VariableDecl {
        kind: VariableKind,
        decl: NodeId<List<Binding>>,
    },
    Empty,
    Expr {
        expr: NodeId<Expr>,
    },
    DoWhile {
        body: NodeId<Stmt>,
        expr: NodeId<Expr>,
    },
    If {
        cond: NodeId<Expr>,
        body: NodeId<Stmt>,
        r#else: Option<NodeId<Stmt>>,
    },
    While {
        cond: NodeId<Expr>,
        body: NodeId<Stmt>,
    },
    For {
        head: NodeId<Expr>,
        body: NodeId<Stmt>,
    },
    Switch {
        cond: NodeId<Expr>,
        cased: NodeId<List<CaseList>>,
    },
    Try {
        block: NodeId<List<Stmt>>,
        catch: Option<NodeId<Expr>>,
        finally: Option<NodeId<List<Stmt>>>,
    },
    Break {
        label: Option<StringId>,
    },
    Continue {
        label: Option<StringId>,
    },
    Throw {
        expr: NodeId<Expr>,
    },
    Return {
        expr: Option<NodeId<Expr>>,
    },
    Labeled {
        label: StringId,
        stmt: NodeId<Stmt>,
    },
    Debugger,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum PostfixOp {
    AddOne,
    SubOne,
    Index,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum PrefixOp {
    AddOne,
    SubOne,
    Plus,
    Minus,
    Not,
    BinaryNot,
    Delete,
    Void,
    TypeOf,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Expr {
    Binary {
        op: BinaryOp,
        left: NodeId<Expr>,
        right: NodeId<Expr>,
    },
    Prefix {
        op: PrefixOp,
        expr: NodeId<Expr>,
    },
    Postfix {
        op: PostfixOp,
        index: Option<NodeId<Expr>>,
        expr: NodeId<Expr>,
    },
    Prime {
        expr: NodeId<PrimeExpr>,
    },
}

#[derive(Clone, Copy, PartialEq)]
pub enum PrimeExpr {
    Number(NumberId),
    String(StringId),
    Regex(StringId),
    Ident(StringId),
    Boolean(bool),
    Null,
    Object(ObjectLiteral),
    Array,
    This,
    Covered(NodeId<Expr>),
}

#[derive(Clone, Copy, PartialEq)]
pub enum PropertyDefinition {
    Ident(StringId),
    Covered {
        ident: StringId,
        initializer: NodeId<Expr>,
    },
    Define {
        property: PropertyName,
        expr: NodeId<Expr>,
    },
    Method {},
    Rest(NodeId<Expr>),
}

#[derive(Clone, Copy, PartialEq)]
pub enum PropertyName {
    Ident(StringId),
    String(StringId),
    Number(NumberId),
    Computed(NodeId<Expr>),
}

#[derive(Clone, Copy, PartialEq)]
pub enum ObjectLiteral {
    Empty,
    Item(NodeId<List<PropertyDefinition>>),
}
