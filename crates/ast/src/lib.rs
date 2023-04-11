#![allow(dead_code)]

mod ast;
pub use ast::{Ast, NodeId};
use token::{NumberId, StringId};
mod r#macro;

pub trait Node {
    fn from_node<'a>(node: &'a AstNode) -> &'a Self;
    fn from_node_mut<'a>(node: &'a mut AstNode) -> &'a mut Self;
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
    stmt: NodeId<T>,
    next: Option<NodeId<List<T>>>,
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
    Ident(StringId),
    Boolean(bool),
    Null,
    Object,
    Array,
}
