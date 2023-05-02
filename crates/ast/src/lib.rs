#![allow(dead_code)]

mod ast;
pub use ast::{Ast as GenAst, List, ListId, NodeId};
use token::{NumberId, StringId};
mod r#macro;

pub type AstStorage = (Vec<Expr>, Vec<PrimeExpr>, Vec<PropertyDefinition>);

pub type Ast = GenAst<AstStorage>;

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct CaseList {
    /// Case expression, it is the default case if there is no expression.
    expr: Option<NodeId<Expr>>,
    body: Option<ListId<Stmt>>,
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
        list: ListId<Stmt>,
    },
    VariableDecl {
        kind: VariableKind,
        decl: ListId<Binding>,
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
        //cased: NodeId<CaseS>,
    },
    Try {
        block: ListId<Stmt>,
        catch: Option<NodeId<Expr>>,
        finally: Option<ListId<Stmt>>,
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
pub enum BaseOp {
    NullCoalessing,
    TenaryNull,
    Or,
    And,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    ShiftLeft,
    ShiftRight,
    ShiftRightUnsigned,
    InstanceOf,
    In,
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum AssignOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    ShiftLeft,
    ShiftRight,
    ShiftRightUnsigned,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum BinaryOp {
    Base(BaseOp),
    Assign(AssignOp),
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum PostfixOp {
    AddOne,
    SubOne,
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum PrefixOp {
    AddOne,
    SubOne,
    Plus,
    Minus,
    Not,
    BitwiseNot,
    New,
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
        expr: NodeId<Expr>,
    },
    // Postfix like operators with internal values
    // Are inlined into Expr in order to keep the Expr size smaller.
    Index {
        index: NodeId<Expr>,
        expr: NodeId<Expr>,
    },
    Dot {
        ident: StringId,
        expr: NodeId<Expr>,
    },
    Call {
        params: ListId<Expr>,
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
    Array(ArrayLiteral),
    This,
    Covered(ListId<Expr>),
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
    Item(ListId<PropertyDefinition>),
}

#[derive(Clone, Copy, PartialEq)]
pub struct ArrayLiteral {
    pub expr: Option<NodeId<Expr>>,
    pub is_spread: bool,
    pub next: Option<NodeId<ArrayLiteral>>,
}
