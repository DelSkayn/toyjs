#![feature(allocator_api)]

use bumpalo::boxed::Box;
//pub use common::bump_list::List as Vec;
pub use bumpalo::collections::Vec;
use common::interner::StringId;
use std::cmp::PartialEq;

mod symbol_table;

mod variables;
pub use variables::*;

#[derive(Debug)]
pub struct Script<'a>(pub ScopeRef<'a>, pub Vec<'a, Stmt<'a>>);

#[derive(Debug, PartialEq)]
pub enum Rest {
    BindingIdent(VariableId),
}

#[derive(Debug, PartialEq)]
pub struct Params<'a>(pub Vec<'a, VariableId>, pub Option<Rest>);

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    Empty,
    Let(VariableId, Option<Expr<'a>>),
    Var(VariableId, Option<Expr<'a>>),
    Const(VariableId, Expr<'a>),
    Expr(Vec<'a, Expr<'a>>),
    Break,
    Continue,
    If(
        Vec<'a, Expr<'a>>,
        Box<'a, Stmt<'a>>,
        Option<Box<'a, Stmt<'a>>>,
    ),
    While(Vec<'a, Expr<'a>>, Box<'a, Stmt<'a>>),
    DoWhile(Box<'a, Stmt<'a>>, Vec<'a, Expr<'a>>),
    For,
    Block(ScopeRef<'a>, Vec<'a, Stmt<'a>>),
    Function(ScopeRef<'a>, VariableId, Params<'a>, Vec<'a, Stmt<'a>>),
    Return(Option<Vec<'a, Expr<'a>>>),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator<'a> {
    Ternary(Box<'a, Expr<'a>>),
    NullCoalessing(Box<'a, Expr<'a>>),
    TenaryNull,
    In,
    InstanceOf,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponentiate,
    ShiftLeft,
    ShiftRight,
    ShiftRightUnsigned,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
    And,
    Or,
    Index,
}

#[derive(Debug, PartialEq)]
pub enum AssignOperator {
    Assign,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponentiate,
    ShiftLeft,
    ShiftRight,
    ShiftRightUnsigned,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
}

#[derive(Debug, PartialEq)]
pub enum PrefixOperator {
    Not,
    Delete,
    Void,
    TypeOf,
    Positive,
    Negative,
    BinaryNot,
    AddOne,
    SubtractOne,
}

#[derive(Debug, PartialEq)]
pub enum PostfixOperator<'a> {
    AddOne,
    SubtractOne,
    Dot(StringId),
    Index(Box<'a, Expr<'a>>),
    Call(Vec<'a, Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Binary(Box<'a, Expr<'a>>, BinaryOperator<'a>, Box<'a, Expr<'a>>),
    Assign(Box<'a, Expr<'a>>, AssignOperator, Box<'a, Expr<'a>>),
    UnaryPrefix(PrefixOperator, Box<'a, Expr<'a>>),
    UnaryPostfix(Box<'a, Expr<'a>>, PostfixOperator<'a>),
    Prime(PrimeExpr<'a>),
}

impl<'a> Expr<'a> {
    pub fn is_assignable(&self) -> bool {
        match *self {
            Expr::Prime(ref x) => match x {
                PrimeExpr::Variable(_) => true,
                PrimeExpr::Literal(_) => false,
                PrimeExpr::Covered(_) => false,
                PrimeExpr::Object(_) => false,
            },
            Expr::Assign(..) => false,
            Expr::Binary(..) => false,
            Expr::UnaryPrefix(..) => false,
            Expr::UnaryPostfix(_, ref op) => match *op {
                PostfixOperator::Dot(_) => true,
                PostfixOperator::Index(_) => true,
                PostfixOperator::Call(_) => false,
                PostfixOperator::AddOne => false,
                PostfixOperator::SubtractOne => false,
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PrimeExpr<'a> {
    Literal(Literal),
    Variable(VariableId),
    Covered(Vec<'a, Expr<'a>>),
    Object(Vec<'a, (StringId, Expr<'a>)>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    String(StringId),
    Integer(i32),
    Float(f64),
    Boolean(bool),
}
