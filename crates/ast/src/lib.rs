#![feature(allocator_api)]

use derivative::Derivative;
use std::{alloc::Allocator, hash::Hash, mem};

//pub use common::bump_list::List as Vec;
use common::interner::StringId;
use std::cmp::PartialEq;

pub mod symbol_table;
pub use symbol_table::{ScopeId, SymbolId, SymbolTable, SymbolTableBuilder};

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub struct Script<A: Allocator>(pub Vec<Stmt<A>, A>);

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub enum Rest {
    BindingIdent(SymbolId),
}

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub struct Params<A: Allocator>(pub Vec<SymbolId, A>, pub Option<Rest>);

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub struct Catch<A: Allocator> {
    pub binding: Option<SymbolId>,
    pub stmt: Box<Stmt<A>, A>,
}

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub struct Case<A: Allocator> {
    pub expr: Expr<A>,
    pub stmts: Vec<Stmt<A>, A>,
}

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub enum Stmt<A: Allocator> {
    Empty,
    Let(SymbolId, Option<Expr<A>>),
    Var(Vec<(SymbolId, Option<Expr<A>>), A>),
    Const(SymbolId, Expr<A>),
    Expr(Vec<Expr<A>, A>),
    Throw(Expr<A>),
    Break,
    Continue,
    If(Vec<Expr<A>, A>, Box<Stmt<A>, A>, Option<Box<Stmt<A>, A>>),
    Switch(Vec<Expr<A>, A>, Vec<Case<A>, A>, Option<Vec<Stmt<A>, A>>),
    While(Vec<Expr<A>, A>, Box<Stmt<A>, A>),
    DoWhile(Box<Stmt<A>, A>, Vec<Expr<A>, A>),
    For(
        Option<ForDecl<A>>,
        Option<Vec<Expr<A>, A>>,
        Option<Vec<Expr<A>, A>>,
        Box<Stmt<A>, A>,
    ),
    Block(ScopeId, Vec<Stmt<A>, A>),
    Function(ScopeId, SymbolId, Params<A>, Vec<Stmt<A>, A>),
    Return(Option<Vec<Expr<A>, A>>),
    Try(Box<Stmt<A>, A>, Option<Catch<A>>, Option<Box<Stmt<A>, A>>),
}

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub enum ForDecl<A: Allocator> {
    Stmt(Box<Stmt<A>, A>),
    Expr(Expr<A>),
}

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub enum BinaryOperator<A: Allocator> {
    Ternary(Box<Expr<A>, A>),
    NullCoalessing,
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

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
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

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub enum PrefixOperator {
    Not,
    Delete,
    Void,
    TypeOf,
    Positive,
    Negative,
    BitwiseNot,
    AddOne,
    SubtractOne,
    New,
}

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub enum PostfixOperator<A: Allocator> {
    AddOne,
    SubtractOne,
    Dot(StringId),
    Index(Box<Expr<A>, A>),
    Call(Vec<Expr<A>, A>),
}

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub enum Expr<A: Allocator> {
    Binary(Box<Expr<A>, A>, BinaryOperator<A>, Box<Expr<A>, A>),
    Assign(Box<Expr<A>, A>, AssignOperator, Box<Expr<A>, A>),
    UnaryPrefix(PrefixOperator, Box<Expr<A>, A>),
    UnaryPostfix(Box<Expr<A>, A>, PostfixOperator<A>),
    Prime(PrimeExpr<A>),
}

impl<A: Allocator> Expr<A> {
    pub fn is_assignable(&self) -> bool {
        match *self {
            Expr::Prime(ref x) => match x {
                PrimeExpr::Variable(_) => true,
                PrimeExpr::Eval(_)
                | PrimeExpr::Literal(_)
                | PrimeExpr::Covered(_)
                | PrimeExpr::Object(_)
                | PrimeExpr::Array(_)
                | PrimeExpr::Function(_, _, _, _)
                | PrimeExpr::This
                | PrimeExpr::NewTarget => false,
            },
            Expr::Assign(ref left, ref op, _) => match op {
                AssignOperator::Assign => left.is_assignable(),
                _ => false,
            },
            Expr::Binary(..) | Expr::UnaryPrefix(..) => false,
            Expr::UnaryPostfix(_, ref op) => match *op {
                PostfixOperator::Dot(_) | PostfixOperator::Index(_) => true,
                PostfixOperator::Call(_)
                | PostfixOperator::AddOne
                | PostfixOperator::SubtractOne => false,
            },
        }
    }
}

#[derive(Derivative, PartialEq)]
#[derivative(Debug(bound = ""))]
pub enum PrimeExpr<A: Allocator> {
    Literal(Literal),
    Variable(SymbolId),
    Covered(Vec<Expr<A>, A>),
    Object(Vec<(StringId, Expr<A>), A>),
    Array(Vec<Expr<A>, A>),
    Function(ScopeId, Option<SymbolId>, Params<A>, Vec<Stmt<A>, A>),
    // A direct eval call
    Eval(Vec<Expr<A>, A>),
    This,
    NewTarget,
}

#[derive(Derivative, Clone, Copy)]
#[derivative(Debug(bound = ""))]
pub enum Literal {
    Null,
    Undefined,
    String(StringId),
    Integer(i32),
    Float(f64),
    Boolean(bool),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Literal::Null => {
                matches!(other, Literal::Null)
            }
            Literal::Undefined => {
                matches!(other, Literal::Undefined)
            }
            Literal::Float(a) => {
                if let Literal::Float(b) = other {
                    a.to_bits() == b.to_bits()
                } else {
                    false
                }
            }
            Literal::Integer(a) => {
                if let Literal::Integer(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Literal::Boolean(a) => {
                if let Literal::Boolean(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Literal::String(a) => {
                if let Literal::String(b) = other {
                    a == b
                } else {
                    false
                }
            }
        }
    }
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            Literal::Null | Literal::Undefined => {}
            Literal::Float(x) => x.to_bits().hash(state),
            Literal::Integer(x) => x.hash(state),
            Literal::Boolean(x) => x.hash(state),
            Literal::String(x) => x.hash(state),
        }
    }
}
