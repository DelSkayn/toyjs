use crate::token::{Token, UnaryOpToken};

#[derive(Debug)]
pub enum Number {
    Integer(u32),
    Float(f64),
}

#[derive(Debug)]
pub struct Parameters<'a> {
    pub params: Vec<(Binding<'a>, Option<AssignExpr<'a>>)>,
    pub rest: Option<Binding<'a>>,
}

#[derive(Debug)]
pub enum PrimeExpr<'a> {
    String(&'a str),
    Number(Number),
    BinInt,
    Ident {
        token: Token<'a>,
    },
    ArrayLiteral {
        elems: Vec<Expr<'a>>,
        spread: Box<Expr<'a>>,
    },
    This,
    Null,
    Boolean(bool),
    Function {
        binding: Option<Token<'a>>,
        params: Parameters<'a>,
        block: Block<'a>,
    },
    ObjectLiteral,
    Class,
    Generator,
    AsyncFunc,
    AsyncGen,
    Regular,
    Template,
}

#[derive(Debug)]
pub struct Arguments<'a> {
    pub args: Vec<AssignExpr<'a>>,
}

#[derive(Debug)]
pub enum AssignExpr<'a> {
    In {
        left: Box<AssignExpr<'a>>,
        right: Token<'a>,
    },
    Prime {
        kind: PrimeExpr<'a>,
    },
    SuperDot,
    SuperIndex {
        expr: Expr<'a>,
    },
    Dot,
    Index {
        expr: Expr<'a>,
    },
    NewTarget,
    ImportMeta,
    Call {
        expr: Box<AssignExpr<'a>>,
        args: Arguments<'a>,
    },
    New {
        expr: Box<AssignExpr<'a>>,
    },
    NewCall {
        expr: Box<AssignExpr<'a>>,
        args: Arguments<'a>,
    },
}

#[derive(Debug)]
pub enum Expr<'a> {
    Literal {
        token: Token<'a>,
    },
    Unary {
        kind: UnaryOpToken,
        prefix: bool,
        arg: Box<Expr<'a>>,
    },
    Prime {
        kind: PrimeExpr<'a>,
    },
    AssignExpr {
        expr: Box<AssignExpr<'a>>,
    },
}

#[derive(Debug)]
pub struct Catch<'a> {
    pub param: Option<Binding<'a>>,
    pub block: Block<'a>,
}

#[derive(Debug)]
pub enum Binding<'a> {
    ObjectPattern,
    ArrayPattern,
    Ident { ident: Token<'a> },
}

#[derive(Debug)]
pub enum LexicalKind {
    Var,
    Const,
    Let,
}

#[derive(Debug)]
pub struct LexicalDecl<'a> {
    pub binding: Binding<'a>,
    pub initializer: Option<AssignExpr<'a>>,
}

#[derive(Debug)]
pub enum DeclKind<'a> {
    Function,
    Generator,
    AsyncFunction,
    AsyncGenerator,
    Class,
    /// Let and const bindings
    Lexical {
        /// Wether the binding is const, otherwise it is let
        kind: LexicalKind,
        decl: Vec<LexicalDecl<'a>>,
    },
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    /// A new block statement,
    /// { stmts, .. }
    Block(Block<'a>),
    Declaration {
        kind: DeclKind<'a>,
    },
    Empty,
    Expression,
    If,
    Iteration,
    Switch,
    Continue,
    Break,
    Return,
    With,
    Labelled,
    Throw {
        exprs: Vec<AssignExpr<'a>>,
    },
    Try {
        block: Block<'a>,
        catch: Option<Catch<'a>>,
        finally: Option<Block<'a>>,
    },
    Debugger,
}

pub enum ItemKind {}

pub struct Item {}

#[derive(Debug)]
pub struct Script<'a> {
    pub stmts: Vec<Stmt<'a>>,
}
