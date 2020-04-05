use crate::token::Token;

#[derive(Debug)]
pub enum Expr<'a> {
    Literal { token: Token<'a> },
}

#[derive(Debug)]
pub struct Catch<'a> {
    pub param: Binding<'a>,
    pub block: Block<'a>,
}

#[derive(Debug)]
pub enum Binding<'a> {
    ObjectPattern,
    ArrayPattern,
    Ident {
        ident: Token<'a>,
        initializer: Option<Expr<'a>>,
    },
}

#[derive(Debug)]
pub enum LexicalKind {
    Var,
    Const,
    Let,
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
        bindings: Vec<Binding<'a>>,
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
    Throw,
    Try {
        block: Box<Block<'a>>,
        catch: Option<Box<Catch<'a>>>,
        finally: Option<Box<Block<'a>>>,
    },
    Debugger,
}

pub enum ItemKind {}

pub struct Item {}

#[derive(Debug)]
pub struct Script<'a> {
    pub stmts: Vec<Stmt<'a>>,
}
