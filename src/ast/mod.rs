use crate::token::{BinOpToken, RelationToken, Token, UnaryOpToken};

mod expr;
pub use expr::*;

#[derive(Debug, Eq, PartialEq)]
pub enum MethodType {
    Normal,
    Getter,
    Setter,
}

#[derive(Debug)]
pub struct Method<'a> {
    pub name: PropertyName<'a>,
    pub params: Parameters<'a>,
    pub is_static: bool,
    pub ty: MethodType,
    pub block: Block<'a>,
}

#[derive(Debug)]
pub enum PropertyName<'a> {
    Literal(Token<'a>),
    Ident(Token<'a>),
    Computed(AssignExpr<'a>),
}

#[derive(Debug)]
pub enum Number {
    Integer(u32),
    Float(f64),
}

#[derive(Debug, Default)]
pub struct Parameters<'a> {
    pub params: Vec<(Binding<'a>, Option<AssignExpr<'a>>)>,
    pub rest: Option<Binding<'a>>,
}

#[derive(Debug)]
pub enum ArrayElement<'a> {
    Elision,
    Expr { expr: Box<AssignExpr<'a>> },
}

#[derive(Debug)]
pub enum Property<'a> {
    Ident(Token<'a>),
    Computed {
        idx: Expr<'a>,
        expr: AssignExpr<'a>,
    },
    Prop {
        name: PropertyName<'a>,
        expr: AssignExpr<'a>,
    },
    Method(Method<'a>),
    Rest {
        expr: AssignExpr<'a>,
    },
}

#[derive(Debug)]
pub struct Arguments<'a> {
    pub args: Vec<AssignExpr<'a>>,
    pub rest: Option<Box<AssignExpr<'a>>>,
}

#[derive(Debug)]
pub struct Catch<'a> {
    pub param: Option<Binding<'a>>,
    pub block: Block<'a>,
}

#[derive(Debug)]
pub struct SingleBinding<'a> {
    pub ident: Token<'a>,
    pub initializer: Option<AssignExpr<'a>>,
}

#[derive(Debug)]
pub enum BindingElement<'a> {
    Single(SingleBinding<'a>),
    Binding {
        bind: Binding<'a>,
        expr: Option<AssignExpr<'a>>,
    },
}

#[derive(Debug)]
pub enum ObjectBinding<'a> {
    SingleName {
        ident: Token<'a>,
        expr: Option<AssignExpr<'a>>,
    },
    PropertyName {
        name: PropertyName<'a>,
        binding: Box<Binding<'a>>,
        expr: Option<AssignExpr<'a>>,
    },
}

#[derive(Debug)]
pub enum ArrayBinding<'a> {
    Elision,
    Binding {
        bind: Binding<'a>,
        expr: Option<AssignExpr<'a>>,
    },
}

#[derive(Debug)]
pub enum Binding<'a> {
    ObjectPattern {
        bindings: Vec<ObjectBinding<'a>>,
        rest: Option<Token<'a>>,
    },
    ArrayPattern {
        bindings: Vec<ArrayBinding<'a>>,
        rest: Option<Box<Binding<'a>>>,
    },
    Ident {
        ident: Token<'a>,
    },
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
pub struct Decl<'a> {
    pub kind: LexicalKind,
    pub decl: Vec<LexicalDecl<'a>>,
}

#[derive(Debug)]
pub enum DeclKind<'a> {
    Function,
    Generator,
    AsyncFunction,
    AsyncGenerator,
    Class,
    /// Let and const bindings
    Lexical(Decl<'a>),
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub struct Clause<'a> {
    pub expr: Expr<'a>,
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub enum ForKind<'a> {
    Empty {
        condition: Option<Expr<'a>>,
        iteration: Option<Expr<'a>>,
    },
    DeclCLike {
        decl: Decl<'a>,
        condition: Option<Expr<'a>>,
        iteration: Option<Expr<'a>>,
    },
    DeclIn {
        decl: Decl<'a>,
        expr: Expr<'a>,
    },
    DeclOf {
        decl: Decl<'a>,
        expr: AssignExpr<'a>,
    },
    ExprCLike {
        expr: Expr<'a>,
        condition: Option<Expr<'a>>,
        iteration: Option<Expr<'a>>,
    },
    ExprIn {
        lhs: AssignExpr<'a>,
        expr: Expr<'a>,
    },
    ExprOf {
        lhs: AssignExpr<'a>,
        expr: Expr<'a>,
    },
}

#[derive(Debug)]
pub struct Class<'a> {
    pub name: Option<Token<'a>>,
    pub heritage: Option<AssignExpr<'a>>,
    pub methods: Vec<Method<'a>>,
}

#[derive(Debug)]
pub struct For<'a> {
    pub kind: ForKind<'a>,
    pub stmt: Box<Stmt<'a>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    /// A new block statement,
    /// { stmts, .. }
    Block(Block<'a>),
    Declaration {
        kind: DeclKind<'a>,
    },
    Class(Class<'a>),
    Empty,
    Expr {
        expr: Expr<'a>,
    },
    If {
        expr: Expr<'a>,
        body: Box<Stmt<'a>>,
        else_body: Option<Box<Stmt<'a>>>,
    },
    Switch {
        expr: Expr<'a>,
        clauses: Vec<Clause<'a>>,
        default: Option<Vec<Stmt<'a>>>,
    },
    For(For<'a>),
    Continue,
    Break {
        label: Option<Token<'a>>,
    },
    DoWhile {
        body: Box<Stmt<'a>>,
        expr: Expr<'a>,
    },
    While {
        expr: Expr<'a>,
        body: Box<Stmt<'a>>,
    },
    Return {
        expr: Option<Expr<'a>>,
    },
    Labelled,
    Throw {
        expr: Expr<'a>,
    },
    Try {
        block: Block<'a>,
        catch: Option<Catch<'a>>,
        finally: Option<Block<'a>>,
    },
    With {
        expr: Expr<'a>,
        stmt: Box<Stmt<'a>>,
    },
    Debugger,
}

#[derive(Debug)]
pub struct Script<'a> {
    pub stmts: Vec<Stmt<'a>>,
}
