use crate::{
    source::Span,
    token::{BinOpToken, RelationToken, Token, UnaryOpToken},
};

mod expr;
pub use expr::*;

#[derive(Debug, Eq, PartialEq)]
pub enum MethodType {
    Normal,
    Getter,
    Setter,
}

#[derive(Debug)]
pub struct Method {
    pub name: PropertyName,
    pub params: Parameters,
    pub is_static: bool,
    pub ty: MethodType,
    pub block: Block,
}

#[derive(Debug)]
pub enum PropertyName {
    Literal(Literal),
    Ident(Ident),
    Computed(AssignExpr),
}

#[derive(Debug)]
pub struct Ident(pub String);

#[derive(Debug)]
pub enum Literal {
    Number(Number),
    String(String),
}

#[derive(Debug)]
pub enum Number {
    Integer(i32),
    Float(f64),
    Big(String),
}

#[derive(Debug, Default)]
pub struct Parameters {
    pub params: Vec<(Binding, Option<AssignExpr>)>,
    pub rest: Option<Binding>,
}

#[derive(Debug)]
pub enum ArrayElement {
    Elision,
    Expr { expr: Box<AssignExpr> },
}

#[derive(Debug)]
pub enum Property {
    Ident(Ident),
    Computed {
        idx: Expr,
        expr: AssignExpr,
    },
    Prop {
        name: PropertyName,
        expr: AssignExpr,
    },
    Method(Method),
    Rest {
        expr: AssignExpr,
    },
}

#[derive(Debug)]
pub struct Arguments {
    pub args: Vec<AssignExpr>,
    pub rest: Option<Box<AssignExpr>>,
}

#[derive(Debug)]
pub struct Catch {
    pub param: Option<Binding>,
    pub block: Block,
}

#[derive(Debug)]
pub struct SingleBinding {
    pub ident: Ident,
    pub initializer: Option<AssignExpr>,
}

#[derive(Debug)]
pub enum BindingElement {
    Single(SingleBinding),
    Binding {
        bind: Binding,
        expr: Option<AssignExpr>,
    },
}

#[derive(Debug)]
pub enum ObjectBinding {
    SingleName {
        ident: Ident,
        expr: Option<AssignExpr>,
    },
    PropertyName {
        name: PropertyName,
        binding: Box<Binding>,
        expr: Option<AssignExpr>,
    },
}

#[derive(Debug)]
pub enum ArrayBinding {
    Elision,
    Binding {
        bind: Binding,
        expr: Option<AssignExpr>,
    },
}

#[derive(Debug)]
pub enum Binding {
    ObjectPattern {
        bindings: Vec<ObjectBinding>,
        rest: Option<Ident>,
    },
    ArrayPattern {
        bindings: Vec<ArrayBinding>,
        rest: Option<Box<Binding>>,
    },
    Ident(Ident),
}

#[derive(Debug)]
pub enum LexicalKind {
    Var,
    Const,
    Let,
}

#[derive(Debug)]
pub struct LexicalDecl {
    pub binding: Binding,
    pub initializer: Option<AssignExpr>,
}

#[derive(Debug)]
pub struct Decl {
    pub kind: LexicalKind,
    pub decl: Vec<LexicalDecl>,
}

#[derive(Debug)]
pub enum DeclKind {
    Function,
    Generator,
    AsyncFunction,
    AsyncGenerator,
    Class,
    /// Let and const bindings
    Lexical(Decl),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Clause {
    pub expr: Expr,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum ForKind {
    Empty {
        condition: Option<Expr>,
        iteration: Option<Expr>,
    },
    DeclCLike {
        decl: Decl,
        condition: Option<Expr>,
        iteration: Option<Expr>,
    },
    DeclIn {
        decl: Decl,
        expr: Expr,
    },
    DeclOf {
        decl: Decl,
        expr: AssignExpr,
    },
    ExprCLike {
        expr: Expr,
        condition: Option<Expr>,
        iteration: Option<Expr>,
    },
    ExprIn {
        lhs: AssignExpr,
        expr: Expr,
    },
    ExprOf {
        lhs: AssignExpr,
        expr: Expr,
    },
}

#[derive(Debug)]
pub struct Class {
    pub name: Option<Ident>,
    pub heritage: Option<AssignExpr>,
    pub methods: Vec<Method>,
}

#[derive(Debug)]
pub struct For {
    pub kind: ForKind,
    pub stmt: Box<Stmt>,
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    /// A new block statement,
    /// { stmts, .. }
    Block(Block),
    Declaration {
        kind: DeclKind,
    },
    Class(Class),
    Empty,
    Expr {
        expr: Expr,
    },
    If {
        expr: Expr,
        body: Box<Stmt>,
        else_body: Option<Box<Stmt>>,
    },
    Switch {
        expr: Expr,
        clauses: Vec<Clause>,
        default: Option<Vec<Stmt>>,
    },
    For(For),
    Continue,
    Break {
        label: Option<Ident>,
    },
    DoWhile {
        body: Box<Stmt>,
        expr: Expr,
    },
    While {
        expr: Expr,
        body: Box<Stmt>,
    },
    Return {
        expr: Option<Expr>,
    },
    Labelled,
    Throw {
        expr: Expr,
    },
    Try {
        block: Block,
        catch: Option<Catch>,
        finally: Option<Block>,
    },
    With {
        expr: Expr,
        stmt: Box<Stmt>,
    },
    Debugger,
}

#[derive(Debug)]
pub struct Script {
    pub stmts: Vec<Stmt>,
}
