use crate::token::{BinOpToken, RelationToken, Token, UnaryOpToken};

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
    Expr { expr: AssignExpr<'a> },
    Spread { expr: AssignExpr<'a> },
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
pub enum PrimeExpr<'a> {
    String(&'a str),
    Number(Number),
    BinInt,
    Ident {
        token: Token<'a>,
    },
    This,
    Null,
    Boolean(bool),
    Function {
        binding: Option<Token<'a>>,
        params: Parameters<'a>,
        block: Block<'a>,
    },
    ObjectLiteral {
        properties: Vec<Property<'a>>,
    },
    ArrayLiteral {
        elems: Vec<ArrayElement<'a>>,
    },
    Class,
    Generator {
        binding: Option<Token<'a>>,
        params: Parameters<'a>,
        block: Block<'a>,
    },
    ParamList {
        expr: Option<Expr<'a>>,
        rest: Option<Binding<'a>>,
    },
    AsyncFunc,
    AsyncGen,
    Regular,
    Template,
}

#[derive(Debug)]
pub struct Arguments<'a> {
    pub args: Vec<AssignExpr<'a>>,
    pub rest: Option<Box<AssignExpr<'a>>>,
}

#[derive(Debug)]
pub enum LhsExpr<'a> {
    New(Box<LhsExpr<'a>>),
    Prime(PrimeExpr<'a>),
    Import(Box<AssignExpr<'a>>),
    SuperCall(Arguments<'a>),
    SuperIdx(Expr<'a>),
    SuperDot(Token<'a>),
    ImportMeta,
    NewTarget,
    Index {
        lhs: Box<LhsExpr<'a>>,
        expr: Expr<'a>,
    },
    Dot {
        lhs: Box<LhsExpr<'a>>,
        ident: Token<'a>,
    },
    Call {
        lhs: Box<LhsExpr<'a>>,
        args: Arguments<'a>,
    },
}

#[derive(Debug)]
pub enum AssignExpr<'a> {
    Delete(Box<AssignExpr<'a>>),
    Void(Box<AssignExpr<'a>>),
    Typeof(Box<AssignExpr<'a>>),
    Positive(Box<AssignExpr<'a>>),
    Negative(Box<AssignExpr<'a>>),
    BinaryNot(Box<AssignExpr<'a>>),
    Not(Box<AssignExpr<'a>>),
    Unary {
        op: UnaryOpToken,
        expr: Box<AssignExpr<'a>>,
    },
    Post {
        incr: bool,
        expr: Box<AssignExpr<'a>>,
    },
    LhsExpr(LhsExpr<'a>),
    Assign {
        lhs: LhsExpr<'a>,
        expr: Box<AssignExpr<'a>>,
    },
    AssignOp {
        lhs: LhsExpr<'a>,
        op: BinOpToken,
        expr: Box<AssignExpr<'a>>,
    },
    BinOp {
        lhs: Box<AssignExpr<'a>>,
        op: BinOpToken,
        expr: Box<AssignExpr<'a>>,
    },
    Relation {
        lhs: Box<AssignExpr<'a>>,
        rel: RelationToken,
        expr: Box<AssignExpr<'a>>,
    },
    Instanceof {
        lhs: Box<AssignExpr<'a>>,
        expr: Box<AssignExpr<'a>>,
    },
    In {
        lhs: Box<AssignExpr<'a>>,
        expr: Box<AssignExpr<'a>>,
    },
}

#[derive(Debug)]
pub struct Expr<'a> {
    pub exprs: Vec<AssignExpr<'a>>,
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
    Ident {
        ident: Token<'a>,
        init: Option<AssignExpr<'a>>,
    },
    Literal {
        lit: Token<'a>,
        bind: Binding<'a>,
        init: Option<AssignExpr<'a>>,
    },
    Computed {
        expr: Expr<'a>,
        bind: Binding<'a>,
        init: Option<AssignExpr<'a>>,
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
        rest: Option<Token<'a>>,
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
pub struct Clause<'a> {
    pub expr: Expr<'a>,
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub enum ForKind<'a> {
    In {
        expr: Expr<'a>,
    },
    Of {
        expr: AssignExpr<'a>,
    },
    CLike {
        cmp: Option<Expr<'a>>,
        incr: Option<Expr<'a>>,
    },
}

#[derive(Debug)]
pub enum ForDecl<'a> {
    Decl(DeclKind<'a>),
    LhsExpr(LhsExpr<'a>),
    Expr(Expr<'a>),
}

#[derive(Debug)]
pub struct Class<'a> {
    pub name: Option<Token<'a>>,
    pub heritage: Option<LhsExpr<'a>>,
    pub methods: Vec<Method<'a>>,
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
    For {
        start: Option<ForDecl<'a>>,
        kind: ForKind<'a>,
        stmt: Box<Stmt<'a>>,
    },
    Continue,
    Break {
        label: Option<Token<'a>>,
    },
    DoWhile {
        body: Box<Stmt<'a>>,
        expr: Expr<'a>,
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
