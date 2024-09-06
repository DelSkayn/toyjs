use std::hash::BuildHasherDefault;

use ast::{impl_display_debug, ListStorage, OptionListStorage};
use common::{hasher::Hasher, id::collections::IdSet, number::Number, span::Span, string::String};

mod ast;
pub use ast::{
    Ast as GenAst, AstRender, Node, NodeId, NodeList, NodeListId, OptionNodeList, OptionNodeListId,
};

#[cfg(feature = "visitor")]
pub mod visitor;

type LibrarySet<T> = IdSet<u32, T, BuildHasherDefault<Hasher>>;

library! {Library{
    stmt: Vec<Stmt>,

    catch: Vec<CatchStmt>,
    for_loop_head: Vec<ForLoopHead>,

    expr: Vec<Expr>,
    exprs: Vec<NodeListId<Expr>>,
    prime_exprs: Vec<PrimeExpr>,
    tenary: Vec<Tenary>,
    template: Vec<Template>,

    case: Vec<CaseItem>,

    var_decl: Vec<VariableDecl>,

    ident_pattern: Vec<IdentOrPattern>,
    element: Vec<BindingElement>,
    elements: Vec<NodeList<BindingElement>>,
    property: Vec<BindingProperty>,
    pattern: Vec<BindingPattern>,

    function: Vec<Function>,
    argument: Vec<Argument>,
    class: Vec<Class>,
    class_member: Vec<ClassMember>,
    array_literal: Vec<ArrayLiteralEntry>,
    propery_definition: Vec<PropertyDefinition>,

    symbol: Vec<Symbol>,

    lists: Vec<ListStorage>,
    option_lists: Vec<OptionListStorage>,

    numbers: LibrarySet<Number>,
    strings: LibrarySet<String>,
}}

pub type Ast = GenAst<Library>;

ast_struct! {
    pub struct Symbol {
        pub name: NodeId<String>,
        pub span: Span,
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub enum VariableKind {
    Const,
    Var,
    Let,
}

#[cfg(feature = "print")]
impl_display_debug!(VariableKind);

ast_enum! {
    pub enum IdentOrPattern {
        Ident{
            name: NodeId<Symbol>
        },
        Pattern{
            pattern: NodeId<BindingPattern>
        }
    }
}

ast_enum! {
    pub enum BindingPattern {
        Object {
            properties: Option<NodeListId<BindingProperty>>,
            rest: Option<NodeId<Symbol>>,
        },
        Array {
            elements: Option<OptionNodeListId<BindingElement>>,
            rest: Option<NodeId<IdentOrPattern>>,
        },
    }
}

ast_enum! {
    pub enum PropertyName {
        Ident{name: NodeId<String>},
        String{ value: NodeId<String> },
        Number{ value: NodeId<Number> },
        Computed{ expr: NodeId<Expr> },
    }
}

ast_enum! {
    pub enum BindingProperty {
        Binding {
            symbol: NodeId<Symbol>,
            initializer: Option<NodeId<Expr>>,
        },
        Property {
            name: PropertyName,
            element: NodeId<BindingElement>,
        },
    }
}

ast_enum! {
    pub enum BindingElement {
        SingleName {
            symbol: NodeId<Symbol>,
            initializer: Option<NodeId<Expr>>,
        },
        Pattern {
            pattern: NodeId<BindingPattern>,
            initializer: Option<NodeId<Expr>>,
        },
    }
}

ast_struct! {
    pub struct VariableDecl {
        pub decl: NodeId<IdentOrPattern>,
        pub initializer: Option<NodeId<Expr>>,
    }
}

ast_enum! {
    pub enum Stmt {
        Block {
            list: Option<NodeListId<Stmt>>,
        },
        VariableDecl {
            kind: VariableKind,
            decl: NodeListId<VariableDecl>,
        },
        Empty,
        Expr {
            expr: NodeListId<Expr>,
        },
        DoWhile {
            body: NodeId<Stmt>,
            cond: NodeListId<Expr>,
        },
        If {
            cond: NodeListId<Expr>,
            body: NodeId<Stmt>,
            r#else: Option<NodeId<Stmt>>,
        },
        While {
            cond: NodeListId<Expr>,
            body: NodeId<Stmt>,
        },
        For {
            head: NodeId<ForLoopHead>,
            body: NodeId<Stmt>,
        },
        Switch {
            cond: NodeListId<Expr>,
            cases: Option<NodeListId<CaseItem>>,
            default: Option<NodeListId<Stmt>>,
        },
        Throw {
            expr: NodeListId<Expr>,
        },
        Try {
            block: Option<NodeListId<Stmt>>,
            catch: Option<NodeId<CatchStmt>>,
            finally: Option<NodeListId<Stmt>>,
        },
        With {
            expr: NodeListId<Expr>,
            stmt: NodeId<Stmt>,
        },
        Break {
            label: Option<NodeId<String>>,
        },
        Continue {
            label: Option<NodeId<String>>,
        },
        Return {
            expr: Option<NodeListId<Expr>>,
        },
        Labeled {
            label: Option<NodeId<String>>,
            stmt: NodeId<Stmt>,
        },
        Function {
            func: NodeId<Function>,
        },
        Class {
            class: NodeId<Class>,
        },
        Debugger,
    }
}

ast_enum! {
    pub enum CstyleDecl {
        Expr{ expr: NodeListId<Expr> },
        Decl {
            kind: VariableKind,
            decl: NodeListId<VariableDecl>,
        },
        Empty,
    }
}

ast_enum! {
    pub enum InOfDecl {
        Expr{ expr: NodeId<Expr>},
        Decl {
            kind: VariableKind,
            binding: NodeId<IdentOrPattern>,
        },
    }
}

ast_enum! {
    pub enum ForLoopHead {
        CStyle {
            decl: CstyleDecl,
            cond: Option<NodeListId<Expr>>,
            post: Option<NodeListId<Expr>>,
        },
        In {
            decl: InOfDecl,
            expr: NodeListId<Expr>,
        },
        Of {
            decl: InOfDecl,
            expr: NodeId<Expr>,
        },
    }
}

ast_struct! {
    pub struct CaseItem {
        pub expr: NodeListId<Expr>,
        pub stmts: Option<NodeListId<Stmt>>,
    }
}

ast_struct! {
    pub struct CatchStmt {
        pub binding: Option<NodeId<IdentOrPattern>>,
        pub block: Option<NodeListId<Stmt>>,
    }
}

ast_struct! {
    pub struct Parameters {
        pub rest: bool,
        pub binding: NodeId<IdentOrPattern>,
        pub initializer: Option<NodeId<Expr>>,
    }
}

ast_enum! {
    pub enum ArrowFunctionBody {
        Expr{ expr: NodeId<Expr> },
        Stmt{ body: Option<NodeListId<Stmt>> },
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionKind {
    Simple,
    Async,
    Generator,
    AsyncGenerator,
}

#[cfg(feature = "print")]
ast::impl_display_debug!(FunctionKind);

impl FunctionKind {
    pub fn set_async(&mut self) {
        match *self {
            Self::Simple => *self = FunctionKind::Async,
            Self::Generator => *self = FunctionKind::AsyncGenerator,
            _ => {}
        }
    }

    pub fn set_generator(&mut self) {
        match *self {
            Self::Simple => *self = FunctionKind::Generator,
            Self::Async => *self = FunctionKind::AsyncGenerator,
            _ => {}
        }
    }
}

ast_enum! {
    pub enum Function {
        Arrow {
            is_strict: bool,
            kind: FunctionKind,
            params: Option<NodeListId<BindingElement>>,
            rest_param: Option<NodeId<IdentOrPattern>>,
            body: ArrowFunctionBody,
        },
        Declared {
            is_strict: bool,
            kind: FunctionKind,
            name: NodeId<Symbol>,
            params: Option<NodeListId<BindingElement>>,
            rest_param: Option<NodeId<IdentOrPattern>>,
            body: Option<NodeListId<Stmt>>,
        },
        Expr {
            is_strict: bool,
            kind: FunctionKind,
            name: Option<NodeId<Symbol>>,
            params: Option<NodeListId<BindingElement>>,
            rest_param: Option<NodeId<IdentOrPattern>>,
            body: Option<NodeListId<Stmt>>,
        },
    }
}

ast_struct! {
    pub struct Class {
        pub name: Option<NodeId<Symbol>>,
        pub heritage: Option<NodeId<Expr>>,
        pub body: Option<NodeListId<ClassMember>>,
    }
}

ast_enum! {
    pub enum ClassMember {
        StaticBlock {
            stmts: Option<NodeListId<Stmt>>,
        },
        Method {
            is_static: bool,
            property: PropertyName,
            func: NodeId<Function>,
        },
        Field {
            is_static: bool,
            property: PropertyName,
            initializer: Option<NodeId<Expr>>,
        },
        Getter {
            is_static: bool,
            property: PropertyName,
            func: NodeId<Function>,
        },
        Setter {
            is_static: bool,
            property: PropertyName,
            func: NodeId<Function>,
        },
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
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

#[cfg(feature = "print")]
ast::impl_display_debug!(BaseOp);

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
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

#[cfg(feature = "print")]
ast::impl_display_debug!(AssignOp);

impl AssignOp {
    /// Returns true if the operation requires a load.
    pub fn loads(self) -> bool {
        self != AssignOp::Assign
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub enum BinaryOp {
    Base(BaseOp),
    Assign(AssignOp),
}

#[cfg(feature = "print")]
ast::impl_display_debug!(BinaryOp);

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub enum PostfixOp {
    AddOne,
    SubOne,
}

#[cfg(feature = "print")]
ast::impl_display_debug!(PostfixOp);

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
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
    Await,
}

#[cfg(feature = "print")]
ast::impl_display_debug!(PrefixOp);

ast_struct! {
    pub struct Argument {
        pub is_spread: bool,
        pub expr: NodeId<Expr>,
    }
}

ast_enum! {
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
        Ternary{
            ternary: NodeId<Tenary>
        },
        // Postfix like operators with internal values
        // Are inlined into Expr in order to keep the Expr size smaller.
        Index {
            index: NodeId<Expr>,
            expr: NodeId<Expr>,
        },
        Dot {
            ident: NodeId<String>,
            expr: NodeId<Expr>,
        },
        Call {
            args: Option<NodeListId<Argument>>,
            expr: NodeId<Expr>,
        },
        Prime {
            expr: NodeId<PrimeExpr>,
        },
        Yield {
            star: bool,
            expr: NodeId<Expr>,
        },
        Destructure {
            // TODO: Change destructuring assignment to use assigment expression.
            pattern: NodeId<BindingPattern>,
            expr: NodeId<Expr>,
        },
        TaggedTemplate {
            tag: NodeId<Expr>,
            template: NodeId<Template>,
        },
    }
}

ast_struct! {
    pub struct Tenary {
        pub cond: NodeId<Expr>,
        pub then: NodeId<Expr>,
        pub r#else: NodeId<Expr>,
    }
}

ast_struct! {
    pub struct ArrayLiteral {
        pub elements: Option<NodeListId<ArrayLiteralEntry>>,
    }
}

ast_struct! {
    pub struct ArrayLiteralEntry {
        pub expr: Option<NodeId<Expr>>,
        pub is_spread: bool,
    }
}

ast_enum! {
    pub enum PrimeExpr {
        Number{ value: NodeId<Number> },
        String{ value: NodeId<String> },
        Template{ template: NodeId<Template> },
        Regex{ regex: NodeId<String> },
        Ident{ symbol: NodeId<Symbol> },
        Boolean{ value: bool },
        Function{ function: NodeId<Function> },
        Class{ class: NodeId<Class> },
        Object{ object: ObjectLiteral },
        Array{ array: Option<NodeListId<ArrayLiteralEntry>> },
        NewTarget,
        Null,
        This,
        Super,
        Covered{ expr: NodeListId<Expr> },
    }
}

ast_enum! {
    pub enum Template {
        Head {
            text: NodeId<String>,
            expr: NodeListId<Expr>,
            next: NodeId<Template>,
        },
        Tail {
            text: NodeId<String>,
        },
    }
}

ast_enum! {
    pub enum PropertyDefinition {
        Ident {
            ident: NodeId<Symbol>,
        },
        /// A destructing assignment will first be parsed as an object literal and then later refined.
        /// This production is not valid for object literals but is for a destructing assignment and
        /// therefore should never be part of a fully parsed object literal.
        Covered {
            symbol: NodeId<Symbol>,
            initializer: NodeId<Expr>,
        },
        Define {
            property: PropertyName,
            expr: NodeId<Expr>,
        },
        Method {
            property: PropertyName,
            func: NodeId<Function>,
        },
        Getter {
            property: PropertyName,
            func: NodeId<Function>,
        },
        Setter {
            property: PropertyName,
            func: NodeId<Function>,
        },
        Rest{ rest: NodeId<Expr>  },
    }
}

ast_enum! {
    pub enum ObjectLiteral {
        Empty,
        Item{ definition: NodeListId<PropertyDefinition> },
    }
}
