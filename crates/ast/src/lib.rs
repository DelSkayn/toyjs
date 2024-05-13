use core::fmt;

use common::{number::NumberId, span::Span, string::StringId};

mod ast;
pub mod visitor;
pub use ast::{Ast as GenAst, List, ListHead, ListId, NodeId, NodeList, NodeListId};

mod render;
pub use render::{RenderAst, RenderCtx, Result};

pub type AstStorage = (
    (Vec<Stmt>, Vec<CaseItem>, Vec<CatchStmt>, Vec<ForLoopHead>),
    (
        Vec<Expr>,
        Vec<PrimeExpr>,
        Vec<Tenary>,
        Vec<NodeList<Option<NodeId<Expr>>>>,
        Vec<Template>,
        Vec<NodeList<Argument>>,
    ),
    (
        Vec<IdentOrPattern>,
        Vec<VariableDecl>,
        Vec<NodeList<Option<NodeId<BindingElement>>>>,
        Vec<BindingElement>,
        Vec<BindingProperty>,
        Vec<BindingPattern>,
    ),
    Vec<Function>,
    (Vec<Class>, Vec<ClassMember>),
    (Vec<ArrayLiteralEntry>, Vec<PropertyDefinition>),
    Vec<Symbol>,
);

pub type Ast = GenAst<AstStorage>;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Symbol {
    pub name: StringId,
    pub span: Span,
}

impl RenderAst for Symbol {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("Symbol", w)?
            .field("name", &self.name)?
            .field_debug("span", &self.span)?
            .finish();

        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum VariableKind {
    Const,
    Var,
    Let,
}

pub enum IdentOrPattern {
    Ident(NodeId<Symbol>),
    Pattern(NodeId<BindingPattern>),
}

impl RenderAst for IdentOrPattern {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            IdentOrPattern::Ident(ref label) => ctx
                .render_struct("IdentOrPattern::Ident", w)?
                .field("0", label)?
                .finish(),
            IdentOrPattern::Pattern(ref label) => ctx
                .render_struct("IdentOrPattern::Pattern", w)?
                .field("0", label)?
                .finish(),
        }

        Ok(())
    }
}

pub enum BindingPattern {
    Object {
        properties: ListHead<BindingProperty>,
        rest: Option<NodeId<Symbol>>,
    },
    Array {
        elements: Option<NodeListId<Option<NodeId<BindingElement>>>>,
        rest: Option<NodeId<IdentOrPattern>>,
    },
}

impl RenderAst for BindingPattern {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            BindingPattern::Object {
                ref properties,
                ref rest,
            } => ctx
                .render_struct("BindingPattern::Object", w)?
                .field("properties", properties)?
                .field("rest", rest)?
                .finish(),
            BindingPattern::Array {
                ref elements,
                ref rest,
            } => ctx
                .render_struct("BindingPattern::Array", w)?
                .field("elements", elements)?
                .field("rest", rest)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum PropertyName {
    Ident(StringId),
    String(StringId),
    Number(NumberId),
    Computed(NodeId<Expr>),
}

impl RenderAst for PropertyName {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            PropertyName::Ident(ref x) => ctx
                .render_struct("PropertyName::Ident", w)?
                .field("0", x)?
                .finish(),
            PropertyName::String(ref x) => ctx
                .render_struct("PropertyName::String", w)?
                .field("0", x)?
                .finish(),
            PropertyName::Number(ref x) => ctx
                .render_struct("PropertyName::Number", w)?
                .field("0", x)?
                .finish(),
            PropertyName::Computed(ref x) => ctx
                .render_struct("PropertyName::Computed", w)?
                .field("0", x)?
                .finish(),
        }
        Ok(())
    }
}

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

impl RenderAst for BindingProperty {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            BindingProperty::Binding {
                ref symbol,
                ref initializer,
            } => ctx
                .render_struct("BindingProperty::Binding", w)?
                .field("symbol", symbol)?
                .field("initializer", initializer)?
                .finish(),
            BindingProperty::Property {
                ref name,
                ref element,
            } => ctx
                .render_struct("BindingProperty::Property", w)?
                .field("name", name)?
                .field("element", element)?
                .finish(),
        }
        Ok(())
    }
}

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

impl RenderAst for BindingElement {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            BindingElement::SingleName {
                ref symbol,
                ref initializer,
            } => ctx
                .render_struct("BindingElement::SingleName", w)?
                .field("symbol", symbol)?
                .field("initializer", initializer)?
                .finish(),
            BindingElement::Pattern {
                ref pattern,
                ref initializer,
            } => ctx
                .render_struct("BindingElement::Pattern", w)?
                .field("pattern", pattern)?
                .field("initializer", initializer)?
                .finish(),
        }
        Ok(())
    }
}

pub struct VariableDecl {
    pub decl: NodeId<IdentOrPattern>,
    pub initializer: Option<NodeId<Expr>>,
}

impl RenderAst for VariableDecl {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("VariableDecl", w)?
            .field("decl", &self.decl)?
            .field("initializer", &self.initializer)?
            .finish();

        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Stmt {
    Block {
        list: ListHead<Stmt>,
    },
    VariableDecl {
        kind: VariableKind,
        decl: ListId<VariableDecl>,
    },
    Empty,
    Expr {
        expr: ListId<Expr>,
    },
    DoWhile {
        body: NodeId<Stmt>,
        cond: ListId<Expr>,
    },
    If {
        cond: ListId<Expr>,
        body: NodeId<Stmt>,
        r#else: Option<NodeId<Stmt>>,
    },
    While {
        cond: ListId<Expr>,
        body: NodeId<Stmt>,
    },
    For {
        head: NodeId<ForLoopHead>,
        body: NodeId<Stmt>,
    },
    Switch {
        cond: ListId<Expr>,
        cases: ListHead<CaseItem>,
        default: Option<ListHead<Stmt>>,
    },
    Throw {
        expr: ListId<Expr>,
    },
    Try {
        block: ListHead<Stmt>,
        catch: Option<NodeId<CatchStmt>>,
        finally: Option<ListHead<Stmt>>,
    },
    With {
        expr: ListId<Expr>,
        stmt: NodeId<Stmt>,
    },
    Break {
        label: Option<StringId>,
    },
    Continue {
        label: Option<StringId>,
    },
    Return {
        expr: Option<ListId<Expr>>,
    },
    Labeled {
        label: StringId,
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

impl RenderAst for Stmt {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            Stmt::Block { ref list } => ctx
                .render_struct("Stmt::Block", w)?
                .field("list", list)?
                .finish(),
            Stmt::VariableDecl { ref kind, ref decl } => ctx
                .render_struct("Stmt::VariableDecl", w)?
                .field_debug("kind", kind)?
                .field("decl", decl)?
                .finish(),
            Stmt::Empty => ctx.render_struct("Stmt::Empty", w)?.finish(),
            Stmt::Expr { ref expr } => ctx
                .render_struct("Stmt::Expr", w)?
                .field("expr", expr)?
                .finish(),
            Stmt::DoWhile { ref body, ref cond } => ctx
                .render_struct("Stmt::DoWhile", w)?
                .field("body", body)?
                .field("cond", cond)?
                .finish(),
            Stmt::If {
                ref cond,
                ref body,
                ref r#else,
            } => ctx
                .render_struct("Stmt::If", w)?
                .field("cond", cond)?
                .field("body", body)?
                .field("else", r#else)?
                .finish(),
            Stmt::While { ref cond, ref body } => ctx
                .render_struct("Stmt::While", w)?
                .field("cond", cond)?
                .field("body", body)?
                .finish(),
            Stmt::For { ref head, ref body } => ctx
                .render_struct("Stmt::For", w)?
                .field("head", head)?
                .field("body", body)?
                .finish(),
            Stmt::Switch {
                ref cond,
                ref cases,
                ref default,
            } => ctx
                .render_struct("Stmt::Switch", w)?
                .field("cond", cond)?
                .field("cases", cases)?
                .field("default", default)?
                .finish(),
            Stmt::Try {
                ref block,
                ref catch,
                ref finally,
            } => ctx
                .render_struct("Stmt::Try", w)?
                .field("block", block)?
                .field("catch", catch)?
                .field("finally", finally)?
                .finish(),
            Stmt::With { ref expr, ref stmt } => ctx
                .render_struct("Stmt::With", w)?
                .field("expr", expr)?
                .field("stmt", stmt)?
                .finish(),
            Stmt::Break { ref label } => ctx
                .render_struct("Stmt::Break", w)?
                .field("label", label)?
                .finish(),
            Stmt::Continue { ref label } => ctx
                .render_struct("Stmt::Continue", w)?
                .field("label", label)?
                .finish(),
            Stmt::Throw { ref expr } => ctx
                .render_struct("Stmt::Throw", w)?
                .field("expr", expr)?
                .finish(),
            Stmt::Return { ref expr } => ctx
                .render_struct("Stmt::Return", w)?
                .field("expr", expr)?
                .finish(),
            Stmt::Labeled {
                ref label,
                ref stmt,
            } => ctx
                .render_struct("Stmt::Labeled", w)?
                .field("label", label)?
                .field("stmt", stmt)?
                .finish(),
            Stmt::Debugger => ctx.render_struct("Stmt::Debugger", w)?.finish(),
            Stmt::Function { ref func } => ctx
                .render_struct("Stmt::Function", w)?
                .field("func", func)?
                .finish(),
            Stmt::Class { ref class } => ctx
                .render_struct("Stmt::Class", w)?
                .field("class", class)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum CstyleDecl {
    Expr(ListId<Expr>),
    Decl {
        kind: VariableKind,
        decl: ListId<VariableDecl>,
    },
    Empty,
}
impl RenderAst for CstyleDecl {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            CstyleDecl::Expr(ref x) => ctx
                .render_struct("CstyleDecl::Expr", w)?
                .field("0", x)?
                .finish(),
            CstyleDecl::Decl { ref kind, ref decl } => ctx
                .render_struct("CstyleDecl::Decl", w)?
                .field_debug("kind", kind)?
                .field("decl", decl)?
                .finish(),
            CstyleDecl::Empty => ctx.render_struct("CstyleDecl::Empty", w)?.finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum InOfDecl {
    Expr(NodeId<Expr>),
    Decl {
        kind: VariableKind,
        binding: NodeId<IdentOrPattern>,
    },
}

impl RenderAst for InOfDecl {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            InOfDecl::Expr(ref x) => ctx
                .render_struct("InOfDecl::Expr", w)?
                .field("0", x)?
                .finish(),
            InOfDecl::Decl {
                ref kind,
                ref binding,
            } => ctx
                .render_struct("InOfDecl::Decl", w)?
                .field_debug("kind", kind)?
                .field("binding", binding)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum ForLoopHead {
    CStyle {
        decl: CstyleDecl,
        cond: Option<ListId<Expr>>,
        post: Option<ListId<Expr>>,
    },
    In {
        decl: InOfDecl,
        expr: ListId<Expr>,
    },
    Of {
        decl: InOfDecl,
        expr: NodeId<Expr>,
    },
}

impl RenderAst for ForLoopHead {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            ForLoopHead::CStyle {
                ref decl,
                ref cond,
                ref post,
            } => ctx
                .render_struct("ForLoopKind::CStyle", w)?
                .field("decl", decl)?
                .field("cond", cond)?
                .field("post", post)?
                .finish(),
            ForLoopHead::In { ref decl, ref expr } => ctx
                .render_struct("ForLoopKind::In", w)?
                .field("decl", decl)?
                .field("expr", expr)?
                .finish(),
            ForLoopHead::Of { ref decl, ref expr } => ctx
                .render_struct("ForLoopKind::Of", w)?
                .field("decl", decl)?
                .field("expr", expr)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct CaseItem {
    pub expr: ListId<Expr>,
    pub stmts: ListHead<Stmt>,
}

impl RenderAst for CaseItem {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("CaseItem", w)?
            .field("expr", &self.expr)?
            .field("stmts", &self.stmts)?
            .finish();
        Ok(())
    }
}

pub struct CatchStmt {
    pub binding: Option<NodeId<IdentOrPattern>>,
    pub block: ListHead<Stmt>,
}

impl RenderAst for CatchStmt {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("CatchStmt", w)?
            .field("binding", &self.binding)?
            .field("block", &self.block)?
            .finish();
        Ok(())
    }
}

pub struct Parameters {
    rest: bool,
    binding: NodeId<IdentOrPattern>,
    initializer: Option<NodeId<Expr>>,
}

impl RenderAst for Parameters {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("Parameters", w)?
            .field_debug("rest", &self.rest)?
            .field("binding", &self.binding)?
            .field("initializer", &self.initializer)?
            .finish();

        Ok(())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ArrowFunctionBody {
    Expr(NodeId<Expr>),
    Stmt(ListHead<Stmt>),
}

impl RenderAst for ArrowFunctionBody {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            ArrowFunctionBody::Expr(ref x) => ctx
                .render_struct("ArrowFunctionBody::Expr", w)?
                .field("0", x)?
                .finish(),
            ArrowFunctionBody::Stmt(ref x) => ctx
                .render_struct("ArrowFunctionBody::Stmt", w)?
                .field("0", x)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionKind {
    Simple,
    Async,
    Generator,
    AsyncGenerator,
}

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

#[derive(Debug)]
pub enum Function {
    Arrow {
        is_strict: bool,
        kind: FunctionKind,
        params: ListHead<BindingElement>,
        rest_param: Option<NodeId<IdentOrPattern>>,
        body: ArrowFunctionBody,
    },
    Declared {
        is_strict: bool,
        kind: FunctionKind,
        name: NodeId<Symbol>,
        params: ListHead<BindingElement>,
        rest_param: Option<NodeId<IdentOrPattern>>,
        body: ListHead<Stmt>,
    },
    Expr {
        is_strict: bool,
        kind: FunctionKind,
        name: Option<NodeId<Symbol>>,
        params: ListHead<BindingElement>,
        rest_param: Option<NodeId<IdentOrPattern>>,
        body: ListHead<Stmt>,
    },
}

impl RenderAst for Function {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            Function::Arrow {
                ref is_strict,
                ref kind,
                ref params,
                ref rest_param,
                ref body,
            } => ctx
                .render_struct("Function::Arrow", w)?
                .field_debug("is_strict", is_strict)?
                .field_debug("kind", kind)?
                .field("params", params)?
                .field("rest_param", rest_param)?
                .field("body", body)?
                .finish(),
            Function::Declared {
                ref is_strict,
                ref kind,
                ref name,
                ref params,
                ref rest_param,
                ref body,
            } => ctx
                .render_struct("Function::Declared", w)?
                .field_debug("is_strict", is_strict)?
                .field_debug("kind", kind)?
                .field("name", name)?
                .field("params", params)?
                .field("rest_param", rest_param)?
                .field("body", body)?
                .finish(),
            Function::Expr {
                ref is_strict,
                ref kind,
                ref name,
                ref params,
                ref rest_param,
                ref body,
            } => ctx
                .render_struct("Function::Expr", w)?
                .field_debug("is_strict", is_strict)?
                .field_debug("kind", kind)?
                .field("name", name)?
                .field("params", params)?
                .field("rest_param", rest_param)?
                .field("body", body)?
                .finish(),
        }

        Ok(())
    }
}

pub struct Class {
    pub name: Option<NodeId<Symbol>>,
    pub heritage: Option<NodeId<Expr>>,
    pub body: ListHead<ClassMember>,
}

impl RenderAst for Class {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("Class", w)?
            .field("name", &self.name)?
            .field("heritage", &self.heritage)?
            .field("body", &self.body)?
            .finish();
        Ok(())
    }
}

pub enum ClassMember {
    StaticBlock {
        stmts: ListHead<Stmt>,
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

impl RenderAst for ClassMember {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            ClassMember::StaticBlock { ref stmts } => ctx
                .render_struct("ClassMember::StaticBlock", w)?
                .field("stmts", stmts)?
                .finish(),
            ClassMember::Method {
                ref is_static,
                ref property,
                ref func,
            } => ctx
                .render_struct("ClassMember::Method", w)?
                .field_debug("is_static", is_static)?
                .field("property", property)?
                .field("func", func)?
                .finish(),
            ClassMember::Getter {
                ref is_static,
                ref property,
                ref func,
            } => ctx
                .render_struct("ClassMember::Getter", w)?
                .field_debug("is_static", is_static)?
                .field("property", property)?
                .field("func", func)?
                .finish(),
            ClassMember::Setter {
                ref is_static,
                ref property,
                ref func,
            } => ctx
                .render_struct("ClassMember::Setter", w)?
                .field_debug("is_static", is_static)?
                .field("property", property)?
                .field("func", func)?
                .finish(),
            ClassMember::Field {
                ref is_static,
                ref property,
                ref initializer,
            } => ctx
                .render_struct("ClassMember::Field", w)?
                .field_debug("is_static", is_static)?
                .field("property", property)?
                .field("initializer", initializer)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
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

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
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

impl AssignOp {
    /// Returns true if the operation requires a load.
    pub fn loads(self) -> bool {
        self != AssignOp::Assign
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum BinaryOp {
    Base(BaseOp),
    Assign(AssignOp),
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum PostfixOp {
    AddOne,
    SubOne,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
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

pub struct Argument {
    pub is_spread: bool,
    pub expr: NodeId<Expr>,
}

impl RenderAst for Argument {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("Argument", w)?
            .field_debug("is_spread", &self.is_spread)?
            .field("expr", &self.expr)?
            .finish();
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
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
    Tenary(NodeId<Tenary>),
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

impl RenderAst for Expr {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            Expr::Binary {
                ref op,
                ref left,
                ref right,
            } => ctx
                .render_struct("Expr::Binary", w)?
                .field_debug("op", op)?
                .field("left", left)?
                .field("right", right)?
                .finish(),
            Expr::Prefix { ref op, ref expr } => ctx
                .render_struct("Expr::Prefix", w)?
                .field_debug("op", op)?
                .field("expr", expr)?
                .finish(),
            Expr::Postfix { ref op, ref expr } => ctx
                .render_struct("Expr::Postfix", w)?
                .field_debug("op", op)?
                .field("expr", expr)?
                .finish(),
            Expr::Index {
                ref index,
                ref expr,
            } => ctx
                .render_struct("Expr::Index", w)?
                .field("index", index)?
                .field("expr", expr)?
                .finish(),
            Expr::Dot {
                ref ident,
                ref expr,
            } => ctx
                .render_struct("Expr::Dot", w)?
                .field("ident", ident)?
                .field("expr", expr)?
                .finish(),
            Expr::Call { ref args, ref expr } => ctx
                .render_struct("Expr::Call", w)?
                .field("args", args)?
                .field("expr", expr)?
                .finish(),
            Expr::Prime { ref expr } => ctx
                .render_struct("Expr::Prime", w)?
                .field("expr", expr)?
                .finish(),
            Expr::Yield { ref expr, ref star } => ctx
                .render_struct("Expr::Prime", w)?
                .field_debug("star", star)?
                .field("expr", expr)?
                .finish(),
            Expr::Tenary(ref x) => ctx
                .render_struct("Expr::Tenary", w)?
                .field("0", x)?
                .finish(),
            Expr::Destructure {
                ref pattern,
                ref expr,
            } => ctx
                .render_struct("Expr::Destructure", w)?
                .field("pattern", pattern)?
                .field("expr", expr)?
                .finish(),
            Expr::TaggedTemplate {
                ref tag,
                ref template,
            } => ctx
                .render_struct("Expr::TaggedTemplate", w)?
                .field("tag", tag)?
                .field("template", template)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct Tenary {
    pub cond: NodeId<Expr>,
    pub then: NodeId<Expr>,
    pub r#else: NodeId<Expr>,
}

impl RenderAst for Tenary {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("Tenary", w)?
            .field("cond", &self.cond)?
            .field("then", &self.then)?
            .field("else", &self.r#else)?
            .finish();

        Ok(())
    }
}

pub struct ArrayLiteral {
    pub elements: ListHead<ArrayLiteralEntry>,
}

pub struct ArrayLiteralEntry {
    pub expr: Option<NodeId<Expr>>,
    pub is_spread: bool,
}

impl RenderAst for ArrayLiteralEntry {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("ArrayLiteralEntry", w)?
            .field("expr", &self.expr)?
            .field_debug("is_spread", &self.is_spread)?
            .finish();

        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum PrimeExpr {
    Number(NumberId),
    String(StringId),
    Template(NodeId<Template>),
    Regex(StringId),
    Ident(NodeId<Symbol>),
    Boolean(bool),
    Function(NodeId<Function>),
    Class(NodeId<Class>),
    Object(ObjectLiteral),
    Array(ListHead<ArrayLiteralEntry>),
    NewTarget,
    Null,
    This,
    Super,
    Covered(ListId<Expr>),
}

impl RenderAst for PrimeExpr {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            PrimeExpr::Number(ref x) => ctx
                .render_struct("PrimeExpr::Number", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::String(ref x) => ctx
                .render_struct("PrimeExpr::String", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::Template(ref x) => ctx
                .render_struct("PrimeExpr::Template", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::Regex(ref x) => ctx
                .render_struct("PrimeExpr::Regex", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::Ident(ref x) => ctx
                .render_struct("PrimeExpr::Ident", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::Boolean(ref x) => ctx
                .render_struct("PrimeExpr::Boolean", w)?
                .field_debug("0", x)?
                .finish(),
            PrimeExpr::Covered(ref x) => ctx
                .render_struct("PrimeExpr::Covered", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::Function(ref x) => ctx
                .render_struct("PrimeExpr::Function", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::Class(ref x) => ctx
                .render_struct("PrimeExpr::Class", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::Null => ctx.render_struct("PrimeExpr::Null", w)?.finish(),
            PrimeExpr::This => ctx.render_struct("PrimeExpr::This", w)?.finish(),
            PrimeExpr::Super => ctx.render_struct("PrimeExpr::Super", w)?.finish(),
            PrimeExpr::NewTarget => ctx.render_struct("PrimeExpr::NewTarget", w)?.finish(),
            PrimeExpr::Object(ObjectLiteral::Empty) => {
                ctx.render_struct("PrimeExpr::Object", w)?.finish()
            }
            PrimeExpr::Object(ObjectLiteral::Item(ref x)) => ctx
                .render_struct("PrimeExpr::Object", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::Array(ref id) => ctx
                .render_struct("PrimeExpr::Array", w)?
                .field("0", id)?
                .finish(),
        }
        Ok(())
    }
}

pub enum Template {
    Head {
        text: StringId,
        expr: ListId<Expr>,
        next: NodeId<Template>,
    },
    Tail {
        text: StringId,
    },
}

impl RenderAst for Template {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            Self::Head {
                ref text,
                ref expr,
                ref next,
            } => ctx
                .render_struct("Template::Head", w)?
                .field("text", text)?
                .field("expr", expr)?
                .field("next", next)?
                .finish(),
            Self::Tail { ref text } => ctx
                .render_struct("Template::Tail", w)?
                .field("text", text)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq)]
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
    Rest(NodeId<Expr>),
}

impl RenderAst for PropertyDefinition {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        match *self {
            PropertyDefinition::Ident { ref ident } => ctx
                .render_struct("PropertyDefinition::Ident", w)?
                .field("ident", ident)?
                .finish(),
            PropertyDefinition::Covered {
                symbol: ref ident,
                ref initializer,
            } => ctx
                .render_struct("PropertyDefinition::Covered", w)?
                .field("ident", ident)?
                .field("initializer", initializer)?
                .finish(),
            PropertyDefinition::Define {
                ref property,
                ref expr,
            } => ctx
                .render_struct("PropertyDefinition::Define", w)?
                .field("property", property)?
                .field("expr", expr)?
                .finish(),
            PropertyDefinition::Method {
                ref property,
                ref func,
            } => ctx
                .render_struct("PropertyDefinition::Method", w)?
                .field("property", property)?
                .field("func", func)?
                .finish(),
            PropertyDefinition::Setter {
                ref property,
                ref func,
            } => ctx
                .render_struct("PropertyDefinition::Setter", w)?
                .field("property", property)?
                .field("func", func)?
                .finish(),
            PropertyDefinition::Getter {
                ref property,
                ref func,
            } => ctx
                .render_struct("PropertyDefinition::Getter", w)?
                .field("property", property)?
                .field("func", func)?
                .finish(),
            PropertyDefinition::Rest(ref x) => ctx
                .render_struct("PropertyDefinition::Rest", w)?
                .field("0", x)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ObjectLiteral {
    Empty,
    Item(ListId<PropertyDefinition>),
}

impl RenderAst for Span {
    fn render<W: fmt::Write>(&self, ctx: &RenderCtx, w: &mut W) -> Result<()> {
        ctx.render_struct("Span", w)?
            .field_debug("offset", &self.offset())?
            .field_debug("size", &self.size())?
            .finish();
        Ok(())
    }
}
