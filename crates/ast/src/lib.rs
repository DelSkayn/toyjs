#![allow(dead_code)]

mod ast;
pub use ast::{Ast as GenAst, List, ListHead, ListId, NodeId, NodeList};
pub use render::{RenderAst, RenderCtx};
use token::{NumberId, StringId};
mod r#macro;
mod render;

pub type AstStorage = (
    (Vec<Stmt>, Vec<CaseItem>, Vec<CatchStmt>),
    Vec<Expr>,
    Vec<PrimeExpr>,
    Vec<PropertyDefinition>,
    Vec<NodeList<ArrayLiteral>>,
);
pub type Ast = GenAst<AstStorage>;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
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

impl RenderAst for Binding {
    fn render<W: std::io::Write>(&self, ctx: &RenderCtx, w: &mut W) -> std::io::Result<()> {
        match *self {
            Binding::Ident {
                ref label,
                ref initializer,
            } => ctx
                .render_struct("Binding::Ident", w)?
                .field("label", label)?
                .field("initializer", initializer)?
                .finish(),
            Binding::Object {} => ctx.render_struct("Binding::Object", w)?.finish(),
            Binding::Array {} => ctx.render_struct("Binding::Array", w)?.finish(),
        }

        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Stmt {
    Block {
        list: ListHead<Stmt>,
    },
    VariableDecl {
        kind: VariableKind,
        decl: ListId<Binding>,
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
        head: NodeId<Expr>,
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
    Debugger,
}

impl RenderAst for Stmt {
    fn render<W: std::io::Write>(&self, ctx: &RenderCtx, w: &mut W) -> std::io::Result<()> {
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
    fn render<W: std::io::Write>(&self, ctx: &RenderCtx, w: &mut W) -> std::io::Result<()> {
        ctx.render_struct("CaseItem", w)?
            .field("expr", &self.expr)?
            .field("stmts", &self.stmts)?
            .finish();
        Ok(())
    }
}

pub struct CatchStmt {
    pub expr: Option<ListId<Expr>>,
    pub block: ListHead<Stmt>,
}

impl RenderAst for CatchStmt {
    fn render<W: std::io::Write>(&self, ctx: &RenderCtx, w: &mut W) -> std::io::Result<()> {
        ctx.render_struct("CatchStmt", w)?
            .field("expr", &self.expr)?
            .field("block", &self.block)?
            .finish();
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

impl RenderAst for Expr {
    fn render<W: std::io::Write>(&self, ctx: &RenderCtx, w: &mut W) -> std::io::Result<()> {
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
            Expr::Call {
                ref params,
                ref expr,
            } => ctx
                .render_struct("Expr::Call", w)?
                .field("params", params)?
                .field("expr", expr)?
                .finish(),
            Expr::Prime { ref expr } => ctx
                .render_struct("Expr::Prime", w)?
                .field("expr", expr)?
                .finish(),
        }
        Ok(())
    }
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
    Array(NodeId<NodeList<ArrayLiteral>>),
    This,
    NewTarget,
    Covered(NodeId<Expr>),
}

impl RenderAst for PrimeExpr {
    fn render<W: std::io::Write>(&self, ctx: &RenderCtx, w: &mut W) -> std::io::Result<()> {
        match *self {
            PrimeExpr::Number(ref x) => ctx
                .render_struct("PrimeExpr::Number", w)?
                .field("0", x)?
                .finish(),
            PrimeExpr::String(ref x) => ctx
                .render_struct("PrimeExpr::String", w)?
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
            PrimeExpr::Null => ctx.render_struct("PrimeExpr::Null", w)?.finish(),
            PrimeExpr::This => ctx.render_struct("PrimeExpr::This", w)?.finish(),
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

impl RenderAst for PropertyDefinition {
    fn render<W: std::io::Write>(&self, ctx: &RenderCtx, w: &mut W) -> std::io::Result<()> {
        match *self {
            PropertyDefinition::Ident(ref x) => ctx
                .render_struct("PropertyDefinition::Ident", w)?
                .field("0", x)?
                .finish(),
            PropertyDefinition::Covered {
                ref ident,
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
            PropertyDefinition::Method {} => {
                ctx.render_struct("PropertyDefinition::Method", w)?.finish()
            }
            PropertyDefinition::Rest(ref x) => ctx
                .render_struct("PropertyDefinition::Rest", w)?
                .field("0", x)?
                .finish(),
        }
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum PropertyName {
    Ident(StringId),
    String(StringId),
    Number(NumberId),
    Computed(NodeId<Expr>),
}
impl RenderAst for PropertyName {
    fn render<W: std::io::Write>(&self, ctx: &RenderCtx, w: &mut W) -> std::io::Result<()> {
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

#[derive(Clone, Copy, PartialEq)]
pub enum ObjectLiteral {
    Empty,
    Item(ListId<PropertyDefinition>),
}

#[derive(Clone, Copy, PartialEq)]
pub struct ArrayLiteral {
    pub expr: Option<NodeId<Expr>>,
    pub is_spread: bool,
}

impl RenderAst for ArrayLiteral {
    fn render<W: std::io::Write>(&self, ctx: &RenderCtx, w: &mut W) -> std::io::Result<()> {
        ctx.render_struct("ArrayLiteral", w)?
            .field("expr", &self.expr)?
            .field_debug("is_spread", &self.is_spread)?
            .finish();

        Ok(())
    }
}
