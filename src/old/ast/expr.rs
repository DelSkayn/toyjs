use super::*;

#[derive(Debug)]
pub enum PrimeExpr {
    Literal(Literal),
    BinInt,
    Ident(Ident),
    This,
    Null,
    Boolean(bool),
    Function {
        binding: Option<Ident>,
        params: Parameters,
        block: Block,
    },
    ObjectLiteral {
        properties: Vec<Property>,
    },
    ArrayLiteral {
        elems: Vec<ArrayElement>,
        rest: Option<Box<AssignExpr>>,
    },
    Class(Box<Class>),
    Generator {
        binding: Option<Ident>,
        params: Parameters,
        block: Block,
    },
    ParamList {
        expr: Option<Expr>,
        rest: Option<Binding>,
    },
    AsyncFunc,
    AsyncGen,
    Regular,
    Template,
}

#[derive(Debug)]
pub enum PrefixOp {
    Delete,
    Void,
    Typeof,
    Positive,
    Negative,
    BitwiseNot,
    Not,
    Increment,
    Decrement,
    New,
}

impl PrefixOp {
    pub fn from_token(token: Token) -> Option<Self> {
        match token.kind {
            tok!("delete") => Some(PrefixOp::Delete),
            tok!("void") => Some(PrefixOp::Void),
            tok!("typeof") => Some(PrefixOp::Typeof),
            tok!("+") => Some(PrefixOp::Positive),
            tok!("-") => Some(PrefixOp::Negative),
            tok!("~") => Some(PrefixOp::BitwiseNot),
            tok!("!") => Some(PrefixOp::Not),
            tok!("++") => Some(PrefixOp::Increment),
            tok!("--") => Some(PrefixOp::Decrement),
            tok!("new") => Some(PrefixOp::New),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum PostfixOp {
    Increment,
    Decrement,
    Index(Expr),
    Call(Arguments),
}

#[derive(Debug)]
pub enum AssignOp {
    Assign,
    Mul,
    Div,
    Modulo,
    Sub,
    Add,
    ShiftLeft,
    ShiftRight,
    UnsignedShiftRight,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    Power,
}

#[derive(Debug)]
pub enum BinOp {
    Tenary(Box<AssignExpr>),
    Assign(AssignOp),
    Coalesce,
    Or,
    And,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equal,
    NotEqual,
    StrictEqual,
    StrictNotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    InstanceOf,
    In,
    ShiftLeft,
    ShiftRight,
    UnsignedShiftRight,
    Plus,
    Minus,
    Mul,
    Modulo,
    Div,
    Power,
    Option,
    Dot,
}

impl BinOp {
    pub fn from_token(token: Token) -> Option<Self> {
        match token.kind {
            tok!("=") => Some(BinOp::Assign(AssignOp::Assign)),
            tok!("*=") => Some(BinOp::Assign(AssignOp::Mul)),
            tok!("/=") => Some(BinOp::Assign(AssignOp::Div)),
            tok!("%=") => Some(BinOp::Assign(AssignOp::Modulo)),
            tok!("+=") => Some(BinOp::Assign(AssignOp::Add)),
            tok!("-=") => Some(BinOp::Assign(AssignOp::Sub)),
            tok!("<<=") => Some(BinOp::Assign(AssignOp::ShiftLeft)),
            tok!(">>=") => Some(BinOp::Assign(AssignOp::ShiftRight)),
            tok!(">>>=") => Some(BinOp::Assign(AssignOp::UnsignedShiftRight)),
            tok!("&=") => Some(BinOp::Assign(AssignOp::BitwiseAnd)),
            tok!("^=") => Some(BinOp::Assign(AssignOp::BitwiseXor)),
            tok!("|=") => Some(BinOp::Assign(AssignOp::BitwiseOr)),
            tok!("**=") => Some(BinOp::Assign(AssignOp::Power)),
            tok!("??") => Some(BinOp::Coalesce),
            tok!("||") => Some(BinOp::Or),
            tok!("&&") => Some(BinOp::And),
            tok!("|") => Some(BinOp::BitwiseOr),
            tok!("^") => Some(BinOp::BitwiseXor),
            tok!("&") => Some(BinOp::BitwiseAnd),
            tok!("==") => Some(BinOp::Equal),
            tok!("!=") => Some(BinOp::NotEqual),
            tok!("===") => Some(BinOp::StrictEqual),
            tok!("!==") => Some(BinOp::StrictNotEqual),
            tok!("<") => Some(BinOp::Less),
            tok!(">") => Some(BinOp::Greater),
            tok!("<=") => Some(BinOp::LessEqual),
            tok!(">=") => Some(BinOp::GreaterEqual),
            tok!("instanceof") => Some(BinOp::InstanceOf),
            tok!("in") => Some(BinOp::In),
            tok!("<<") => Some(BinOp::ShiftLeft),
            tok!(">>") => Some(BinOp::ShiftRight),
            tok!(">>>") => Some(BinOp::UnsignedShiftRight),
            tok!("+") => Some(BinOp::Plus),
            tok!("-") => Some(BinOp::Minus),
            tok!("*") => Some(BinOp::Mul),
            tok!("%") => Some(BinOp::Modulo),
            tok!("/") => Some(BinOp::Div),
            tok!("**") => Some(BinOp::Power),
            tok!("?.") => Some(BinOp::Option),
            tok!(".") => Some(BinOp::Dot),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum AssignExpr {
    Prefix {
        op: PrefixOp,
        expr: Box<AssignExpr>,
    },
    Postfix {
        expr: Box<AssignExpr>,
        op: PostfixOp,
    },
    Bin {
        lhs: Box<AssignExpr>,
        op: BinOp,
        rhs: Box<AssignExpr>,
    },
    Prime(PrimeExpr),
    Covered(Expr),
    NewTarget,
    Super,
    Import,
    ImportMeta,
}

impl AssignExpr {
    /// Wether this production is an assignable left hand side expression.
    pub fn is_assign_lhs(&self) -> bool {
        match *self {
            AssignExpr::Prime(ref p) => match p {
                PrimeExpr::Ident(_) => true,
                PrimeExpr::This => true,
                _ => false,
            },
            AssignExpr::Covered(_) => true,
            AssignExpr::Postfix { ref op, expr: _ } => match op {
                PostfixOp::Index(_) => true,
                _ => false,
            },
            AssignExpr::Bin {
                ref lhs,
                ref op,
                ref rhs,
            } => match op {
                BinOp::Dot | BinOp::Option => return lhs.is_assign_lhs() && rhs.is_assign_lhs(),
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Expr {
    pub exprs: Vec<AssignExpr>,
}
