#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOpToken {
    /// -
    Minus,
    /// +
    Plus,
    /// *
    Mul,
    /// **
    Exponentiate,
    /// /
    Div,
    /// //
    IntegerDiv,
    /// %
    Remainder,
    /// <<
    LeftShift,
    /// >>
    RightShift,
    /// >>>
    UnsignedRightShift,
    /// &
    BitwiseAnd,
    /// ^
    BitwiseXor,
    /// |
    BitwiseOr,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOpToken {
    /// -
    Negative,
    /// +
    Positive,
    /// ++
    AddOne,
    /// --
    SubractOne,
    /// ~
    BitwiseNot,
    /// !
    Not,
    /// delete
    Delete,
    /// void
    Void,
    /// typeof
    Typeof,
}
