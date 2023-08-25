pub enum Kind {
    /// Defined with the side effect of introducing new global variables.
    Global,
    /// Define within the function scope.
    Function,
    /// Defined with let
    Let,
    /// Defined with const
    Const,
    /// Variable is used without ever being declared.
    Unresolved,
}

pub struct Lifetime {
    /// When the variable is first defined.
    definition: NodeId<ast::Expression>,
    /// When the variable is last used.
    last_use: NodeId<ast::Expression>,
}

pub struct VariableInfo {
    /// The identifier of the variable.
    ident: StringId,
    /// Is the variable captured by a closure.
    captured: bool,
    /// How is the variable declared.
    kind: Kind,
    /// How long does the variable life.
    lifetime: Option<Lifetime>,
}

impl<'a> Compiler<'a> {}
