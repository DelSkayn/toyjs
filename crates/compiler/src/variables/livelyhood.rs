use ast::{visitor::Visitor, Ast};

use super::{ScopeId, SymbolUseOrder, Variables};
use crate::Error;

/// Struct for running livelyhood analysis on symbols.
pub struct Livelyhood<'a, 'b> {
    ast: &'a Ast,
    variables: &'b mut Variables,
    current_use: SymbolUseOrder,
    root: ScopeId,
    current: ScopeId,
    current_function: ScopeId,
}

impl<'a, 'b> Livelyhood<'a, 'b> {
    pub fn new(ast: &'a Ast, variables: &'b mut Variables, root: ScopeId) -> Self {
        let current_function = variables.function_of(root);
        Livelyhood {
            ast,
            variables,
            current_use: SymbolUseOrder::first(),
            current: root,
            current_function,
            root,
        }
    }
}

impl Visitor<Error> for Livelyhood<'_, '_> {
    fn ast(&self) -> &Ast {
        self.ast
    }
}
