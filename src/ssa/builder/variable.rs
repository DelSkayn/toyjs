use crate::{
    interner::StringId,
    util::{Index, OptionIndex},
};
use fxhash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum BindingType {
    /// Binded with `var` or implicitly
    Var,
    /// Binded with `let`
    Let,
    /// Binded with `const`
    Const,
}

#[derive(Debug)]
pub struct Variable {
    name: StringId,
    /// Whether the variable is captured in a closure.
    captured: bool,
    implicit: bool,
    scope: Index,
    binding: BindingType,
}

#[derive(Copy, Clone, Debug)]
pub struct ImplicitVariable {
    id: Index,
    top_function_scope: Index,
}

#[derive(Debug)]
pub struct VariableTable {
    scopes: Vec<ScopeData>,
    variable: Vec<Variable>,
    current_scope: Index,
    top_function_scope: Index,
    implicits: FxHashMap<StringId, Vec<ImplicitVariable>>,
}

#[derive(Debug)]
pub struct Variables {
    scopes: Vec<ScopeData>,
    variable: Vec<Variable>,
}

shrinkwrap_index!(VariableId, OptionVariableId);

#[derive(Debug)]
pub struct ScopeData {
    table: FxHashMap<StringId, Index>,
    function_boundry: bool,
    parent: OptionIndex,
    children: Vec<Index>,
}

impl VariableTable {
    pub fn new() -> Self {
        VariableTable {
            scopes: vec![ScopeData {
                table: FxHashMap::default(),
                function_boundry: true,
                parent: OptionIndex::none(),
                children: Vec::new(),
            }],
            current_scope: Index(0),
            top_function_scope: Index(0),
            variable: Vec::new(),
            implicits: FxHashMap::default(),
        }
    }

    pub fn into_variables(self) -> Variables {
        Variables {
            scopes: self.scopes,
            variable: self.variable,
        }
    }

    pub fn reference(&mut self, ident: StringId) -> VariableId {
        let mut cur_scope = self.current_scope;
        let mut crossed_function_boundery = false;
        loop {
            if let Some(x) = self.scopes[cur_scope.into_usize()]
                .table
                .get(&ident)
                .copied()
            {
                self.variable[x.into_usize()].captured =
                    self.variable[x.into_usize()].captured | crossed_function_boundery;
                return VariableId(x);
            }
            let n_scope = self.scopes[cur_scope.into_usize()].parent;
            crossed_function_boundery =
                crossed_function_boundery | self.scopes[cur_scope.into_usize()].function_boundry;
            if n_scope == OptionIndex::none() {
                return self.declare_implicit(ident);
            }
            cur_scope = n_scope.unwrap();
        }
    }

    fn declare_implicit(&mut self, ident: StringId) -> VariableId {
        let variable = Variable {
            name: ident,
            captured: false,
            scope: Index(0),
            implicit: true,
            binding: BindingType::Var,
        };
        let id = Index::from(self.variable.len());
        self.variable.push(variable);
        self.scopes[0].table.insert(ident, id);
        self.implicits
            .entry(ident)
            .or_insert_with(Vec::new)
            .push(ImplicitVariable {
                id,
                top_function_scope: self.top_function_scope,
            });
        VariableId(id)
    }

    pub fn declare(&mut self, name: StringId, binding: BindingType) -> OptionVariableId {
        let scope = match binding {
            BindingType::Let | BindingType::Const => self.current_scope,
            BindingType::Var => self.top_function_scope,
        };
        if binding == BindingType::Var {
            if let Some(x) = self.implicits.get_mut(&name) {
                let top_function_scope = self.top_function_scope;
                let find = x
                    .iter()
                    .enumerate()
                    .find(|(_, x)| x.top_function_scope == top_function_scope)
                    .map(|(idx, x)| (idx, *x));
                if let Some((idx, _)) = find {
                    let v = x.swap_remove(idx);
                    if x.is_empty() {
                        self.implicits.remove(&name);
                    }
                    self.scopes[0].table.remove(&name);
                    self.variable[v.id.into_usize()].implicit = false;
                    self.variable[v.id.into_usize()].scope = self.top_function_scope;
                    self.scopes[self.top_function_scope.into_usize()]
                        .table
                        .insert(name, v.id);
                    return OptionVariableId::some(VariableId(v.id));
                }
            }
        }
        let index = Index::from(self.variable.len());
        match self.scopes[scope.into_usize()].table.entry(name) {
            Entry::Vacant(x) => {
                x.insert(index);
            }
            Entry::Occupied(x) => {
                if binding == BindingType::Var {
                    return OptionVariableId::some(VariableId(*x.get()));
                }
                return OptionVariableId::none();
            }
        }
        let variable = Variable {
            name,
            binding,
            implicit: false,
            scope,
            captured: false,
        };
        self.variable.push(variable);
        self.scopes[scope.into_usize()].table.insert(name, index);
        OptionVariableId::some(VariableId(index))
    }

    pub fn push_scope(&mut self) {
        let new_scope = ScopeData {
            table: FxHashMap::default(),
            function_boundry: true,
            parent: OptionIndex::some(self.current_scope),
            children: Vec::new(),
        };
        let new_index = Index::from(self.scopes.len());
        self.scopes[self.current_scope.into_usize()]
            .children
            .push(Index::from(new_index));
        self.scopes.push(new_scope);
        self.current_scope = new_index;
    }

    pub fn push_function_scope(&mut self) {
        let new_scope = ScopeData {
            table: FxHashMap::default(),
            function_boundry: true,
            parent: OptionIndex::some(self.current_scope),
            children: Vec::new(),
        };
        let new_index = Index::from(self.scopes.len());
        self.scopes[self.current_scope.into_usize()]
            .children
            .push(Index::from(new_index));
        self.scopes.push(new_scope);
        self.current_scope = new_index;
        self.top_function_scope = new_index;
    }

    pub fn pop_scope(&mut self) {
        let parent = self.scopes[self.current_scope.into_usize()].parent;
        assert_ne!(parent, OptionIndex::none(), "tried to pop root scope");
        self.current_scope = parent.unwrap();
        self.top_function_scope = self.current_scope;
        while !self.scopes[self.top_function_scope.into_usize()].function_boundry {
            self.top_function_scope = self.scopes[self.top_function_scope.into_usize()]
                .parent
                .unwrap()
        }
    }
}
