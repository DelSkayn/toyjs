use bumpalo::{collections::Vec, Bump};
use common::{collections::HashMap, interner::StringId, newtype_index, newtype_slice, newtype_vec};
use std::cell::{Cell, RefCell};

#[derive(Clone, Debug, Copy)]
pub enum VariableKind {
    Global,
    Implicit,
    Local,
    LocalConstant,
    Captured,
    CapturedConstant,
}

newtype_index! (
    pub struct VariableId
);

#[derive(Debug)]
pub struct VariableVec<'alloc> {
    inner: Vec<'alloc, Variable<'alloc>>,
}

newtype_vec! (
    struct VariableVec<'alloc,>.inner[VariableId] -> Variable<'alloc,>
);

#[derive(Debug)]
pub struct Variable<'alloc> {
    pub kind: VariableKind,
    pub name: StringId,
    pub scope: &'alloc Scope<'alloc>,
}

#[derive(Debug)]
pub struct Scope<'alloc> {
    parent: Option<&'alloc Scope<'alloc>>,
    parent_function: Option<&'alloc Scope<'alloc>>,
    is_function: bool,
    has_captured_variable: Cell<bool>,
    children: RefCell<Vec<'alloc, &'alloc Scope<'alloc>>>,
}

#[derive(Debug)]
pub struct Variables<'alloc> {
    root: &'alloc Scope<'alloc>,
    implicits: HashMap<StringId, VariableId>,
    current: &'alloc Scope<'alloc>,
    current_function: &'alloc Scope<'alloc>,
    alloc: &'alloc Bump,
    variables: VariableVec<'alloc>,
    variable_ids: HashMap<(*const Scope<'alloc>, StringId), VariableId>,
}

newtype_slice! (
    struct Variables<'alloc,>.variables.inner[VariableId] -> Variable<'alloc,>
);

impl<'alloc> Variables<'alloc> {
    pub fn new_in(alloc: &'alloc Bump) -> Self {
        let root = alloc.alloc(Scope {
            parent: None,
            parent_function: None,
            is_function: true,
            has_captured_variable: Cell::new(true),
            children: RefCell::new(Vec::new_in(alloc)),
        });
        Variables {
            root,
            implicits: HashMap::default(),
            current: root,
            current_function: root,
            variables: VariableVec {
                inner: Vec::new_in(alloc),
            },
            alloc,
            variable_ids: HashMap::default(),
        }
    }

    pub fn root(&self) -> &'alloc Scope<'alloc> {
        self.root
    }

    pub fn use_variable(&mut self, name: StringId) -> VariableId {
        let mut current = self.current;
        let mut crossed_function = false;
        loop {
            let key = (current as *const _, name);
            if let Some(x) = self.variable_ids.get(&key).copied() {
                if crossed_function {
                    // Variable crossed function boundery
                    // so some variable kinds should be changed to reflect that.
                    match self.variables[x].kind {
                        VariableKind::Local => {
                            if let Some(x) = self.variables[x].scope.parent_function {
                                x.has_captured_variable.set(true);
                            } else {
                                self.variables[x].scope.has_captured_variable.set(true);
                            }
                            self.variables[x].kind = VariableKind::Captured;
                        }
                        VariableKind::LocalConstant => {
                            if let Some(x) = self.variables[x].scope.parent_function {
                                x.has_captured_variable.set(true);
                            } else {
                                self.variables[x].scope.has_captured_variable.set(true);
                            }
                            self.variables[x].kind = VariableKind::CapturedConstant;
                        }
                        _ => {}
                    }
                }
                return x;
            }
            crossed_function |= current.is_function;
            if let Some(x) = current.parent {
                current = x;
            } else {
                break;
            }
        }
        // Implicit variable
        let variable = self.variables.push(Variable {
            name,
            kind: VariableKind::Implicit,
            scope: self.current_function,
        });
        self.implicits.insert(name, variable);
        variable
    }

    pub fn define_global(&mut self, name: StringId) -> VariableId {
        // Connect uses of variables to later definitions
        let key = (self.current_function as *const _, name);
        if let Some(id) = self.implicits.remove(&name) {
            self.variables[id].kind = VariableKind::Global;
            self.variable_ids.insert(key, id);
            return id;
        }
        let id = self.variables.push(Variable {
            kind: VariableKind::Global,
            name,
            scope: self.current_function,
        });
        self.variable_ids.insert(key, id);
        id
    }

    pub fn define_local(&mut self, name: StringId, constant: bool) -> VariableId {
        let ty = if constant {
            VariableKind::LocalConstant
        } else {
            VariableKind::Local
        };
        let key = (self.current as *const _, name);
        // Connect uses of variables to later definitions
        if let Some(id) = self.implicits.remove(&name) {
            self.variables[id].kind = ty;
            self.variable_ids.insert(key, id);
            return id;
        }

        let id = self.variables.push(Variable {
            kind: ty,
            name,
            scope: self.current,
        });
        id
    }

    pub fn push_scope(&mut self) -> &'alloc Scope<'alloc> {
        let scope = Scope {
            parent: Some(self.current),
            parent_function: Some(self.current_function),
            is_function: false,
            has_captured_variable: Cell::new(false),
            children: RefCell::new(Vec::new_in(self.alloc)),
        };
        let scope_ref = self.alloc.alloc(scope);
        self.current.children.borrow_mut().push(scope_ref);
        self.current = scope_ref;
        scope_ref
    }

    pub fn push_function(&mut self) -> &'alloc Scope<'alloc> {
        let scope = Scope {
            parent: Some(self.current),
            parent_function: Some(self.current_function),
            is_function: true,
            has_captured_variable: Cell::new(false),
            children: RefCell::new(Vec::new_in(self.alloc)),
        };
        let scope_ref = self.alloc.alloc(scope) as &_;
        self.current.children.borrow_mut().push(scope_ref);
        self.current = scope_ref;
        self.current_function = scope_ref;
        scope_ref
    }

    pub fn pop(&mut self) {
        if self.current.is_function {
            self.current_function = self
                .current
                .parent_function
                .expect("tried to pop root scope")
        }
        self.current = self.current.parent.expect("tried to pop root scope");
    }
}
