use bumpalo::{collections::Vec, Bump};
use common::{collections::HashMap, interner::StringId, newtype_index, newtype_slice, newtype_vec};
use std::{
    cell::{Cell, RefCell},
    fmt, ptr,
};

#[derive(Clone, Debug, Copy)]
pub enum VariableKind {
    Global,
    Implicit,
    Local,
    LocalConstant,
    Captured,
    CapturedConstant,
}

impl VariableKind {
    pub fn is_local(&self) -> bool {
        match self {
            VariableKind::Global | VariableKind::Implicit => false,
            VariableKind::Local
            | VariableKind::LocalConstant
            | VariableKind::Captured
            | VariableKind::CapturedConstant => true,
        }
    }
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
    /// The stack depth the variable was defined at.
    /// So for example variable foo in the following code is defined in stack depth 2:
    /// ```javascript
    /// function(){
    ///     if (bar){
    ///         function(){
    ///             let foo = 3;
    ///         }
    ///     }
    /// }
    /// ```
    pub define_depth: u32,
    pub slot: Option<u32>,
    pub scope: &'alloc Scope<'alloc>,
}

pub struct Scope<'alloc> {
    pub parent: Option<&'alloc Scope<'alloc>>,
    pub parent_function: Option<&'alloc Scope<'alloc>>,
    pub is_function: bool,
    pub has_captured_variable: Cell<bool>,
    pub children: RefCell<Vec<'alloc, &'alloc Scope<'alloc>>>,
    pub variables: RefCell<Vec<'alloc, VariableId>>,
    pub captures: RefCell<Vec<'alloc, VariableId>>,
    pub stack_depth: u32,
    pub next_slot: Cell<u32>,
}

impl<'alloc> fmt::Debug for Scope<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope")
            .field("parent", &self.parent.map(|_| "cyclic"))
            .field("parent_function", &self.parent_function.map(|_| "cyclic"))
            .field("is_function", &self.is_function)
            .field("has_captured_variable", &self.has_captured_variable)
            .field("children", &self.children)
            .field("variables", &self.variables)
            .field("captures", &self.captures)
            .field("stack_depth", &self.stack_depth)
            .field("next_slot", &self.next_slot)
            .finish()
    }
}

impl<'alloc> Scope<'alloc> {
    pub fn traverse_childeren<F: FnMut(&Self) -> bool>(&self, f: &mut F) {
        self.children.borrow().iter().copied().for_each(|c| {
            if f(c) {
                c.traverse_childeren(f)
            }
        });
    }
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
    cur_depth: u32,
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
            variables: RefCell::new(Vec::new_in(alloc)),
            captures: RefCell::new(Vec::new_in(alloc)),
            stack_depth: 0,
            next_slot: Cell::new(0),
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
            cur_depth: 0,
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
                if crossed_function {
                    self.current.captures.borrow_mut().push(x);
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
            define_depth: 0,
            slot: None,
            scope: self.current_function,
        });
        if !ptr::eq(self.current, self.root) {
            self.current.captures.borrow_mut().push(variable);
        }
        self.implicits.insert(name, variable);
        variable
    }

    pub fn define_global(&mut self, name: StringId) -> VariableId {
        // Connect uses of variables to later definitions
        let key = (self.current_function as *const _, name);
        if let Some(id) = self.implicits.remove(&name) {
            self.variables[id].kind = VariableKind::Global;
            self.variables[id].define_depth = self.cur_depth;
            self.variable_ids.insert(key, id);
            return id;
        }
        let id = self.variables.push(Variable {
            kind: VariableKind::Global,
            name,
            define_depth: self.cur_depth,
            slot: None,
            scope: self.current_function,
        });
        self.variable_ids.insert(key, id);
        self.current_function.variables.borrow_mut().push(id);
        self.variable_ids.insert((self.current_function, name), id);
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

        let slot = self.current_function.next_slot.get();
        self.current_function.next_slot.set(slot + 1);
        let id = self.variables.push(Variable {
            kind: ty,
            name,
            define_depth: self.cur_depth,
            slot: Some(slot),
            scope: self.current,
        });
        self.current.variables.borrow_mut().push(id);
        self.variable_ids.insert((self.current, name), id);
        id
    }

    pub fn push_scope(&mut self) -> &'alloc Scope<'alloc> {
        let scope = Scope {
            parent: Some(self.current),
            parent_function: Some(self.current_function),
            is_function: false,
            has_captured_variable: Cell::new(false),
            children: RefCell::new(Vec::new_in(self.alloc)),
            variables: RefCell::new(Vec::new_in(self.alloc)),
            captures: RefCell::new(Vec::new_in(self.alloc)),
            stack_depth: self.cur_depth,
            next_slot: Cell::new(0),
        };
        let scope_ref = self.alloc.alloc(scope);
        self.current.children.borrow_mut().push(scope_ref);
        self.current = scope_ref;
        scope_ref
    }

    pub fn push_function(&mut self) -> &'alloc Scope<'alloc> {
        self.cur_depth.checked_add(1).expect("scope depth to great");
        let scope = Scope {
            parent: Some(self.current),
            parent_function: Some(self.current_function),
            is_function: true,
            has_captured_variable: Cell::new(false),
            children: RefCell::new(Vec::new_in(self.alloc)),
            variables: RefCell::new(Vec::new_in(self.alloc)),
            captures: RefCell::new(Vec::new_in(self.alloc)),
            stack_depth: self.cur_depth,
            next_slot: Cell::new(0),
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
        if self.current.is_function {
            self.cur_depth -= 1;
        }
        self.current = self.current.parent.expect("tried to pop root scope");
    }
}
