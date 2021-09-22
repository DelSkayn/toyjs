use bumpalo::{collections::Vec, Bump};
use common::{
    bump_list::List, collections::HashMap, interner::StringId, newtype_index, newtype_slice,
    newtype_vec,
};

use std::{
    cell::{Cell, RefCell},
    cmp::Eq,
    fmt,
    ops::Deref,
    ptr,
};

#[derive(Clone, Debug, Copy)]
pub enum VariableKind {
    Global,
    Implicit,
    Local(u32),
    LocalConstant(u32),
    Captured(u32),
    CapturedConstant(u32),
    Argument(u32),
    CapturedArgument(u32),
}

impl VariableKind {
    fn capture(self) -> VariableKind {
        match self {
            VariableKind::Local(x) => VariableKind::Captured(x),
            VariableKind::LocalConstant(x) => VariableKind::CapturedConstant(x),
            VariableKind::Argument(x) => VariableKind::CapturedArgument(x),
            x => x,
        }
    }

    pub fn is_local(&self) -> bool {
        match self {
            VariableKind::Global
            | VariableKind::Implicit
            | VariableKind::Argument(_)
            | VariableKind::CapturedArgument(_) => false,
            VariableKind::Local(_)
            | VariableKind::LocalConstant(_)
            | VariableKind::Captured(_)
            | VariableKind::CapturedConstant(_) => true,
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
    pub scope: ScopeRef<'alloc>,
}

#[derive(Debug)]
pub struct FunctionScope<'alloc> {
    pub num_arguments: Cell<u32>,
    pub captures: RefCell<List<'alloc, VariableId>>,
    pub has_captured_variable: Cell<bool>,
    pub stack_depth: u32,
    pub next_slot: Cell<u32>,
}

#[derive(Debug)]
pub enum ScopeType<'alloc> {
    Lexical,
    Function(FunctionScope<'alloc>),
}

impl<'alloc> ScopeType<'alloc> {
    pub fn as_function(&self) -> &FunctionScope<'alloc> {
        match *self {
            ScopeType::Lexical => panic!("called as function on non-function scope"),
            ScopeType::Function(ref x) => x,
        }
    }
}

pub struct Scope<'alloc> {
    pub parent: Option<ScopeRef<'alloc>>,
    pub parent_function: Option<ScopeRef<'alloc>>,
    pub children: RefCell<Vec<'alloc, ScopeRef<'alloc>>>,
    pub variables: RefCell<Vec<'alloc, VariableId>>,
    pub ty: ScopeType<'alloc>,
}

impl<'alloc> fmt::Debug for Scope<'alloc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope")
            .field("parent", &self.parent.map(|_| "cyclic"))
            .field("parent_function", &self.parent_function.map(|_| "cyclic"))
            .field("children", &self.children)
            .field("variables", &self.variables)
            .field("ty", &self.ty)
            .finish()
    }
}

impl<'alloc> ScopeRef<'alloc> {
    pub fn traverse_childeren<F: FnMut(Self) -> bool>(&self, f: &mut F) {
        self.children.borrow().iter().copied().for_each(|c| {
            if f(c) {
                c.traverse_childeren(f)
            }
        });
    }

    pub fn is_function(self) -> bool {
        match self.ty {
            ScopeType::Lexical => false,
            ScopeType::Function(_) => true,
        }
    }

    pub fn current_function(self) -> ScopeRef<'alloc> {
        if self.is_function() {
            self
        } else {
            self.parent_function.unwrap()
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ScopeRef<'alloc>(&'alloc Scope<'alloc>);

impl<'alloc> PartialEq for ScopeRef<'alloc> {
    fn eq(&self, other: &ScopeRef<'alloc>) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'alloc> Deref for ScopeRef<'alloc> {
    type Target = Scope<'alloc>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[derive(Debug)]
pub struct Variables<'alloc> {
    root: ScopeRef<'alloc>,
    implicits: HashMap<StringId, VariableId>,
    current: ScopeRef<'alloc>,
    current_function: ScopeRef<'alloc>,
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
        let root = ScopeRef(alloc.alloc(Scope {
            parent: None,
            parent_function: None,
            children: RefCell::new(Vec::new_in(alloc)),
            variables: RefCell::new(Vec::new_in(alloc)),
            ty: ScopeType::Function(FunctionScope {
                num_arguments: Cell::new(0),
                captures: RefCell::new(List::new_in(alloc)),
                stack_depth: 0,
                has_captured_variable: Cell::new(false),
                next_slot: Cell::new(0),
            }),
        }));
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

    pub fn root(&self) -> ScopeRef<'alloc> {
        self.root
    }

    pub fn use_variable(&mut self, name: StringId) -> VariableId {
        let mut current = self.current;
        let mut crossed_function = false;
        loop {
            let key = (current.0 as *const _, name);
            if let Some(x) = self.variable_ids.get(&key).copied() {
                if crossed_function {
                    // Variable crossed function boundery
                    // so some variable kinds should be changed to reflect that.
                    if self.variables[x].kind.is_local() {
                        self.variables[x]
                            .scope
                            .current_function()
                            .ty
                            .as_function()
                            .has_captured_variable
                            .set(true);
                        self.variables[x].kind = self.variables[x].kind.capture();
                    }
                }
                if crossed_function {
                    self.current
                        .current_function()
                        .ty
                        .as_function()
                        .captures
                        .borrow_mut()
                        .push(x);
                }
                return x;
            }
            crossed_function |= current.is_function();
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
            scope: self.current_function,
        });
        if self.current != self.root {
            self.current
                .current_function()
                .ty
                .as_function()
                .captures
                .borrow_mut()
                .push(variable);
        }
        self.implicits.insert(name, variable);
        variable
    }

    pub fn define_argument(&mut self, name: StringId) -> VariableId {
        let key = (self.current_function.0 as *const _, name);
        let num_args = self.current_function.ty.as_function().num_arguments.get();
        self.current_function
            .ty
            .as_function()
            .num_arguments
            .set(num_args + 1);
        if let Some(id) = self.implicits.remove(&name) {
            self.variables[id].kind = VariableKind::Argument(num_args);
            self.variables[id].define_depth = self.cur_depth;
            self.variable_ids.insert(key, id);
            return id;
        }
        let id = self.variables.push(Variable {
            kind: VariableKind::Argument(num_args),
            name,
            define_depth: self.cur_depth,
            scope: self.current_function,
        });
        self.variable_ids.insert(key, id);
        self.current_function.variables.borrow_mut().push(id);
        self.variable_ids
            .insert((self.current_function.0, name), id);
        id
    }

    pub fn define_global(&mut self, name: StringId) -> VariableId {
        // Connect uses of variables to later definitions
        let key = (self.current_function.0 as *const _, name);
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
            scope: self.current_function,
        });
        self.variable_ids.insert(key, id);
        self.current_function.variables.borrow_mut().push(id);
        self.variable_ids
            .insert((self.current_function.0, name), id);
        id
    }

    pub fn define_local(&mut self, name: StringId, constant: bool) -> VariableId {
        let slot = self.current_function.ty.as_function().next_slot.get();
        self.current_function
            .ty
            .as_function()
            .next_slot
            .set(slot + 1);
        let ty = if constant {
            VariableKind::LocalConstant(slot)
        } else {
            VariableKind::Local(slot)
        };
        let id = self.variables.push(Variable {
            kind: ty,
            name,
            define_depth: self.cur_depth,
            scope: self.current,
        });
        self.current.variables.borrow_mut().push(id);
        self.variable_ids.insert((self.current.0, name), id);
        id
    }

    pub fn push_scope(&mut self) -> ScopeRef<'alloc> {
        let scope = Scope {
            parent: Some(self.current),
            parent_function: Some(self.current_function),
            children: RefCell::new(Vec::new_in(self.alloc)),
            variables: RefCell::new(Vec::new_in(self.alloc)),
            ty: ScopeType::Lexical,
        };
        let scope_ref = ScopeRef(self.alloc.alloc(scope));
        self.current.children.borrow_mut().push(scope_ref);
        self.current = scope_ref;
        scope_ref
    }

    pub fn push_function(&mut self) -> ScopeRef<'alloc> {
        self.cur_depth.checked_add(1).expect("scope depth to great");
        let scope = Scope {
            parent: Some(self.current),
            parent_function: Some(self.current_function),
            children: RefCell::new(Vec::new_in(self.alloc)),
            variables: RefCell::new(Vec::new_in(self.alloc)),
            ty: ScopeType::Function(FunctionScope {
                next_slot: Cell::new(0),
                stack_depth: self.cur_depth,
                num_arguments: Cell::new(0),
                captures: RefCell::new(List::new_in(self.alloc)),
                has_captured_variable: Cell::new(false),
            }),
        };
        let scope_ref = ScopeRef(self.alloc.alloc(scope));
        self.current.children.borrow_mut().push(scope_ref);
        self.current = scope_ref;
        self.current_function = scope_ref;
        scope_ref
    }

    pub fn pop(&mut self) {
        if self.current.is_function() {
            self.cur_depth -= 1;
            self.current_function = self
                .current
                .parent_function
                .expect("tried to pop root scope")
        }
        self.current = self.current.parent.expect("tried to pop root scope");
    }
}
