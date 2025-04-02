use crate::scanner::Symbol;
use crate::stmt::Block;
use slotmap::{SecondaryMap, new_key_type};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
new_key_type! {pub struct FuncId; pub struct ObjId;}

pub type UpvalueId = u8;
pub type RegIdx = u8;

#[derive(Debug, Clone, Copy)]
pub enum Binding {
    Local(RegIdx),
    Upvalue(UpvalueId),
}

#[derive(Debug)]
pub struct Variable {
    register: RegIdx,
    closed_over: Cell<bool>,
}

impl Variable {
    pub fn new(register: RegIdx) -> Self {
        Self {
            register,
            closed_over: Cell::new(false),
        }
    }
    pub fn register(&self) -> RegIdx {
        self.register
    }

    pub fn close_over(&self) {
        self.closed_over.set(true);
    }
    pub fn is_closed_over(&self) -> bool {
        self.closed_over.get()
    }
}

#[derive(Debug)]
pub struct Scope {
    pub bindings: HashMap<Symbol, Variable>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            bindings: HashMap::new(),
        }
    }

    /// Add a Symbol->Register binding to this scope
    pub fn push_binding(&mut self, name: Symbol, reg: RegIdx) -> anyhow::Result<()> {
        self.bindings.insert(name, Variable::new(reg));

        Ok(())
    }

    pub fn push_bindings(&mut self, names: &[Symbol], start_reg: RegIdx) -> anyhow::Result<RegIdx> {
        let mut reg = start_reg;
        for name in names {
            self.push_binding(*name, reg)?;
            reg += 1;
        }
        Ok(reg)
    }

    pub fn lookup_binding(&self, name: Symbol) -> Option<&Variable> {
        self.bindings.get(&name)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Nonlocal {
    pub upvalue_id: u8,
    pub frame_offset: u8,
    pub frame_register: u8,
}

impl Nonlocal {
    pub fn new(upvalue_id: UpvalueId, frame_offset: u8, frame_register: RegIdx) -> Nonlocal {
        Nonlocal {
            upvalue_id,
            frame_offset,
            frame_register,
        }
    }
}

#[derive(Debug)]
pub struct Variables {
    pub scopes: Vec<Scope>,
    pub nonlocals: RefCell<HashMap<Symbol, Nonlocal>>,
    pub next_upvalue: Cell<u8>,
}

impl Variables {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            nonlocals: RefCell::new(HashMap::new()),
            next_upvalue: Cell::new(0),
        }
    }

    pub fn acquire_upvalue_id(&self) -> UpvalueId {
        let id = self.next_upvalue.get();
        self.next_upvalue.set(id + 1);
        id
    }

    pub fn get_nonlocals(&self) -> anyhow::Result<Option<Vec<Nonlocal>>> {
        let count = self.next_upvalue.get();
        if count == 0 {
            return Ok(None);
        }
        let nonlocals = self.nonlocals.borrow();
        let mut nonlocal_vals: Vec<Nonlocal> = nonlocals.values().copied().collect();
        nonlocal_vals.sort_by(|x, y| x.upvalue_id.cmp(&y.upvalue_id));

        Ok(Some(nonlocal_vals))
    }
}


#[derive(Debug, Default, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct Function {
    pub name: Option<Symbol>,
    pub body: Block,
    pub arity: u8,
    pub params: Vec<Symbol>,
    pub func_id: FuncId,
}

impl Function {
    pub fn new(
        name: Option<Symbol>,
        body: Block,
        arity: u8,
        params: Vec<Symbol>,
        func_id: FuncId,
    ) -> Self {
        Self {
            name,
            body,
            arity,
            params,
            func_id,
        }
    }
}


#[derive(Debug, PartialEq, Clone, PartialOrd, Hash, Eq, Copy)]
pub enum Object {
    Closure,
}
