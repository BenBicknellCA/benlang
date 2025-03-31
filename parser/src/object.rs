use crate::scanner::Symbol;
use crate::stmt::Block;
use slotmap::{SecondaryMap, new_key_type};
use std::cell::Cell;
use std::collections::HashMap;
new_key_type! {pub struct FuncId; pub struct ObjId;}

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

pub struct Closure {
    func_id: FuncId,
    upvalues: Vec<UpvalueId>,
}

#[derive(Copy, Clone, PartialEq)]
pub enum Binding {
    Local(u8),
    Upvalue(u8),
}

impl Var {
    fn new(register: u8) -> Self {
        Self {
            register,
            closed_over: Cell::new(false),
        }
    }

    fn register(&self) -> u8 {
        self.register
    }

    fn close_over(&self) {
        self.closed_over.set(true);
    }

    pub fn is_closed_over(&self) -> bool {
        self.closed_over.get()
    }
}


#[derive(Debug, PartialEq, Clone, PartialOrd, Hash, Eq, Copy)]
pub enum Object {
    Closure,
}

#[derive(Debug)]
pub struct Nonlocal {
    upvalue_id: u8,
    frame_offset: u8,
    frame_register: u8,
}

impl Nonlocal {
    pub fn new(upvalue_id: u8, frame_offset: u8, frame_register: u8) -> Self {
        Self {
            upvalue_id,
            frame_offset,
            frame_register,
        }
    }
}

#[derive(Debug, Default)]
pub struct Variables {
    pub parent: Option<FuncId>,
    pub scopes: Vec<Scope>,
    pub nonlocals: HashMap<Symbol, Nonlocal>,
    pub next_upvalue: Cell<u8>,
}

pub type UpvalueId = u8;

impl Variables {
    pub fn new(parent: Option<FuncId>) -> Self {
        Self {
            parent,
            scopes: Vec::new(),
            nonlocals: HashMap::new(),
            next_upvalue: Cell::new(0),
        }
    }

    fn acquire_upvalue_id(&self) -> UpvalueId {
        let id = self.next_upvalue.get();
        self.next_upvalue.set(id + 1);
        id
    }

    pub fn lookup_binding(
        name: Symbol,
        func_id: FuncId,
        vars: &mut SecondaryMap<FuncId, Variables>,
    ) -> anyhow::Result<Option<Binding>> {
        let mut frame_offset: u8 = 0;

        let mut func_vars = func_id;

        let mut locals = Some(func_vars);
        while let Some(ref mut l) = locals {
            let mut function = &mut vars[*l];
            for scope in function.scopes.iter().rev() {
                if let Some(mut var) = scope.lookup_binding(name) {
                    if frame_offset == 0 {
                        return Ok(Some(Binding::Local(var.register)));
                    }

                    if let None = function.nonlocals.get(&name) {
                        let id = function.acquire_upvalue_id();
                        let nonlocal = Nonlocal::new(id, frame_offset, var.register());

                        function.nonlocals.insert(name, nonlocal);
                        var.close_over();

                        //                    if nonlocals.get(&name).is_none() {
                        //                    }
                    }
                }
            }

            if let Some(is_local) = &locals {
                if let Some(parent) = vars[*is_local].parent {
                    locals = Some(parent);
                    frame_offset += 1;
                }
            }
        }
        let nonlocals = &vars[func_id].nonlocals;
        if let Some(nonlocal) = nonlocals.get(&name) {
            return Ok(Some(Binding::Upvalue(nonlocal.upvalue_id)));
        }
        Ok(None)
    }
}

pub type RegIdx = u8;

#[derive(Debug)]
pub struct Var {
    register: RegIdx,
    pub closed_over: std::cell::Cell<bool>,
}

#[derive(Debug)]
pub struct Scope {
    pub bindings: HashMap<Symbol, Var>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }
    pub fn push_binding(&mut self, name: Symbol, reg: RegIdx) -> anyhow::Result<()> {
        self.bindings.insert(name, Var::new(reg));
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
    pub fn lookup_binding_mut(&mut self, name: Symbol) -> Option<&mut Var> {
        self.bindings.get_mut(&name)
    }

    pub fn lookup_binding(&self, name: Symbol) -> Option<&Var> {
        self.bindings.get(&name)
    }
}
