use crate::scanner::Symbol;
use crate::stmt::Block;
use slotmap::new_key_type;
new_key_type! {pub struct FuncId; pub struct ObjId;}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct Function {
    pub name: Symbol,
    pub body: Block,
    pub arity: u8,
    pub params: Option<Vec<Symbol>>,
}

impl Function {
    pub fn new(name: Symbol, body: Block, arity: u8, params: Option<Vec<Symbol>>) -> Self {
        Self {
            name,
            body,
            arity,
            params,
        }
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Hash, Eq)]
pub enum Object {
    Function(FuncId),
}
