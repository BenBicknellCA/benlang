use crate::scanner::Symbol;
use crate::stmt::Block;
use slotmap::new_key_type;
new_key_type! {pub struct FuncId; pub struct ObjId;}

#[derive(Debug, Default, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct Function {
    pub name: Option<Symbol>,
    pub body: Block,
    pub arity: u8,
    pub params: Option<Vec<Symbol>>,
    pub func_id: FuncId,
}

impl Function {
    pub fn new(
        name: Option<Symbol>,
        body: Block,
        arity: u8,
        params: Option<Vec<Symbol>>,
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

#[derive(Debug, PartialEq, Clone, PartialOrd, Hash, Eq)]
pub enum Object {
    Function(Function),
}
