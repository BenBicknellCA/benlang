use slotmap::SecondaryMap;
use parser::object::FuncId;
use codegen::FuncProto;
use parser::scanner::{Symbol, SymbolTable};

pub struct VM {
    func_protos: SecondaryMap<FuncId, FuncProto>,
    string_interner: SymbolTable,
    main: FuncId,
}

pub struct CallFrame {}
impl VM {
    pub fn eval(&mut self) {}
}