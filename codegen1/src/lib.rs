use anyhow::{Result, anyhow};
use cfg::CFG;
use cfg::CFGBuilder;
use cfg::basic_block::BasicBlock;
use cfg::ir::ConstId;
use cfg::ir::HIR;
use cfg::ssa::SSABuilder;
use parser::FuncData;
use parser::FuncPool;
use parser::expr::{Binary, Unary, Call};
use parser::expr::{BinaryOp, Expr};
use parser::expr_parser::ExprId;
use parser::object::{FuncId, Function};
use parser::scanner::{Symbol, SymbolTable};
use parser::value::Literal;
use parser::value::Value;
use petgraph::graph::NodeIndex;
use petgraph::visit::Dfs;
use slotmap::{SecondaryMap, SlotMap};
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;

pub type RegIdx = u8;

pub type PIdx = i32;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub enum RegOrConst {
    Reg(RegIdx),
    Const(ConstId),
}

impl RegOrConst {
    pub fn get_reg(&self) -> RegIdx {
        if let RegOrConst::Reg(r) = self {
            return *r;
        }
        todo!()
    }
}

pub type Bytecode = Vec<OpCode>;
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpCode {
    // dst , src
    Move(RegIdx, RegOrConst),
    LoadConst(RegIdx, ConstId),
    LoadTrue(RegIdx),
    LoadFalse(RegIdx),
    LoadNil(RegIdx),
    Add(RegIdx, RegOrConst, RegOrConst),
    Sub(RegIdx, RegOrConst, RegOrConst),
    Mul(RegIdx, RegOrConst, RegOrConst),
    Div(RegIdx, RegOrConst, RegOrConst),
    Mod(RegIdx, RegOrConst, RegOrConst),
    And(RegOrConst, RegOrConst),
    Or(RegOrConst, RegOrConst),
    Eq(RegOrConst, RegOrConst),
    Ne(RegOrConst, RegOrConst),
    Lt(RegOrConst, RegOrConst),
    Le(RegOrConst, RegOrConst),
    Gt(RegOrConst, RegOrConst),
    Ge(RegOrConst, RegOrConst),
    BAnd(RegIdx, RegOrConst, RegOrConst),
    BOr(RegIdx, RegOrConst, RegOrConst),
    Not(RegOrConst),
    Neg(RegOrConst),
    Jmp(PIdx),
    Return0,
    Return1(RegOrConst),
    PrintExpr(ExprId),
    SetGlobal(RegOrConst, RegOrConst),
    GetGlobal(RegOrConst, RegOrConst),
    MakeUpvalue(RegIdx, RegOrConst),
    GetUpvalue(RegIdx, RegIdx),
    CloseUpvalues(RegIdx),
    CloseUpvalue(RegIdx),
    Call(RegIdx, RegIdx, u8),
    Copy(RegIdx, RegOrConst),
    //    Div(RegIdx, RegIdx, RegIdx),
    //    Mod(RegIdx, RegIdx, RegIdx),
    //    Pow(RegIdx, RegIdx, RegIdx),
    //    BNot(RegIdx, RegIdx,
}

impl From<u8> for RegOrConst {
    fn from(val: u8) -> Self {
        RegOrConst::Reg(val)
    }
}

impl From<ConstId> for RegOrConst {
    fn from(val: ConstId) -> Self {
        RegOrConst::Const(val)
    }
}

pub struct Compiler<'a> {
    pub symbol_table: &'a SymbolTable,
    cfg: &'a CFG,
    func_data: &'a FuncData,
    ssa: &'a SSABuilder,
    func_pool: &'a FuncPool,
    func_id: FuncId,
    pub func_protos: SecondaryMap<FuncId, FuncProto>,
}

impl FuncProto {}

impl<'a> Compiler<'a> {
    pub fn func_protos(self) -> SecondaryMap<FuncId, FuncProto> {
        self.func_protos
    }
    pub fn new(
        symbol_table: &'a SymbolTable,
        cfg: &'a CFG,
        func_data: &'a FuncData,
        ssa: &'a SSABuilder,
        func_pool: &'a FuncPool,
        func_id: FuncId,
    ) -> Self {
        Self {
            func_id,
            symbol_table,
            cfg,
            func_data,
            ssa,
            func_pool,
            func_protos: SecondaryMap::new(),
        }
    }

    fn lookup_binding(&mut self, name: Symbol) -> Result<Option<Binding>> {
        // The frame_offset is the number of parent nesting functions searched for a variable
        let mut frame_offset: u8 = 0;
        let mut locals = Some(&self.func_protos[self.func_id].variables);
        while let Some(mut l) = locals {
            for scope in l.scopes.iter().rev() {
                if let Some(var) = scope.lookup_binding(name) {
                    if frame_offset == 0 {
                        // At depth 0, this is a local binding
                        return Ok(Some(Binding::Local(var.register())));
                    }
                    // Otherwise it is a nonlocal and needs to be referenced as an upvalue.
                    // Create a new upvalue reference if one does not exist.

                    let mut nonlocals = l.nonlocals.borrow_mut();

                    if let None = nonlocals.get(&name) {
                        let upval_id = l.acquire_upvalue_id();
                        // Create a new non-local descriptor and add it
                        let nonlocal = Nonlocal::new(upval_id, frame_offset, var.register());
                        nonlocals.insert(name, nonlocal);

                        // Mark the variable as closed-over, as in, a closure will refer to it
                        // and it's upvalue must be closed at runtime
                        var.close_over();
                    }

                    if self.func_data.child_to_parent.get(self.func_id).is_some() {
                        if let Some(parent) = self.func_data.child_to_parent.get(self.func_id) {
                            locals = Some(&self.func_protos[*parent].variables);
                            frame_offset += 1;
                        }
                    }

                    // We've reached the end of the scopes at this point so we can check if we
                    // know about this binding as an upvalue and return it
                }
            }
        }

        let nonlocals = self.func_protos[self.func_id].variables.nonlocals.borrow();
        if let Some(nonlocal) = nonlocals.get(&name) {
            return Ok(Some(Binding::Upvalue(nonlocal.upvalue_id)));
        }

        Ok(None)
    }

    pub fn prep_for_next_func(&mut self, cfg_builder: &'a CFGBuilder, func_id: FuncId) {
        self.cfg = &cfg_builder.func_to_cfg[func_id];
        self.func_data = &cfg_builder.func_data;
        self.func_id = func_id;
        self.ssa = &cfg_builder.func_to_ssa[func_id];
        self.func_pool = &cfg_builder.func_pool;
    }

    pub fn new_from_id(cfg_builder: &'a CFGBuilder, func_id: FuncId) -> Self {
        Self {
            func_id,
            symbol_table: &cfg_builder.symbol_table,
            cfg: &cfg_builder.func_to_cfg[func_id],
            func_data: &cfg_builder.func_data,
            ssa: &cfg_builder.func_to_ssa[func_id],
            func_pool: &cfg_builder.func_pool,
            func_protos: SecondaryMap::new(),
        }
    }

    pub fn add_sentinal_jmp_at_idx(&mut self, idx: usize) -> usize {
        self.func_protos[self.func_id].insert_op_at(idx, OpCode::Jmp(-1))
    }

    pub fn add_sentinal_jmp(&mut self) -> usize {
        self.func_protos[self.func_id].insert_op(OpCode::Jmp(-1))
    }

    pub fn patch_sentinal_jmp(&mut self, jmp_idx: usize, new_val: i32) {
        if let Some(OpCode::Jmp(jmp_size)) =
            self.func_protos[self.func_id].bytecode.get_mut(jmp_idx)
        {
            return *jmp_size = new_val;
        }
        panic!("could not patch sentinal jmp");
    }
    pub fn compile_func(&mut self, func_id: FuncId) -> Result<()> {
        let func: &Function = &self.func_pool[self.func_id];

        self.func_protos.insert(func_id, func.into());
        //        self.func_protos[self.func_id].const_pool.insert(Literal::Proto(func_id));
        //        self.func_protos[self.func_id].insert_op(OpCode::LoadConst());
        let root = NodeIndex::new(0);
        let graph = self.cfg;


        let mut beginning_end: HashMap<NodeIndex, (usize, Option<usize>)> = HashMap::new();
        let mut jmps_to_patch: HashMap<usize, NodeIndex> = HashMap::new();

        let mut dfs = Dfs::new(graph, root);

        let all_vars: Vec<Symbol> = self.ssa.var_defs.0.iter().flat_map(|pair| pair.1.keys().copied()).collect();
        let mut func_scope = Scope::new();

        self.func_protos[self.func_id].free_reg = func_scope.push_bindings(&all_vars, self.func_protos[self.func_id].free_reg)?;
        self.func_protos[func_id].variables.scopes.push(func_scope);

        while let Some(node) = dfs.next(graph) {
            let beginning_of_node = self.func_protos[self.func_id].op_count;
            beginning_end.insert(node, (beginning_of_node, None));
            let node_weight = &self.cfg[node];

            self.gen_bytecode_for_node_and_edges(node, &self.cfg[node], &mut jmps_to_patch)?;

            beginning_end.get_mut(&node).unwrap().1 = Some(self.func_protos[self.func_id].op_count);
        }

        println!("bytes: {:?}", self.func_protos[self.func_id].bytecode);

        for (idx, node) in &jmps_to_patch {
            let node_end = beginning_end[node].1.unwrap();

            let jmp_to = node_end as i32 - *idx as i32;

            self.patch_sentinal_jmp(*idx, jmp_to);
        };
        Ok(())
    }


    pub fn compile_all_funcs(&mut self, cfg: &'a CFGBuilder) -> Result<()> {
        for key in self.func_pool.keys() {
            self.prep_for_next_func(cfg, key);
            self.compile_func(key)?;
        }
        Ok(())
    }

    pub fn emit_load_const(&mut self, value: &Value) -> Result<RegOrConst> {
        if let Value::Literal(literal) = value {
            let dst = self.acquire_reg();
            let const_id = self.func_protos[self.func_id].const_pool.insert(*literal);
            self.func_protos[self.func_id].insert_op(OpCode::LoadConst(dst, const_id));
            return Ok(RegOrConst::Reg(dst));
        }
        Err(anyhow!("could not emit load const for {value:?}"))
    }

    pub fn resolve_arg(&mut self, expr_id: ExprId) -> Result<RegOrConst> {
        //        let expr = &self.func_data.expr_pools[self.func_id][expr_id];
        self.compile_expr(expr_id)
        //        match expr {
        //
        //            Expr::Value(val) => Ok(RegOrConst::Const(self.resolve_value(val)?)),
        //            Expr::Variable(var) => {
        //                self.resolve_var(&var.0)
        //            }
        //
        //            _ => todo!("expr: {expr:?}"),
        //        }
    }

    pub fn make_op_args() {}

    pub fn compile_binary_cmp(&mut self, binary: &Binary) -> Result<RegOrConst> {
        let op = &binary.op;
        let dst = self.acquire_reg();
        let lhs_reg = self.resolve_arg(binary.lhs)?;
        let rhs_reg = self.resolve_arg(binary.rhs)?;
        let opcode = match op {
            BinaryOp::GreaterEqual => OpCode::Ge(lhs_reg, rhs_reg),
            BinaryOp::LessEqual => OpCode::Le(lhs_reg, rhs_reg),
            BinaryOp::Equal => OpCode::Eq(lhs_reg, rhs_reg),
            BinaryOp::NotEqual => OpCode::Ne(lhs_reg, rhs_reg),
            BinaryOp::Or => OpCode::Or(lhs_reg, rhs_reg),
            BinaryOp::GreaterThan => OpCode::Gt(lhs_reg, rhs_reg),
            BinaryOp::LessThan => OpCode::Lt(lhs_reg, rhs_reg),
            _ => todo!(),
        };
        Ok(RegOrConst::Reg(
            self.func_protos[self.func_id].insert_op(opcode) as u8,
        ))
    }

    pub fn compile_binary_not_cmp(&mut self, binary: &Binary) -> Result<RegOrConst> {
        let op = &binary.op;
        let dst = self.acquire_reg();
        let lhs_reg = self.resolve_arg(binary.lhs)?;
        let rhs_reg = self.resolve_arg(binary.rhs)?;
        let opcode = match op {
            BinaryOp::Plus => OpCode::Add(dst, lhs_reg, rhs_reg),
            BinaryOp::Star => OpCode::Mul(dst, lhs_reg, rhs_reg),
            BinaryOp::Minus => OpCode::Sub(dst, lhs_reg, rhs_reg),
            BinaryOp::Slash => OpCode::Div(dst, lhs_reg, rhs_reg),
            _ => unreachable!(),
        };
        Ok(RegOrConst::Reg(
            self.func_protos[self.func_id].insert_op(opcode) as u8,
        ))
    }

    pub fn emit_unary_bytecode(&mut self, unary: &Unary) -> Result<RegOrConst> {
        todo!()
        //        let op = &unary.op; let dst = self.gen_load(unary.opnd);
        //        let opcode = match op {
        //            UnaryOp::Bang => OpCode::Not(RegOrExpr::Reg(dst)),
        //            UnaryOp::Minus => OpCode::Neg(RegOrExpr::Reg(dst)),
        //        };
        //        self.func_proto.insert_op(opcode)
    }

    pub fn resolve_value(&mut self, value: &Value) -> Result<ConstId> {
        match value {
            Value::Literal(lit) => Ok(self.func_protos[self.func_id].const_pool.insert(*lit)),
            Value::Object(obj) => todo!(),
        }
    }

    pub fn resolve_var(&mut self, name: &Symbol) -> Result<RegIdx> {
        let reg: RegIdx = match self.lookup_binding(*name)? {
            Some(Binding::Local(register)) => register,

            Some(Binding::Upvalue(upvalue_id)) => {
                // Retrieve the value via Upvalue indirection
                let dest = self.acquire_reg();
                self.func_protos[self.func_id].insert_op(OpCode::GetUpvalue(dest, upvalue_id));
                dest
            }

            None => {
                // Otherwise do a late-binding global lookup
                let name = self.emit_load_const(&Value::Literal(Literal::String(*name)))?;
                let dest = name; // reuse the register
                self.func_protos[self.func_id].insert_op(OpCode::GetGlobal(dest, name));
                dest.get_reg()
            }
        };
        Ok(reg)

        //        let reg = self.acquire_reg();
        //        let var_reg = self.lookup_binding(name);
        //        let opcode = OpCode::Move(var_reg, val);
        //        self.func_protos[self.func_id].insert_op(opcode);
        //        Ok(var_reg.into())

        //        Ok(self.func_protos[self.func_id].var_to_reg[name])
    }

    pub fn compile_binary(&mut self, bin: &Binary) -> Result<RegOrConst> {
        if bin.is_cmp() {
            self.compile_binary_cmp(bin)?;
            return Ok(self.func_protos[self.func_id].free_reg.into());
        }

        self.compile_binary_not_cmp(bin)
    }

    pub fn compile_call(&mut self, call: &Call) -> Result<RegOrConst> {
        println!("COMPILING CALL: {call:?}");
        // allocate a register for the return value
        let dest = self.acquire_reg();
        //        // allocate a register for a closure environment pointer
        let _closure_env = self.acquire_reg();
        //
        //        // evaluate arguments first
        let arg_list = &call.args;
        let src_reg = self.func_protos[self.func_id].free_reg;
        //
        for arg in arg_list {
            let src = self.compile_expr(*arg)?;
            // if a local variable register was returned, we need to copy the register to the arg
            // list. Bound registers are necessarily lower indexes than where the function call is
            // situated because expression scope and register acquisition progresses the register
            // index in use.
            if src_reg <= dest {
                let dest = self.acquire_reg();
                self.func_protos[self.func_id].insert_op(OpCode::Copy(dest, src));
            }
        }

        let func_to_call = call.callee.unwrap();
        let src = self.resolve_var(&func_to_call)?;

        // put the function pointer in the last register of the call so it'll be discarded
        //                let function = self.compile_eval(function_expr)?;
        self.func_protos[self.func_id].insert_op(
            OpCode::Call(
                src,
                dest,
                call.args.len() as u8,
            ),
        );

        //         ignore use of any registers beyond the result once the call is complete
        self.func_protos[self.func_id].reset_reg(dest + 1);
        Ok(dest.into())
    }


    pub fn compile_expr(&mut self, expr: ExprId) -> Result<RegOrConst> {
        let to_match: &Expr = &self.func_data.expr_pools[self.func_id][expr];
        match to_match {
            Expr::Binary(bin) => { self.compile_binary(bin) }
            Expr::Unary(un) => self.emit_unary_bytecode(un),
            Expr::Assign(assign) => {
                self.emit_assign_bytecode(assign.name, assign.val)
            }
            Expr::Call(call) => {
                self.compile_call(call)
            }
            Expr::Stmt(stmt) => {
                todo!()
            }
            Expr::Variable(var) => {
                //                let idx = self.resolve_var(&var.0);
                Ok(RegOrConst::Reg(self.resolve_var(&var.0)?))
            }
            Expr::Value(val) => {
                self.emit_load_const(val)
                //                self.func_proto.op_count
            }
            _ => {
                panic!("todo: {to_match:?}")
            }
        }
    }

    pub fn acquire_reg(&mut self) -> u8 {
        self.func_protos[self.func_id].acquire_reg()
    }


    //    pub fn emit_var_bytecode(&mut self, name: Symbol, val: ExprId) -> Result<RegOrConst> {
    //        let dst = self.lookup_binding(name)?;
    //        if let Some(dst) = dst {
    //            let dst = self.resolve_var(&name)?;
    //            let src = self.compile_expr(val)?;
    //
    //            let opcode = OpCode::Move(dst, src);
    //            self.func_protos[self.func_id].insert_op(opcode);
    //            return Ok(RegOrConst::Reg(dst));
    //        }
    //        Err(anyhow!("todo"))
    //    }

    pub fn emit_assign_bytecode(&mut self, name: Symbol, val: ExprId) -> Result<RegOrConst> {
        todo!()
        //        self.emit_var_bytecode(name, val)
    }

    pub fn gen_func_decl_bytecode(&self, cfg: &CFG, bytecode: &mut Bytecode) -> usize {
        todo!()
    }

    pub fn emit_bytecode(
        &mut self,
        node: NodeIndex,
        hir: &HIR,
        jmps: &mut HashMap<usize, NodeIndex>,
    ) -> Result<()> {
        match hir {
            HIR::Expr(expr_id) => {
                self.compile_expr(*expr_id)?;
            }

            //            HIR::Var(assign) => {
            //
            //                self.emit_var_bytecode(assign.name, assign.val)?;
            //            }
            HIR::Assign(assign) => {
                todo!()
                //                return self.emit_assign_bytecode(assign.name, RegOrConst::Const(assign.val));
            }
            HIR::Return0 => {
                self.func_protos[self.func_id].insert_op(OpCode::Return0);
            }
            HIR::Return1(val) => {
                let arg = self.resolve_arg(*val)?;
                self.func_protos[self.func_id].insert_op(OpCode::Return1(arg));
            }
            HIR::Print(expr) => {
                OpCode::PrintExpr(*expr);
            }
            HIR::Jmp(jmp_node) => {
                jmps.insert(self.func_protos[self.func_id].op_count, *jmp_node);
                self.func_protos[self.func_id].insert_op(OpCode::Jmp(-1));
            }
        };

        //        self.func_proto.insert_op(opcode);
        Ok(())
    }

    pub fn gen_bytecode_for_node_and_edges(
        &mut self,
        node: NodeIndex,
        block: &BasicBlock,
        jmps: &mut HashMap<usize, NodeIndex>,
    ) -> Result<usize> {
        for hir in block.borrow_stmts() {
            self.emit_bytecode(node, hir, jmps)?;
        }
        Ok(self.func_protos[self.func_id].op_count)
    }
}
#[derive(Debug)]
pub struct FuncProto {
    pub bytecode: Bytecode,
    //    pub registers: [Value; 255],
    pub free_reg: u8,
    pub name: Option<Symbol>,
    pub arity: u8,
    pub op_count: usize,
    pub variables: Variables,
    pub const_pool: SlotMap<ConstId, Literal>,
}

impl FuncProto {
    pub fn new(function: &Function, bytecode: Vec<OpCode>) -> Self {
        function.into()
    }

    pub fn acquire_reg(&mut self) -> u8 {
        let reg = self.free_reg;
        self.free_reg += 1;
        reg
    }
    fn reset_reg(&mut self, reg: RegIdx) {
        self.free_reg = reg
    }


    pub fn insert_op_at(&mut self, idx: usize, op: OpCode) -> usize {
        let count = self.op_count;
        self.op_count += 1;
        self.bytecode.insert(idx, op);
        count
    }

    pub fn insert_op(&mut self, op: OpCode) -> usize {
        let count = self.op_count;
        self.op_count += 1;
        self.bytecode.push(op);
        count
    }
}

pub type UpvalueId = u8;

#[derive(Debug, Clone, Copy)]
enum Binding {
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
    bindings: HashMap<Symbol, Variable>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            bindings: HashMap::new(),
        }
    }

    /// Add a Symbol->Register binding to this scope
    pub fn push_binding(&mut self, name: Symbol, reg: RegIdx) -> Result<()> {
        self.bindings.insert(name, Variable::new(reg));

        Ok(())
    }

    fn push_bindings(&mut self, names: &[Symbol], start_reg: RegIdx) -> Result<RegIdx> {
        let mut reg = start_reg;
        for name in names {
            println!("NAME: {:?}", name);
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
struct Nonlocal {
    upvalue_id: u8,
    frame_offset: u8,
    frame_register: u8,
}

impl Nonlocal {
    fn new(upvalue_id: UpvalueId, frame_offset: u8, frame_register: RegIdx) -> Nonlocal {
        Nonlocal {
            upvalue_id,
            frame_offset,
            frame_register,
        }
    }
}

#[derive(Debug)]
struct Variables {
    scopes: Vec<Scope>,
    nonlocals: RefCell<HashMap<Symbol, Nonlocal>>,
    next_upvalue: Cell<u8>,
}
// ANCHOR_END: DefVariables

impl Variables {
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
            nonlocals: RefCell::new(HashMap::new()),
            next_upvalue: Cell::new(0),
        }
    }

    /// Search for a binding, following parent scopes.

    /// Return the next upvalue id and increment the counter
    fn acquire_upvalue_id(&self) -> UpvalueId {
        let id = self.next_upvalue.get();
        self.next_upvalue.set(id + 1);
        id
    }

    /// Return an ArrayU16 of nonlocal references if there are any for the function
    fn get_nonlocals(&self) -> Result<Option<Vec<Nonlocal>>> {
        let count = self.next_upvalue.get();
        if count == 0 {
            return Ok(None);
        }
        let nonlocals = self.nonlocals.borrow();
        let mut nonlocal_vals: Vec<Nonlocal> = nonlocals.values().copied().collect();
        nonlocal_vals.sort_by(|x, y| x.upvalue_id.cmp(&y.upvalue_id));

        Ok(Some(nonlocal_vals))
    }

    /// Pop the last scoped variables and create close-upvalue instructions for any closed over
    fn pop_scope(&mut self) -> Vec<OpCode> {
        let mut closings = Vec::new();

        if let Some(scope) = self.scopes.pop() {
            for var in scope.bindings.values() {
                if var.is_closed_over() {
                    closings.push(OpCode::CloseUpvalues(var.register()))
                };
            }
        };
        closings
    }
}

impl From<&Function> for FuncProto {
    fn from(val: &Function) -> Self {
        const NIL: Value = Value::Literal(Literal::Nil);
        let mut count = 2;
        let mut scopes = Scope::new();
        let mut variables = Variables::new();
        count += scopes.push_bindings(&val.params, count).unwrap();
        if let Some(name) = val.name {
            scopes.push_binding(name, count).unwrap();
            count += 1;
        }
        variables.scopes.push(scopes);
        FuncProto {
            variables,
            bytecode: Vec::new(),
            //            registers: [NIL; 255],
            free_reg: count,
            name: val.name,
            arity: val.arity,
            op_count: 0,
            const_pool: SlotMap::with_key(),
        }
    }
}

#[cfg(test)]
mod codegen_tests {

    pub fn binary() {
        let SOURCE: &'static str = "func test_func() {
            var test_var = 0;
            while (true && true) {
                if (false) {
                    test_var = 11223 * 99;
                } else {
                    test_var = 100 - 22;
                }
            }
            test_var + 3000;
            var new_var = test_var + 1;
            }
            test_func()";
    }
}
