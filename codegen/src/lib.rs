use anyhow::{Result, anyhow};
use cfg::CFG;
use cfg::CFGBuilder;
use cfg::basic_block::BasicBlock;
use cfg::ir::ConstId;
use cfg::ir::HIR;
use cfg::ssa::SSABuilder;
use parser::FuncData;
use parser::FuncPool;
use parser::expr::{Binary, Call, Unary};
use parser::expr::{BinaryOp, Expr};
use parser::expr_parser::ExprId;
use parser::object::{Binding, Nonlocal, Scope, Var, Variables};
use parser::object::{FuncId, Function, Object};
use parser::scanner::{Symbol, SymbolTable};
use parser::value::Literal;
use parser::value::Value;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::matrix_graph::Zero;
use petgraph::visit::Dfs;
use petgraph::visit::EdgeRef;
use slotmap::{SecondaryMap, SlotMap};
use std::cell::Cell;
use std::collections::HashMap;

pub type RegIdx = u8;

pub type PIdx = i32;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum RegOrConst {
    Reg(RegIdx),
    Const(ConstId),
}

impl RegOrConst {
    pub fn get_reg(&self) -> Option<RegIdx> {
        if let RegOrConst::Reg(idx) = self {
            return Some(*idx);
        };
        None
    }
    pub fn get_const_id(&self) -> Option<ConstId> {
        if let RegOrConst::Const(id) = self {
            return Some(*id);
        };
        None
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
    Eq(RegOrConst, RegOrConst),
    Ne(RegOrConst, RegOrConst),
    Or(RegOrConst, RegOrConst),
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
    Call(RegIdx, u8),
    Copy(RegIdx, RegOrConst),
    SetGlobal(RegOrConst, RegIdx),
    GetGlobal(RegIdx, RegOrConst),
    Closure(FuncId),
    Close(u8),
    GetUpvalue(RegIdx, u8),

    NoOp,
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
    symbol_table: &'a SymbolTable,
    cfg: &'a CFG,
    pub func_data: FuncData,
    ssa: &'a SSABuilder,
    func_pool: &'a FuncPool,
    func_id: FuncId,
    pub func_protos: SecondaryMap<FuncId, FuncProto>,
}

impl FuncProto {
    pub fn acquire_reg(&mut self) -> u8 {
        let reg = self.next_reg;
        self.next_reg += 1;
        reg
    }
}

impl<'a> Compiler<'a> {
    pub fn new(
        symbol_table: &'a SymbolTable,
        cfg: &'a CFG,
        func_data: FuncData,
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

    fn pop_scope(vars: &mut Variables) -> Vec<OpCode> {
        let mut closings = Vec::new();

        if let Some(scope) = vars.scopes.pop() {
            for var in scope.bindings.values() {
                if var.is_closed_over() {
                    // todo!
                };
            }
        }
        closings
    }

    pub fn prep_for_next_func(&mut self, cfg_builder: &'a CFGBuilder, func_id: FuncId) {
        self.cfg = &cfg_builder.func_to_cfg[func_id];
        self.func_id = func_id;
        self.ssa = &cfg_builder.func_to_ssa[func_id];
    }

    pub fn new_from_id(cfg_builder: &'a mut CFGBuilder, func_id: FuncId) -> Self {
        let func_data = std::mem::take(&mut cfg_builder.func_data);
        Self {
            func_id,
            symbol_table: &cfg_builder.symbol_table,
            cfg: &cfg_builder.func_to_cfg[func_id],
            func_data,
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

    pub fn compile_func(&mut self, func_id: FuncId, parent: Option<FuncId>) -> Result<()> {
        let func: &Function = &self.func_pool[func_id];

        let func_id = FuncProto::new(func, parent, &mut self.func_data.variables);

        let params = &func.params;

        let mut result_reg = 0;

        let graph = self.cfg;

        let mut beginning_end: HashMap<NodeIndex, (usize, Option<usize>)> = HashMap::new();
        let mut jmps_to_patch: HashMap<usize, NodeIndex> = HashMap::new();

        let mut dfs = Dfs::new(graph, NodeIndex::new(0));

        while let Some(node) = dfs.next(graph) {
            let beginning_of_node = self.func_protos[self.func_id].op_count;
            beginning_end.insert(node, (beginning_of_node, None));
            let node_weight = &self.cfg[node];

            self.compile_node(node, &graph, &mut jmps_to_patch)
                .expect("TODO: panic message");

            beginning_end.get_mut(&node).unwrap().1 = Some(self.func_protos[self.func_id].op_count);
        }

        for (idx, node) in &jmps_to_patch {
            let node_end = beginning_end[node].1.unwrap();

            let jmp_to = node_end as i32 - *idx as i32;

            self.patch_sentinal_jmp(*idx, jmp_to);
        }
        Ok(())
    }

    pub fn compile_all_funcs(&mut self, cfg: &'a CFGBuilder) {
        for key in self.func_pool.keys() {
            self.prep_for_next_func(cfg, key);
            let parent = if let Some(key) = self.func_data.child_to_parent.get(key) {
                Some(*key)
            } else {
                None
            };
            self.compile_func(key, parent);
        }
    }

    pub fn get_lit(&mut self, lit: Literal) -> Result<ConstId> {
        todo!()
    }

    pub fn emit_load_const(&mut self, lit: Literal) -> Result<RegOrConst> {
        let result = self.func_protos[self.func_id].acquire_reg();
        let lit_id = self.func_protos[self.func_id].const_pool.insert(lit);
        self.func_protos[self.func_id].insert_op(OpCode::LoadConst(result, lit_id));
        Ok(RegOrConst::Reg(result))
    }

    pub fn emit_arg_bytecode(&mut self, arg: ExprId) -> Result<RegOrConst> {
        todo!()
    }

    pub fn emit_call(&mut self, call: &Call) -> Result<RegOrConst> {
        todo!()
    }

    pub fn resolve_arg(&mut self, expr_id: ExprId) -> Option<RegOrConst> {
        todo!()
    }

    pub fn get_cmp_binary_bytecode(&mut self, binary: &Binary) {
        let lhs_reg = self.resolve_arg(binary.lhs).unwrap();
        let rhs_reg = self.resolve_arg(binary.rhs).unwrap();
        let op = match &binary.op {
            BinaryOp::GreaterEqual => OpCode::Ge(lhs_reg, rhs_reg),
            BinaryOp::LessEqual => OpCode::Le(lhs_reg, rhs_reg),
            BinaryOp::GreaterThan => OpCode::Gt(lhs_reg, rhs_reg),
            BinaryOp::LessThan => OpCode::Lt(lhs_reg, rhs_reg),
            _ => panic!(),
        };
        self.func_protos[self.func_id].insert_op(op);
    }

    pub fn get_binary_bytecode(&mut self, binary: &Binary) -> Result<RegOrConst> {
                let dst = self.func_protos[self.func_id].acquire_reg();
                let lhs_reg = self.resolve_arg(binary.lhs).unwrap();
                let rhs_reg = self.resolve_arg(binary.rhs).unwrap();
                println!("OP: {:?}", binary.op);
                let op = match &binary.op {
                    BinaryOp::Plus => OpCode::Add(dst, lhs_reg, rhs_reg),
                    BinaryOp::Minus => OpCode::Sub(dst, lhs_reg, rhs_reg),
                    BinaryOp::Slash => OpCode::Div(dst, lhs_reg, rhs_reg),
                    BinaryOp::Star => OpCode::Mul(dst, lhs_reg, rhs_reg),
                    //            BinaryOp::Mod => OpCode::Mod (lhs_reg, rhs_reg),
                    _ => panic!(),
                };
                let dst = self.func_protos[self.func_id].insert_op(op);
                Ok(RegOrConst::Reg(dst as u8))
    }

    pub fn emit_bin_bytecode(&mut self, binary: &Binary) -> Result<Option<RegOrConst>> {
        if binary.is_cmp() {
            self.get_cmp_binary_bytecode(binary);
            Ok(None)
        } else {
            Ok(Some(self.get_binary_bytecode(binary)?))
        }
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

    pub fn resolve_var(&mut self, name: Symbol) -> Result<RegOrConst> {
        let binding = Variables::lookup_binding(name, self.func_id, &mut self.func_data.variables)?;
        match binding {
            Some(Binding::Local(register)) => Ok(RegOrConst::Reg(register)),
            Some(Binding::Upvalue(upvalue_id)) => {
                let dest = self.func_protos[self.func_id].acquire_reg();
                self.func_protos[self.func_id].insert_op(OpCode::GetUpvalue(dest, upvalue_id));
                Ok(RegOrConst::Reg(dest))
            }
            None => {
                let name = self.emit_load_const(Literal::String(name))?;
                let dest = name;
                let dest = dest.get_reg().unwrap();
                self.func_protos[self.func_id].insert_op(OpCode::GetGlobal(dest, name));
                Ok(RegOrConst::Reg(dest))
            }
        }
    }

    pub fn resolve_value(&mut self, value: &Value) -> ConstId {
        match value {
            Value::Literal(lit) => self.func_protos[self.func_id].const_pool.insert(*lit),
            Value::Object(obj) => todo!(),
        }
    }

    pub fn compile_expr(&mut self, expr: ExprId) -> Result<RegOrConst> {
//        let expr = &self.func_data.expr_pools[self.func_id][expr];
        if let Some(expr) = &self.func_data.expr_pools[self.func_id].get(expr) {
        return Ok(match &expr {
            Expr::Binary(bin) => {
                if let Some(to_ret) = self.emit_bin_bytecode(bin)? {
                    to_ret
                } else {
                    RegOrConst::Reg(self.func_protos[self.func_id].next_reg)
                }
            }
            Expr::Unary(un) => self.emit_unary_bytecode(un)?,
            Expr::Assign(assign) => self.emit_assign_bytecode(assign.name, assign.val)?,

            Expr::Call(call) => self.emit_call(call)?,
            Expr::Value(val) => match val {
                Value::Literal(lit) => self.emit_load_const(*lit)?,
                Value::Object(obj) => {
                    todo!()
                }
            },

            Expr::Stmt(stmt) => {
                todo!()
            }
            Expr::Variable(var) => self.resolve_var(var.0)?,

            _ => {
                todo!()
            }
        }
            );

        }
        Err(anyhow!("todo"))
    }


    

    pub fn emit_var_bytecode(&mut self, name: &Symbol, val: RegOrConst) -> Result<RegOrConst> {
        todo!()
    }

    pub fn emit_assign_bytecode(&mut self, name: Symbol, expr_id: ExprId) -> Result<RegOrConst> {
        todo!();
        //        if let Some(src) = self.emit_expr_bytecode(expr_id) {
        //            let dst = src;
        //            let opcode = OpCode::Move(dst, src);
        //            self.func_protos[self.func_id].insert_op(opcode);
        //            return Ok(RegOrConst::Reg(dst));
    }

    pub fn gen_func_decl_bytecode(&self, cfg: &CFG, bytecode: &mut Bytecode) -> usize {
        todo!()
    }

    pub fn emit_bytecode(
        &mut self,
        hir: &HIR,
        jmps: &mut HashMap<usize, NodeIndex>,
    ) -> Result<RegOrConst> {
        match hir {
            HIR::Const(const_id) => {
                todo!()
            }
            HIR::Expr(expr_id) => self.compile_expr(*expr_id),
            HIR::Var(assign) => {
                todo!()

                //                let val = self.emit_expr_bytecode(assign.val);
                //                self.emit_var_bytecode(&assign.name, val).unwrap();
            }
            HIR::Assign(assign) => self.emit_assign_bytecode(assign.name, assign.val),
            HIR::DeclareFunc(func) => todo!(),
            HIR::Return0 => Ok(RegOrConst::Reg(
                self.func_protos[self.func_id].insert_op(OpCode::Return0) as u8,
            )),
            HIR::Return1(val) => {
                let arg = self.resolve_arg(*val);
                Ok(RegOrConst::Reg(
                    self.func_protos[self.func_id].insert_op(OpCode::Return1(arg.unwrap())) as u8,
                ))
            }
            HIR::Print(expr) => Ok(RegOrConst::Reg(
                self.func_protos[self.func_id].insert_op(OpCode::PrintExpr(*expr)) as u8,
            )),
            HIR::Jmp(jmp_node) => {
                jmps.insert(self.func_protos[self.func_id].op_count, *jmp_node);
                Ok(RegOrConst::Reg(
                    self.func_protos[self.func_id].insert_op(OpCode::Jmp(-1)) as u8,
                ))
            }
        }

        //        self.func_proto.insert_op(opcode);
    }

    pub fn compile_node(
        &mut self,
        node: NodeIndex,
        graph: &CFG,
        jmps: &mut HashMap<usize, NodeIndex>,
    ) -> Result<usize> {
        let block = &graph[node];
        for hir in block.borrow_stmts() {
            self.emit_bytecode(hir, jmps)?;

            // this is bad
            for succ in graph.edges_directed(node, Direction::Outgoing) {
                let target = succ.target();
                for (phi, block) in self.ssa.phis_to_block.0.iter() {
                    if *block == target {
                        todo!()
                        //                        println!("ASDASDA {:?}", self.ssa.phis_to_block.0[phi]);
                        //                        self.func_protos[self.func_id].bytecode.insert()
                    }
                }
            }
        }
        Ok(self.func_protos[self.func_id].op_count)
    }
}

#[derive(Debug)]
pub struct FuncProto {
    pub parent: Option<FuncId>,
    pub bytecode: Bytecode,
    pub name: Option<Symbol>,
    pub params: Vec<Symbol>,
    pub arity: u8,
    pub op_count: usize,
    pub const_pool: SlotMap<ConstId, Literal>,
    pub next_reg: RegIdx,
}

impl FuncProto {
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
impl FuncProto {
    fn new(
        val: &Function,
        parent: Option<FuncId>,
        func_data: &mut SecondaryMap<FuncId, Variables>,
    ) -> Self {
        const NIL: Value = Value::Literal(Literal::Nil);
        let params = val.params.clone();

        let mut variables = Variables::new(parent);
        let mut param_scope = Scope::new();
        let mut next_reg = 2;
        next_reg = param_scope.push_bindings(&params, next_reg).unwrap();
        variables.scopes.push(param_scope);

        func_data.insert(val.func_id, variables);

        Self {
            next_reg,
            parent,
            bytecode: Vec::new(),
            name: val.name,
            params,
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
