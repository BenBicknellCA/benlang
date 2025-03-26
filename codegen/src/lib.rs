use anyhow::{Result, anyhow};
use cfg::CFG;
use cfg::CFGBuilder;
use cfg::basic_block::BasicBlock;
use cfg::ir::ConstId;
use cfg::ir::HIR;
use cfg::ssa::SSABuilder;
use parser::FuncData;
use parser::FuncPool;
use parser::expr::{Binary, Unary};
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
use std::collections::HashMap;

pub type RegIdx = u8;

pub type PIdx = i32;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegOrConst {
    Reg(RegIdx),
    Const(ConstId),
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
    And(RegIdx, RegOrConst, RegOrConst),
    Or(RegIdx, RegOrConst, RegOrConst),
    Eq(RegIdx, RegOrConst, RegOrConst),
    Ne(RegIdx, RegOrConst, RegOrConst),
    Lt(RegIdx, RegOrConst, RegOrConst),
    Le(RegIdx, RegOrConst, RegOrConst),
    Gt(RegIdx, RegOrConst, RegOrConst),
    Ge(RegIdx, RegOrConst, RegOrConst),
    BAnd(RegIdx, RegOrConst, RegOrConst),
    BOr(RegIdx, RegOrConst, RegOrConst),
    Not(RegOrConst),
    Neg(RegOrConst),
    Jmp(PIdx),
    Return0,
    Return1(RegOrConst),
    PrintExpr(ExprId),
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

pub struct Generator<'a> {
    symbol_table: &'a SymbolTable,
    cfg: &'a CFG,
    func_data: &'a FuncData,
    ssa: &'a SSABuilder,
    func_pool: &'a FuncPool,
    func_id: FuncId,
    pub func_protos: SecondaryMap<FuncId, FuncProto>,
}

impl FuncProto {}

impl<'a> Generator<'a> {
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
    pub fn generate_func_proto(&mut self, func_id: FuncId) {
        let func: &Function = &self.func_pool[func_id];
        self.func_protos.insert(func_id, func.into());
        let root = NodeIndex::new(0);
        let graph = self.cfg;

        let mut beginning_end: HashMap<NodeIndex, (usize, Option<usize>)> = HashMap::new();
        let mut jmps_to_patch: HashMap<usize, NodeIndex> = HashMap::new();

        let mut dfs = Dfs::new(graph, root);

        while let Some(node) = dfs.next(graph) {
            let beginning_of_node = self.func_protos[self.func_id].op_count;
            beginning_end.insert(node, (beginning_of_node, None));
            let node_weight = &self.cfg[node];

            self.gen_bytecode_for_node_and_edges(node, &self.cfg[node], &mut jmps_to_patch);

            beginning_end.get_mut(&node).unwrap().1 = Some(self.func_protos[self.func_id].op_count);
        }

        for (idx, node) in &jmps_to_patch {
            let node_end = beginning_end[node].1.unwrap();

            let jmp_to = node_end as i32 - *idx as i32;

            self.patch_sentinal_jmp(*idx, jmp_to);
        }
    }

    pub fn generate_all_func_protos(&mut self, cfg: &'a CFGBuilder) {
        for key in self.func_pool.keys() {
            self.prep_for_next_func(cfg, key);
            self.generate_func_proto(key);
        }
    }

    pub fn emit_load_const(&mut self, value: &Value) -> Result<RegOrConst> {
        if let Value::Literal(literal) = value {
            let dst = self.get_free_reg_and_inc();
            let const_id = self.func_protos[self.func_id].const_pool.insert(*literal);
            self.func_protos[self.func_id].insert_op(OpCode::LoadConst(dst, const_id));
            return Ok(RegOrConst::Reg(dst));
        }
        Err(anyhow!("could not emit load const for {value:?}"))
    }

    pub fn resolve_arg(&mut self, expr_id: ExprId) -> RegOrConst {
        let expr = &self.func_data.expr_pools[self.func_id][expr_id];
        match expr {
            Expr::Value(val) => RegOrConst::Const(self.resolve_value(val)),
            Expr::Variable(var) => {
                RegOrConst::Reg(self.func_protos[self.func_id].var_to_reg[&var.0])
            }
            _ => todo!(),
        }
    }

    pub fn make_op_args() {}

    pub fn emit_bin_bytecode(&mut self, binary: &Binary) -> Result<RegOrConst> {
        let op = &binary.op;
        let dst = self.get_free_reg_and_inc();
        let lhs_reg = self.resolve_arg(binary.lhs);
        let rhs_reg = self.resolve_arg(binary.rhs);
        let opcode = match op {
            BinaryOp::Plus => OpCode::Add(dst, lhs_reg, rhs_reg),
            BinaryOp::Star => OpCode::Mul(dst, lhs_reg, rhs_reg),
            BinaryOp::Minus => OpCode::Sub(dst, lhs_reg, rhs_reg),
            BinaryOp::Slash => OpCode::Div(dst, lhs_reg, rhs_reg),
            BinaryOp::GreaterEqual => OpCode::Ge(dst, lhs_reg, rhs_reg),
            BinaryOp::LessEqual => OpCode::Le(dst, lhs_reg, rhs_reg),
            BinaryOp::Equal => OpCode::Eq(dst, lhs_reg, rhs_reg),
            BinaryOp::NotEqual => OpCode::Ne(dst, lhs_reg, rhs_reg),
            BinaryOp::Or => OpCode::Or(dst, lhs_reg, rhs_reg),
            BinaryOp::GreaterThan => OpCode::Gt(dst, lhs_reg, rhs_reg),
            BinaryOp::LessThan => OpCode::Lt(dst, lhs_reg, rhs_reg),
            _ => todo!(),
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

    pub fn resolve_value(&mut self, value: &Value) -> ConstId {
        match value {
            Value::Literal(lit) => self.func_protos[self.func_id].const_pool.insert(*lit),
            Value::Object(obj) => todo!(),
        }
    }

    pub fn resolve_var(&self, name: &Symbol) -> Result<u8> {
        Ok(self.func_protos[self.func_id].var_to_reg[name])
    }

    pub fn emit_expr_bytecode(&mut self, expr: ExprId) -> Result<RegOrConst> {
        let to_match: &Expr = &self.func_data.expr_pools[self.func_id][expr];
        match to_match {
            Expr::Binary(bin) => self.emit_bin_bytecode(bin),
            Expr::Unary(un) => self.emit_unary_bytecode(un),
            Expr::Assign(assign) => {
                let val = self.emit_expr_bytecode(assign.val);
                self.emit_assign_bytecode(assign.name, val?)
            }
            Expr::Call(_) => {
                todo!()
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

    pub fn get_free_reg_and_inc(&self) -> u8 {
        self.func_protos[self.func_id].get_free_reg_and_inc()
    }

    pub fn assign_var_to_reg(&mut self, var: Symbol) -> u8 {
        let idx = self.get_free_reg_and_inc();
        self.func_protos[self.func_id].var_to_reg.insert(var, idx);
        idx
    }

    pub fn emit_var_bytecode(&mut self, name: Symbol, val: RegOrConst) -> Result<RegOrConst> {
        let var_reg = self.assign_var_to_reg(name);
        let opcode = OpCode::Move(var_reg, val);
        self.func_protos[self.func_id].insert_op(opcode);
        Ok(var_reg.into())
    }

    pub fn emit_assign_bytecode(&mut self, name: Symbol, src: RegOrConst) -> Result<RegOrConst> {
        let dst = self.func_protos[self.func_id].var_to_reg[&name];
        let opcode = OpCode::Move(dst, src);
        self.func_protos[self.func_id].insert_op(opcode);
        Ok(RegOrConst::Reg(dst))
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
            HIR::Const(const_id) => {
                todo!()
            }
            HIR::Expr(expr_id) => {
                self.emit_expr_bytecode(*expr_id)?;
            }

            HIR::Var(assign) => {
                let val = self.emit_expr_bytecode(assign.val);
                self.emit_var_bytecode(assign.name, val?)?;
            }
            HIR::Assign(assign) => {
                todo!()
                //                return self.emit_assign_bytecode(assign.name, RegOrConst::Const(assign.val));
            }
            HIR::DeclareFunc(func) => todo!(),
            HIR::Return0 => {
                self.func_protos[self.func_id].insert_op(OpCode::Return0);
            }
            HIR::Return1(val) => {
                let arg = self.resolve_arg(*val);
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
    ) -> usize {
        for hir in block.borrow_stmts() {
            self.emit_bytecode(node, hir, jmps);
        }
        self.func_protos[self.func_id].op_count
    }
}
#[derive(Debug)]
pub struct FuncProto {
    pub bytecode: Bytecode,
    //    pub registers: [Value; 255],
    pub free_reg: Cell<u8>,
    pub var_to_reg: HashMap<Symbol, u8>,
    pub name: Option<Symbol>,
    pub arity: u8,
    pub op_count: usize,
    const_pool: SlotMap<ConstId, Literal>,
}

impl FuncProto {
    pub fn new(function: &Function, bytecode: Vec<OpCode>) -> Self {
        function.into()
    }

    pub fn get_free_reg_and_inc(&self) -> u8 {
        let idx = self.free_reg.get();
        self.free_reg.set(self.free_reg.get() + 1);
        idx
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

impl From<&Function> for FuncProto {
    fn from(val: &Function) -> Self {
        const NIL: Value = Value::Literal(Literal::Nil);
        let mut var_to_reg: HashMap<Symbol, u8> = HashMap::new();
        let mut count = 1;
        if let Some(params) = &val.params {
            for param in params {
                var_to_reg.insert(*param, count);
                count += 1;
            }
        }
        FuncProto {
            bytecode: Vec::new(),
            //            registers: [NIL; 255],
            free_reg: Cell::new(count),
            var_to_reg,
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
