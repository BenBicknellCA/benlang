use cfg::CFG;
use cfg::ir::HIR;
use cfg::ssa::SSABuilder;
use parser::ExprPool;
use parser::FuncData;
use parser::FuncPool;
use parser::expr::{Binary, Unary};
use parser::expr::{BinaryOp, Expr};
use parser::expr_parser::ExprId;
use parser::object::{FuncId, Function};
use parser::scanner::{Symbol, SymbolTable};
use parser::value::Literal;
use parser::value::Value;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::visit::IntoNeighborsDirected;
use std::cell::Cell;
use std::collections::HashMap;

pub type Bytecode = Vec<OpCode>;

struct Generator {
    symbol_table: SymbolTable,
    cfg: CFG,
    func_data: FuncData,
    expr_pool: ExprPool,
    func_pool: FuncPool,
    ssa: SSABuilder,
    func_proto: FuncProto,
}

impl Generator {
    pub fn new(
        symbol_table: SymbolTable,
        cfg: CFG,
        func_data: FuncData,
        expr_pool: ExprPool,
        ssa: SSABuilder,
        func_pool: FuncPool,
        func: &Function,
    ) -> Self {
        Self {
            symbol_table,
            cfg,
            func_data,
            expr_pool,
            ssa,
            func_pool,
            func_proto: func.into(),
        }
    }
    pub fn generate_func_proto(&mut self, cfg: &CFG, func_id: FuncId) -> FuncProto {
        let func: &Function = &self.func_pool[func_id];
        let func_state: FuncProto = func.into();
        let root = NodeIndex::new(0);
        let graph = cfg.neighbors_directed(root, Direction::Outgoing);
        let mut bytecode = Vec::new();
        for node in graph {
            self.gen_bytecode_for_node_and_edges(node, cfg[node].borrow_stmts(), &mut bytecode);
        }
        func_state
    }

    pub fn gen_load(&self, bytecode: &mut Bytecode, expr: ExprId) -> u8 {
        let dst = self.get_free_reg_and_inc();
        bytecode.push(OpCode::LoadVal(dst, expr));
        dst
    }

    pub fn make_op_args() {}

    pub fn gen_bin_bytecode(&self, binary: &Binary, bytecode: &mut Bytecode) {
        let op = &binary.op;
        let rhs = &binary.rhs;
        let dst = self.get_free_reg_and_inc();
        let lhs_reg = self.gen_load(bytecode, binary.lhs).into();
        let rhs_reg = self.gen_load(bytecode, binary.rhs).into();
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
        bytecode.push(opcode);
    }

    pub fn gen_unary_bytecode(&self, unary: &Unary, bytecode: &mut Bytecode) {}

    pub fn gen_expr_bytecode(&self, expr: ExprId, bytecode: &mut Bytecode) {
        let expr = &self.expr_pool[expr];
        match expr {
            Expr::Binary(bin) => self.gen_bin_bytecode(bin, bytecode),
            Expr::Unary(un) => self.gen_unary_bytecode(un, bytecode),
            Expr::Call(_) => {
                todo!()
            }
            Expr::Stmt(stmt) => {
                todo!()
            }
            _ => {
                panic!("todo: {expr:?}")
            }
        }
    }

    pub fn get_free_reg_and_inc(&self) -> u8 {
        self.func_proto.get_free_reg_and_inc()
    }

    pub fn assign_var_to_reg(&mut self, var: Symbol) -> u8 {
        let idx = self.get_free_reg_and_inc();
        self.func_proto.var_to_reg.insert(var, idx);
        idx
    }

    pub fn gen_var_assign_bytecode(&mut self, name: Symbol, val: ExprId, bytecode: &mut Bytecode) {
        let reg = self.assign_var_to_reg(name);
        bytecode.push(OpCode::LoadVal(reg, val))
    }

    pub fn gen_func_decl_bytecode(&self, cfg: &CFG, bytecode: &mut Bytecode) {}

    pub fn gen_bytecode_for_hir(&mut self, node: NodeIndex, hir: &HIR, bytecode: &mut Bytecode) {
        let opcode = match hir {
            HIR::Expr(expr_id) => return self.gen_expr_bytecode(*expr_id, bytecode),
            HIR::Var(name, val) => return self.gen_var_assign_bytecode(*name, *val, bytecode),
            HIR::DeclareFunc(func) => todo!(),
            HIR::Return0 => OpCode::Return0,
            HIR::Return1(val) => OpCode::Return1(RegOrExpr::Expr(*val)),
            HIR::Print(expr) => OpCode::PrintExpr(*expr),
        };
        bytecode.push(opcode);
    }

    pub fn gen_bytecode_for_node_and_edges(
        &mut self,
        node: NodeIndex,
        block: &[HIR],
        bytecode: &mut Bytecode,
    ) {
        for hir in block {
            self.gen_bytecode_for_hir(node, hir, bytecode);
        }
    }
}

struct FuncProto {
    pub bytecode: Bytecode,
    pub registers: [Value; 255],
    pub free_reg: Cell<u8>,
    pub var_to_reg: HashMap<Symbol, u8>,
    pub name: Option<Symbol>,
    pub arity: u8,
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
            registers: [NIL; 255],
            free_reg: Cell::new(count),
            var_to_reg,
            name: val.name,
            arity: val.arity,
        }
    }
}

pub type RegIdx = u8;

pub type PIdx = i32;

#[derive(Debug)]
pub enum RegOrExpr {
    Reg(RegIdx),
    Expr(ExprId),
}

#[derive(Debug)]
pub enum OpCode {
    // dst , src
    Move(RegIdx, RegIdx),
    LoadVal(RegIdx, ExprId),
    LoadTrue(RegIdx),
    LoadFalse(RegIdx),
    LoadNil(RegIdx),
    Add(RegIdx, RegOrExpr, RegOrExpr),
    Sub(RegIdx, RegOrExpr, RegOrExpr),
    Mul(RegIdx, RegOrExpr, RegOrExpr),
    Div(RegIdx, RegOrExpr, RegOrExpr),
    Mod(RegIdx, RegOrExpr, RegOrExpr),
    And(RegIdx, RegOrExpr, RegOrExpr),
    Or(RegIdx, RegOrExpr, RegOrExpr),
    Eq(RegIdx, RegOrExpr, RegOrExpr),
    Ne(RegIdx, RegOrExpr, RegOrExpr),
    Lt(RegIdx, RegOrExpr, RegOrExpr),
    Le(RegIdx, RegOrExpr, RegOrExpr),
    Gt(RegIdx, RegOrExpr, RegOrExpr),
    Ge(RegIdx, RegOrExpr, RegOrExpr),
    BAnd(RegIdx, RegOrExpr, RegOrExpr),
    BOr(RegIdx, RegOrExpr, RegOrExpr),
    Not(RegOrExpr),
    Jmp(PIdx),
    Return0,
    Return1(RegOrExpr),
    PrintExpr(ExprId),
    //    Div(RegIdx, RegIdx, RegIdx),
    //    Mod(RegIdx, RegIdx, RegIdx),
    //    Pow(RegIdx, RegIdx, RegIdx),
    //    BNot(RegIdx, RegIdx,
}

impl From<u8> for RegOrExpr {
    fn from(val: u8) -> Self {
        RegOrExpr::Reg(val)
    }
}

impl From<ExprId> for RegOrExpr {
    fn from(val: ExprId) -> Self {
        RegOrExpr::Expr(val)
    }
}
