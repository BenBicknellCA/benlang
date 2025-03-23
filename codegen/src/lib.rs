use cfg::CFG;
use cfg::ir::HIR;
use cfg::ssa::SSABuilder;
use parser::ExprPool;
use parser::FuncPool;
use parser::FuncData;
use parser::expr_parser::ExprId;
use parser::object::{FuncId, Function};
use parser::scanner::{Symbol, SymbolTable};
use parser::value::Literal;
use parser::value::Value;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::visit::IntoNeighborsDirected;
use std::collections::HashMap;

pub type Bytecode = Vec<OpCode>;

struct Generator {
    symbol_table: SymbolTable,
    func_data: FuncData,
    expr_pool: ExprPool,
    func_pool: FuncPool,
    ssa: SSABuilder,
}

impl Generator {
    pub fn new(
        symbol_table: SymbolTable,
        func_data: FuncData,
        expr_pool: ExprPool,
        ssa: SSABuilder,
        func_pool: FuncPool,
    ) -> Self {
        Self {
            symbol_table,
            func_data,
            expr_pool,
            ssa,
            func_pool,
        }
    }
    pub fn generate_bytecode_for_cfg(&self, cfg: &CFG, func_id: FuncId) -> FuncProto {
        let func: &Function = &self.func_pool[func_id];
        let mut func_state: FuncProto = func.into();
        let root = NodeIndex::new(0);
        let graph = cfg.neighbors_directed(root, Direction::Outgoing);
        for node in graph {
            self.gen_bytecode_for_node_and_edges(cfg, node, &mut func_state.bytecode);
        }
        //       let mut
        todo!()
    }

    pub fn gen_bin_bytecode(&self, cfg: &CFG, bytecode: &mut Bytecode) {}

    pub fn gen_unary_bytecode(&self, cfg: &CFG, bytecode: &mut Bytecode) {}

    pub fn gen_expr_bytecode(&self, expr: ExprId, cfg: &CFG, bytecode: &mut Bytecode) {}

    pub fn gen_var_assign_bytecode(
        &self,
        name: Symbol,
        val: ExprId,
        cfg: &CFG,
        bytecode: &mut Bytecode,
    ) {}

    pub fn gen_func_decl_bytecode(&self, cfg: &CFG, bytecode: &mut Bytecode) {}

    pub fn gen_bytecode_for_node_and_edges(
        &self,
        cfg: &CFG,
        node: NodeIndex,
        bytecode: &mut Bytecode,
    ) {
        let block = &cfg[node];
        for hir in block.borrow_stmts() {
            match hir {
                HIR::Expr(expr_id) => self.gen_expr_bytecode(*expr_id, cfg, bytecode),
                HIR::Var(name, val) => self.gen_var_assign_bytecode(*name, *val, cfg, bytecode),
                HIR::DeclareFunc(func) => todo!(),
                HIR::Return0 => bytecode.push(OpCode::Return0),
                HIR::Return1(val) => bytecode.push(OpCode::Return1(RegOrExpr::Expr(*val))),
                HIR::Print(expr) => bytecode.push(OpCode::PrintExpr(*expr)),
            }
        }
    }
}

struct FuncProto {
    pub bytecode: Bytecode,
    pub registers: [Value; 255],
    pub free_reg: u8,
    pub var_to_reg: HashMap<Symbol, u8>,
    pub name: Option<Symbol>,
    pub arity: u8,
}

impl FuncProto {
    pub fn new(function: &Function, bytecode: Vec<OpCode>) -> Self {
        function.into()
    }
}

impl Into<FuncProto> for &Function {
    fn into(self) -> FuncProto {
        const NIL: Value = Value::Literal(Literal::Nil);
        let mut var_to_reg: HashMap<Symbol, u8> = HashMap::new();
        let mut count = 1;
        if let Some(params) = &self.params {
            for param in params {
                var_to_reg.insert(*param, count);
                count += 1;
            }
        }
        FuncProto {
            bytecode: Vec::new(),
            registers: [NIL; 255],
            free_reg: count,
            var_to_reg,
            name: self.name,
            arity: self.arity,
        }
    }
}

pub type RegIdx = usize;

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
    Add(RegIdx, RegIdx, RegIdx),
    Sub(RegIdx, RegIdx, RegIdx),
    Mul(RegIdx, RegIdx, RegIdx),
    And(RegIdx, RegIdx, RegIdx),
    Or(RegIdx, RegIdx, RegIdx),
    Eq(RegIdx, RegIdx, RegIdx),
    Ne(RegIdx, RegIdx, RegIdx),
    Lt(RegIdx, RegIdx, RegIdx),
    Le(RegIdx, RegIdx, RegIdx),
    Gt(RegIdx, RegIdx, RegIdx),
    Ge(RegIdx, RegIdx, RegIdx),
    BAnd(RegIdx, RegIdx, RegIdx),
    BOr(RegIdx, RegIdx, RegIdx),
    Not(RegIdx),
    Jmp(PIdx),
    Return0,
    Return1(RegOrExpr),
    PrintExpr(ExprId),
    //    Div(RegIdx, RegIdx, RegIdx),
    //    Mod(RegIdx, RegIdx, RegIdx),
    //    Pow(RegIdx, RegIdx, RegIdx),
    //    BNot(RegIdx, RegIdx,
}
