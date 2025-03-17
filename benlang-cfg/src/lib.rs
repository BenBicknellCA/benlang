mod basic_block;
mod ir;
mod phi;
mod ssa;

use crate::basic_block::BasicBlock;
use crate::ir::Ir;
use crate::ssa::SSABuilder;
use benlang_parser::expr_parser::ExprId;
use benlang_parser::scanner::{Symbol, SymbolTable};
use benlang_parser::stmt_parser::StmtId;
use anyhow::{Result, Error};
use benlang_parser::{
    ExprPool, StmtPool,
    expr::Expr,
    object::Function,
    stmt::{Block, Conditional, If, Stmt, While},
};

use petgraph::{
    Direction, Graph,
    graph::{Edges, NodeIndex},
};
pub type CFG = Graph<BasicBlock, Option<bool>>;

pub struct CFGBuilder {
    current_node: NodeIndex,
    cfg: CFG,
    ssa: SSABuilder,
    stmt_pool: StmtPool,
    expr_pool: ExprPool,
}

impl CFGBuilder {
    fn new(symbol_table: SymbolTable, stmt_pool: StmtPool, expr_pool: ExprPool) -> Self {
        let mut cfg: Graph<BasicBlock, Option<bool>> = Graph::new();
        let current_node = cfg.add_node(BasicBlock::default());
        let mut ssa = SSABuilder::new(symbol_table);
        ssa.add_block(current_node, &cfg, false).unwrap();
        CFGBuilder {
            cfg,
            ssa,
            stmt_pool,
            expr_pool,
            current_node,
        }
    }

    fn print_expr(&self, expr_id: ExprId) {
        let expr = self.expr_pool.get(expr_id).unwrap();
        println!("expr: {expr:?}");
    }

    fn get_current_bb(&self) -> &BasicBlock {
        self.cfg.node_weight(self.current_node).unwrap()
    }

    fn get_current_bb_mut(&mut self) -> &mut BasicBlock {
        self.cfg.node_weight_mut(self.current_node).unwrap()
    }

    fn is_current_node_empty(&self) -> bool {
        self.get_current_bb().is_empty()
    }

    fn add_empty_node(&mut self) -> NodeIndex {
        let node_index = self.cfg.add_node(BasicBlock::default());
        self.cfg.node_weight_mut(node_index).unwrap().node_index = node_index;
        let _ = self.ssa.add_block(node_index, &self.cfg, false);
        node_index
    }

    fn add_ir_stmt_to_current_node(&mut self, ir_stmt: Ir) {
        if let Some(current_node) = self.cfg.node_weight_mut(self.current_node) {
            current_node.append_body(ir_stmt);
        }
    }

    fn set_current_node_term(&mut self, ir_stmt: Ir) {
        if let Some(current_node) = self.cfg.node_weight_mut(self.current_node) {
            current_node.set_term(ir_stmt);
        }
    }

    fn get_cond_id<T: Conditional>(&self, cond: T) -> ExprId {
        cond.cond()
    }

    fn get_cond_ref<T: Conditional>(&self, cond: T) -> &Expr {
        self.expr_pool.get(cond.cond()).expect("cond id")
    }

    fn expr_to_ir(&self, cond: ExprId) -> Ir {
        Ir::try_from(&Stmt::Expr(cond)).unwrap()
    }

    fn add_empty_node_and_set_current(&mut self) -> NodeIndex {
        self.current_node = self.add_empty_node();
        self.current_node
    }

    fn process_split_vec(&mut self, vec_vec: &[&[StmtId]]) {
        for vec in vec_vec {
            for stmt in vec.iter() {
                self.stmt(*stmt);
            }
        }
    }

    fn prep_cond<T: Conditional>(&mut self, cond_stmt: &T) -> (NodeIndex, NodeIndex) {
        let cond: ExprId = cond_stmt.cond();
        let cond_ir = self.expr_to_ir(cond);
        let cond_idx = self.current_node;

        self.set_current_node_term(cond_ir);

        self.ssa.seal_block(cond_idx, &self.cfg).unwrap();

        let first_branch_block = cond_stmt.first_block();
        let first_branch_node = self.add_empty_node_and_set_current();

        self.cfg.add_edge(cond_idx, first_branch_node, Some(true));

        self.process_split_vec(first_branch_block.get_body_split_at_leaders().as_ref());
        (cond_idx, first_branch_node)
    }

    fn process_if_stmt(&mut self, if_stmt: &If) -> Result<()> {
        let (cond_node, fbn) = self.prep_cond(if_stmt);
        let exit_node = self.add_empty_node();

        self.ssa.seal_block(fbn, &self.cfg)?;
        // condition to first branch handled in fn prep_cond

        // first branch to exit
        self.cfg.add_edge(fbn, exit_node, None);

        if let Some(second_branch_block) = if_stmt.second_block() {
            let second_branch_node = self.add_empty_node_and_set_current();

            // condition to second branch
            self.cfg.add_edge(cond_node, second_branch_node, Some(true));

            // second branch to exit
            self.cfg.add_edge(second_branch_node, exit_node, None);

            self.process_split_vec(second_branch_block.get_body_split_at_leaders().as_ref());
            self.ssa.seal_block(second_branch_node, &self.cfg)?;
        }
        self.current_node = exit_node;
        Ok(())
    }

    fn process_while_stmt(&mut self, while_stmt: &While) -> Result<()> {
        // condition node and while body node
        let (cond_node, wbn) = self.prep_cond(while_stmt);
        //while body back to condition, body back to condition is done in first edge created in
        //func
        self.cfg.add_edge(wbn, cond_node, None);
        self.ssa.seal_block(wbn, &self.cfg)?;
        Ok(())
    }

    fn ret_0(&mut self) {
        self.set_current_node_term(Ir::Return0);
    }

    fn ret_1(&mut self, val: ExprId) {
        self.set_current_node_term(Ir::Return1(val));
    }

    fn block_stmt(&mut self, block: &Block) {
        let prev = self.current_node;
        let block_stmt_node = self.add_empty_node_and_set_current();
        self.cfg.add_edge(prev, block_stmt_node, None);
        self.process_split_vec(&block.get_body_split_at_leaders());
    }

    fn term_stmt(&mut self, stmt: StmtId) {
        if let Some(stmt) = self.stmt_pool.remove(stmt) {
            assert!(stmt.is_term());
            match stmt {
                Stmt::If(ifstmt) => {
                    self.process_if_stmt(&ifstmt);
                }
                Stmt::While(whilestmt) => {
                    self.process_while_stmt(&whilestmt);
                }
                // block statements, different than If and While bodies
                Stmt::Block(blck) => {
                    self.block_stmt(&blck);
                }
                Stmt::Return0 => self.ret_0(),
                Stmt::Return1(val) => self.ret_1(val),
                Stmt::Function(_) => todo!(),
                _ => unreachable!("not a term: {stmt:?}"),
            };
        }
    }

    fn get_all_vars_used_in_expr(expr_pool: &ExprPool, expr_id: ExprId, vec: &mut Vec<Symbol>) {
        let expr = &expr_pool[expr_id];
        match expr {
            Expr::Binary(bin) => {
                let lhs = bin.0;
                let rhs = bin.2;
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, lhs, vec);
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, rhs, vec);
            }
            Expr::Unary(unary) => {
                let opnd = unary.1;
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, opnd, vec);
            }
            Expr::Stmt(stmt) => {
                todo!()
            }
            Expr::Call(call) => {
                todo!()
            }
            Expr::Assign(assign) => {
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, assign.1, vec);
            }
            Expr::Value(val) => {}
            Expr::Identifier(iden) => {
                todo!()
            }
            Expr::Grouping(group) => {
                todo!()
            }
            Expr::Variable(var) => {
                vec.push(var.0);
            }
        }
    }

    fn stmt(&mut self, stmt_id: StmtId) {
        let stmt = &self.stmt_pool[stmt_id];

        // stmt is not a terminator
        if let Ok(ir_stmt) = Ir::try_from(stmt) {
            let mut vec = Vec::new();
            if let Ir::Var(name, val) = ir_stmt {
                let _ = self
                    .ssa
                    .write_variable(name, self.current_node, ssa::PhiOrExpr::Expr(val));
            }
            self.ssa.process_all_vars_in_expr(
                &self.expr_pool,
                ir_stmt.get_expr().unwrap(),
                &mut vec,
                self.current_node,
                &self.cfg,
            );
            return;
        }
        assert!(self.stmt_pool[stmt_id].is_term());
        self.term_stmt(stmt_id);
    }

    pub fn build_func_cfg(&mut self, func: &Function) -> Result<()> {
        for stmt in &func.body.body {
            self.stmt(*stmt);
        }
        Ok(())
    }

    pub fn get_node_preds(&self, node: NodeIndex) -> Edges<Option<bool>, petgraph::Directed> {
        self.cfg.edges_directed(node, Direction::Incoming)
    }

    pub fn get_node_preds_count(&self, node: NodeIndex) -> usize {
        self.get_node_preds(node).count()
    }
}

#[cfg(test)]
mod cfg_tests {

    use super::*;
    use benlang_parser::Parser;
    use benlang_parser::scanner::Scanner;
    use petgraph::dot::{Config, Dot};

    pub fn prep_parser() -> Parser {
        static SOURCE: &str = "
            func test_func(first_param, second_param) {
                    var test_var = 1 + 1;
                    if (true == true) {
                        test_var = 3;
                    } else {
                        test_var = 4;
                    }
                    var extra_block = 123123;
                    while (true)
                    {
                        2 * 2;
                        3 / 3;
                        1 - 1;
                    }
                    var afterwhile = 321321;
                    1 / 2 / 3;
            }";
        let mut scanner = Scanner::new(SOURCE);
        scanner.scan();

        let mut parser = Parser::new(scanner.tokens, scanner.interner);
        let ast = parser.build_ast().unwrap();
        parser
    }

    pub fn build_cfg() -> CFGBuilder {
        let mut parser = prep_parser();
        let ast = parser.build_ast().unwrap();
        let func_id: StmtId = ast[0];
        let func = &parser.stmt_pool[func_id].clone();
        let mut cfg_builder = CFGBuilder::new(parser.interner, parser.stmt_pool, parser.expr_pool);
        if let Stmt::Function(func) = func {
            cfg_builder.build_func_cfg(func);
            //            println!(
            //                "{:?}",
            //                Dot::with_config(&cfg_builder.cfg, &[Config::EdgeNoLabel])
            //            );
        };
        cfg_builder
    }

    #[test]
    fn test_build_cfg() {
        let cfg = build_cfg();
        assert_eq!(cfg.cfg.node_count(), cfg.ssa.sealed_blocks.len());
    }
}
