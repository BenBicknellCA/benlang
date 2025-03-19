mod basic_block;
mod ir;
mod phi;
mod ssa;

use crate::basic_block::BasicBlock;
use crate::ir::Ir;
use crate::ssa::SSABuilder;
use anyhow::Result;
use benlang_parser::expr_parser::ExprId;
use benlang_parser::scanner::{Symbol, SymbolTable};
use benlang_parser::stmt_parser::StmtId;
use benlang_parser::{
    ExprPool, StmtPool,
    expr::Expr,
    object::Function,
    stmt::{Block, Conditional, If, Stmt, While},
};

use petgraph::{Graph, graph::NodeIndex};

pub type CFG = Graph<BasicBlock, Option<bool>>;

pub struct CFGBuilder {
    current_node: NodeIndex,
    pub cfg: CFG,
    pub ssa: SSABuilder,
    pub stmt_pool: StmtPool,
    expr_pool: ExprPool,
    pub func_stmt_id: StmtId,
}

impl CFGBuilder {
    pub fn new(
        symbol_table: SymbolTable,
        stmt_pool: StmtPool,
        expr_pool: ExprPool,
        func_stmt_id: StmtId,
    ) -> Self {
        let mut cfg: Graph<BasicBlock, Option<bool>> = Graph::new();
        let current_node = cfg.add_node(BasicBlock::default());
        cfg[current_node].node_index = current_node;
        let mut ssa = SSABuilder::new(symbol_table, current_node, &cfg);
        ssa.add_block(current_node, &cfg, false).unwrap();
        CFGBuilder {
            cfg,
            ssa,
            stmt_pool,
            expr_pool,
            current_node,
            func_stmt_id,
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
        self.ssa.add_block(node_index, &self.cfg, false).unwrap();
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

    //    fn process_split_vec(&mut self, vec_vec: &[&[StmtId]]) {
    //        for vec in vec_vec {
    //            for stmt in vec.iter() {
    //                self.stmt(*stmt);
    //            }
    //        }
    //    }
    fn prep_cond<T: Conditional>(&mut self, cond_stmt: &T) -> Result<(NodeIndex, NodeIndex, NodeIndex,)> {
        let cond: ExprId = cond_stmt.cond();
        let cond_ir = self.expr_to_ir(cond);
        let cond_idx = self.current_node;
        let mut cond_vars = Vec::new();
        CFGBuilder::get_all_vars_used_in_expr(
            &self.expr_pool,
            cond_ir.get_expr().unwrap(),
            &mut cond_vars,
        );
        for var in cond_vars {
            self.ssa.read_variable(var, cond_idx, &self.cfg)?;
        }

        self.set_current_node_term(cond_ir);
        if !T::is_while() {
            self.ssa.seal_block(cond_idx, &self.cfg)?;
        }

        let first_branch_block = cond_stmt.first_block();
        let first_branch_node = self.add_empty_node_and_set_current();

        self.cfg.add_edge(cond_idx, first_branch_node, Some(true));
        if !T::is_while() {
            self.stmts(&first_branch_block.body)?;
        }

        Ok((cond_idx, first_branch_node, self.add_empty_node()))
    }

    fn process_if_stmt(&mut self, if_stmt: &If) -> Result<()> {
        let (cond_node, fbn, exit_node) = self.prep_cond(if_stmt)?;
        self.cfg.add_edge(fbn, exit_node, None);

        self.ssa.seal_block(fbn, &self.cfg)?;
        // condition to first branch handled in fn prep_cond

        // first branch to exit

        if let Some(second_branch_block) = if_stmt.second_block() {
            let second_branch_node = self.add_empty_node_and_set_current();

            // condition to second branch
            self.cfg.add_edge(cond_node, second_branch_node, Some(false));

            // second branch to exit
            self.cfg.add_edge(second_branch_node, exit_node, None);

            self.stmts(&second_branch_block.body)?;

            //            self.process_split_vec(second_branch_block.get_body_split_at_leaders().as_ref());
            self.ssa.seal_block(second_branch_node, &self.cfg)?;
        } else {
            self.cfg.add_edge(cond_node, exit_node, Some(false));
        }
        //        if !exit.is_empty() {
        //            for block in exit {
        //                self.stmts(block);
        //            }
        //        }
        self.ssa.seal_block(exit_node, &self.cfg)?;
        self.current_node = exit_node;
        Ok(())
    }

    fn process_while_stmt(&mut self, while_stmt: &While) -> Result<()> {
        // condition node and while body node
        let while_body = while_stmt.first_block();
        let (cond_node, wbn, while_exit_node) = self.prep_cond(while_stmt)?;
        //while body back to condition, body back to condition is done in first edge created in
        //func
//        self.cfg.add_edge(exit_node, cond_node, None);
        self.ssa.seal_block(wbn, &self.cfg)?;

        self.cfg.add_edge(cond_node, while_exit_node, Some(false));


        self.current_node = wbn;
        self.stmts(&while_body.body)?;

//        self.cfg.add_edge(self.current_node, while_exit_node, None);
        self.cfg.add_edge(self.current_node, cond_node, None);



        self.ssa.seal_block(cond_node, &self.cfg)?;
        self.ssa.seal_block(while_exit_node, &self.cfg)?;
        self.current_node = while_exit_node;


        //        println!("EXIT: {exit:?}");
        //        if !exit.is_empty() {
        //            for block in exit {
        //                self.stmts(block);
        //            }
        //        }
        Ok(())
    }

    fn ret_0(&mut self) {
        self.set_current_node_term(Ir::Return0);
    }

    fn ret_1(&mut self, val: ExprId) {
        self.set_current_node_term(Ir::Return1(val));
    }

    fn block_stmt(&mut self, block: &Block) -> Result<()> {
        let prev = self.current_node;
        let block_stmt_node = self.add_empty_node_and_set_current();
        self.cfg.add_edge(prev, block_stmt_node, None);
        self.stmts(&block.body)
        //        self.process_split_vec(&block.get_body_split_at_leaders());
    }

    fn term_stmt(&mut self, stmt: StmtId) -> Result<()> {
        assert!(self.stmt_pool[stmt].is_term());
        if let Some(stmt) = self.stmt_pool.remove(stmt) {
            match stmt {
                Stmt::If(ifstmt) => {
                    self.process_if_stmt(&ifstmt)?;
                }
                Stmt::While(whilestmt) => {
                    self.process_while_stmt(&whilestmt)?;
                }
                // block statements, different than If and While bodies
                Stmt::Block(blck) => {
                    self.block_stmt(&blck)?;
                }
                Stmt::Return0 => self.ret_0(),
                Stmt::Return1(val) => self.ret_1(val),
                Stmt::Function(_) => todo!(),
                _ => unreachable!("not a term: {stmt:?}"),
            };
        }
        Ok(())
    }

    fn get_all_vars_used_in_expr(expr_pool: &ExprPool, expr_id: ExprId, vec: &mut Vec<Symbol>) {
        let expr = &expr_pool[expr_id];
        match expr {
            Expr::Binary(bin) => {
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, bin.0, vec);
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, bin.2, vec);
            }
            Expr::Unary(unary) => {
                let opnd = unary.1;
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, opnd, vec);
            }
            Expr::Stmt(_) => {
                todo!()
            }
            Expr::Call(_) => {
                todo!()
            }
            Expr::Assign(assign) => {
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, assign.1, vec);
            }
            Expr::Value(_) => {
                // do nothing
            }
            Expr::Identifier(_) => {
                todo!()
            }
            Expr::Grouping(_) => {
                todo!()
            }
            Expr::Variable(var) => {
                vec.push(var.0);
            }
        };
    }

    fn stmts(&mut self, stmts: &[StmtId]) -> Result<()> {
        let mut seal_prev = false;
        for stmt in stmts {
            if seal_prev {
                self.ssa.seal_block(self.current_node, &self.cfg).unwrap();
            }

            seal_prev = self.stmt(*stmt)?;
        }
        Ok(())
    }

    fn stmt(&mut self, stmt_id: StmtId) -> Result<bool> {
        let stmt = &self.stmt_pool[stmt_id];

        // stmt is not a terminator
        if let Ok(ir_stmt) = Ir::try_from(stmt) {
            let mut vec: Vec<Symbol> = Vec::new();
            match ir_stmt {
                Ir::Var(name, val) => {
                    self.ssa
                        .write_variable(name, self.current_node, ssa::PhiOrExpr::Expr(val))
                        .unwrap();
                }
                Ir::Expr(expr_id) => {
                    if let Expr::Assign(assgn) = &self.expr_pool[expr_id] {
                        self.ssa
                            .write_variable(
                                assgn.0,
                                self.current_node,
                                ssa::PhiOrExpr::Expr(assgn.1),
                            )
                            .unwrap();
                    }
                }
                _ => {}
            }
            self.ssa.process_all_vars_in_expr(
                &self.expr_pool,
                ir_stmt.get_expr().unwrap(),
                &mut vec,
                self.current_node,
                &self.cfg,
            );
            self.add_ir_stmt_to_current_node(ir_stmt);
            return Ok(false);
        }
        assert!(self.stmt_pool[stmt_id].is_term());
        self.term_stmt(stmt_id)?;
        Ok(true)
    }

    pub fn build_func_cfg(&mut self, func: &Function) -> Result<()> {
        self.stmts(&func.body.body)
    }
}

#[cfg(test)]
mod cfg_tests {

    use super::*;
    use benlang_parser::Parser;
    use benlang_parser::scanner::Scanner;
    use petgraph::dot::{Config, Dot};
    // test_var * 2

    fn prep_parser_cfg() -> Parser {
        static SOURCE: &str = "
            func test_func(first_param, second_param) {
                    var test_var = 1;
                    var while_cond = true;
                    var counter = 0;
                    while (while_cond == true) {
                        counter = counter + 1;
                        if (counter == 1000) {
                            while_cond = false;
                        } else {
                            while_cond = true;
                        }
                    }
                    if (true) {
                        true;
                    }
                    var cond = true;
                    var counter = 0;

                    var new_var = test_var + 1;
            }";
        println!("SRC: {SOURCE}");
        let mut scanner = Scanner::new(SOURCE);
        scanner.scan();

        let mut parser = Parser::new(scanner.tokens, scanner.interner);
        parser
    }
    fn build_cfg() -> CFGBuilder {
        let mut parser = prep_parser_cfg();
        parser.build_ast().unwrap();
        let ast = parser.ast;
        let func_id: StmtId = ast[0];
        let func = &parser.stmt_pool[func_id].clone();
        let mut cfg_builder =
            CFGBuilder::new(parser.interner, parser.stmt_pool, parser.expr_pool, func_id);
        cfg_builder
    }

    #[test]
    fn test_build_cfg() {
        let mut cfg_builder = build_cfg();
        let func = &cfg_builder.stmt_pool[cfg_builder.func_stmt_id].clone();
        if let Stmt::Function(func) = func {
            cfg_builder.build_func_cfg(func).unwrap();
            println!(
                "{:?}",
                Dot::with_config(&cfg_builder.cfg, &[Config::EdgeNoLabel])
            );
        };
        assert_eq!(
            cfg_builder.cfg.node_count(),
            cfg_builder.ssa.sealed_blocks.len()
        );
    }
}
