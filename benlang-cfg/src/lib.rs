mod basic_block;
mod ir;

use crate::basic_block::BasicBlock;
use crate::ir::Ir;
use benlang_parser::expr_parser::ExprId;
use benlang_parser::stmt_parser::StmtId;
use benlang_parser::{
    ExprPool, StmtPool,
    expr::Expr,
    object::Function,
    stmt::{Block, Conditional, If, Stmt, While},
};

use petgraph::{
    Graph,
    graph::NodeIndex,
};

#[derive(Debug)]
pub struct CFGBuilder<'a> {
    current_node: NodeIndex,
    cfg: Graph<BasicBlock, Option<bool>>,
    stmt_pool: &'a StmtPool,
    expr_pool: &'a ExprPool,
}

impl<'a> CFGBuilder<'a> {
    fn new(stmt_pool: &'a StmtPool, expr_pool: &'a ExprPool) -> Self {
        let mut cfg: Graph<BasicBlock, Option<bool>> = Graph::new();
        let current_node = cfg.add_node(BasicBlock::default());
        CFGBuilder {
            stmt_pool,
            expr_pool,
            current_node,
            cfg,
        }
    }

    fn print_ir(&self, ir: &Ir) {
        //        match ir {
        //            Ir::Expr(expr) => {
        //                print!("expr: ");
        //                self.print_expr(expr);
        //            },
        //            _ =>
        //        }
        //        if let Some(stmt) = self.stmt_pool.get(stmt_id) {
        //            match stmt {
        //                Stmt::Expr(expr) => {
        //                    print!("expr stmt: ");
        //                    self.print_expr(*expr)
        //                }
        //                _ => println!("stmt: {stmt:?}"),
        //            }
        //        }
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
        self.cfg.add_node(BasicBlock::default())
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
        let current_weight = self.get_current_bb().statements.last();
        if let Some(Ir::Expr(last_stmt)) = current_weight {
            print!("LAST NON TERM STMT IN NODE BEFORE SETTING EMPTY AS CURRENT: ");
            self.print_expr(*last_stmt);
        } else {
            println!("NO LAST NON TERM STMT");
        }
        self.current_node = self.cfg.add_node(BasicBlock::default());
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

        let first_branch_block = cond_stmt.first_block();
        let first_branch_node = self.add_empty_node_and_set_current();

        self.cfg.add_edge(cond_idx, first_branch_node, Some(true));

        self.process_split_vec(first_branch_block.get_body_split_at_leaders().as_ref());
        (cond_idx, first_branch_node)
    }

    fn process_if_stmt(&mut self, if_stmt: &If) {
        let (cond_node, fbn) = self.prep_cond(if_stmt);
        let exit_node = self.add_empty_node();

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
        }
        self.current_node = exit_node;
    }

    fn process_while_stmt(&mut self, while_stmt: &While) {
        // condition node and while body node
        let (cond_node, wbn) = self.prep_cond(while_stmt);
        //while body back to condition, body back to condition is done in first edge created in
        //func
        self.cfg.add_edge(wbn, cond_node, None);
    }

    fn conditional<T: Conditional + std::fmt::Debug>(&mut self, cond_stmt: &T) {
        //        let exit_node = self.add_empty_node();
        //
        //        let is_while = T::is_while();
        //
        //        if is_while {
        //            //while body back to condition, body back to condition is done in first edge created in
        //            //func
        //            self.cfg.add_edge(first_branch_node, cond_idx, None);
        //
        //            self.cfg.add_edge(first_branch_node, exit_node, None);
        //        } else {
        //            self.cfg.add_edge(first_branch_node, exit_node, Some(false));
        //        };
        //
        //        if let Some(second_branch_block) = cond_stmt.second_block() {
        //            let second_branch_node = self.add_empty_node_and_set_current();
        //            self.cfg.add_edge(cond_idx, second_branch_node, Some(true));
        //
        //            self.cfg.add_edge(second_branch_node, exit_node, None);
        //
        //            self.process_split_vec(second_branch_block.get_body_split_at_leaders().as_ref());
        //        }
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
        if let Some(term_stmt) = self.stmt_pool.get(stmt) {
            match term_stmt {
                Stmt::If(ifstmt) => self.process_if_stmt(ifstmt),
                Stmt::While(whilestmt) => self.process_while_stmt(whilestmt),
                // block statements, different than If and While bodies
                Stmt::Block(blck) => {
                    self.block_stmt(blck);
                }
                Stmt::Return0 => self.ret_0(),
                Stmt::Return1(val) => self.ret_1(*val),
                Stmt::Function(_) => todo!(),
                _ => unreachable!("not a term"),
            }
            //            if !term_stmt.is_conditional() {
            //                self.add_uncond_exit_node();
            //            }
        }
    }

    fn stmt(&mut self, stmt_id: StmtId) {
        let stmt = &self.stmt_pool[stmt_id];
        // stmt is not a terminator
        if let Ok(ir_stmt) = Ir::try_from(stmt) {
            return self.add_ir_stmt_to_current_node(ir_stmt);
            //        self.cfg.get_(self.current_node).statements.push(ir_stmt);
        };
        self.term_stmt(stmt_id);

        //    if !BasicBlock::is_stmt_term(stmt) {};
        //        match stmt {
        //            Stmt::If(ifstmt) => {},
        //            Stmt::While(whilestmt) => {},
        //            Stmt::Var(name, val) => {},
        //            Stmt::Block(block) => {},
        //            Stmt::Expr(expr) => {},
        //            Stmt::Return0 => {},
        //            Stmt::Return1(val) => {},
        //            Stmt::Function(func) => {},
        //            Stmt::Print(prnt) => {},
        //        }
    }

    pub fn build_func_cfg(&mut self, func: &Function) {
        for stmt in &func.body.body {
            self.stmt(*stmt);
        }
    }
}

#[cfg(test)]
mod cfg_tests {

    use super::*;
    use benlang_parser::Parser;
    use benlang_parser::scanner::Scanner;
    use petgraph::dot::{Config, Dot};
    #[test]

    fn test_build_cfg() {
        static SOURCE: &str = "
            func test_func(first_param, second_param) {    
                    if (true == true) {
                    var test_var = 1 + 1;
                    test_var - 1;
                    } else
                    {
                    var extra_block = 123123;
                    while (true) 
                    {
                        2 * 2;
                        3 / 3;
                        1 - 1;
                    }
                    var afterwhile = 321321;
                }
                1 / 2 / 3;
            }";
        let mut scanner = Scanner::new(SOURCE);
        scanner.scan();

        let mut parser = Parser::new(scanner.tokens, scanner.interner);
        let ast = parser.build_ast().unwrap();
        let func_id: StmtId = ast[0];

        let mut cfg_builder = CFGBuilder::new(&parser.stmt_pool, &parser.expr_pool);
        if let Some(Stmt::Function(func)) = cfg_builder.stmt_pool.get(func_id) {
            cfg_builder.build_func_cfg(func);
//            println!(
//                "{:?}",
//                Dot::with_config(&cfg_builder.cfg, &[Config::EdgeNoLabel])
//            );
        }
    }
}
