mod basic_block;
mod ir;

use crate::basic_block::BasicBlock;
use crate::ir::Ir;
use benlang_parser::stmt_parser::StmtId;
use benlang_parser::{ExprPool, StmtPool};

use petgraph::{Graph, graph::NodeIndex};

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

    fn handle_stmt(&mut self, stmt: StmtId) {
        let stmt = &self.stmt_pool[stmt];
        // stmt is not a terminator
        if let Ok(ir_stmt) = Ir::try_from(stmt) {
            if let Some(current_node) = self.cfg.node_weight_mut(self.current_node) {
                current_node.statements.push(ir_stmt);
            }
            //        self.cfg.get_(self.current_node).statements.push(ir_stmt);
        }
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
}
