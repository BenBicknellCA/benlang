mod basic_block;
pub mod ir;
mod min_ssa;
mod optimize;
mod phi;
pub mod ssa;

use crate::basic_block::{BasicBlock, TermKind};
use crate::ir::HIR;
use parser::FuncPool;
use parser::object::FuncId;

use crate::ssa::SSABuilder;
use anyhow::{Result, anyhow};
use parser::expr_parser::ExprId;
use parser::scanner::{Symbol, SymbolTable};
use parser::stmt_parser::StmtId;
use parser::{
    ExprPool, FuncData, StmtPool,
    expr::Expr,
    stmt::{Block, Conditional, If, Stmt, While},
};

use petgraph::{Graph, graph::NodeIndex};
use slotmap::SecondaryMap;

pub type CFG = Graph<BasicBlock, Option<bool>>;

pub struct CFGBuilder {
    current_node: NodeIndex,
    current_func: FuncId,
    pub func_data: FuncData,
    pub func_pool: FuncPool,
    pub func_to_cfg: SecondaryMap<FuncId, CFG>,
    pub func_to_ssa: SecondaryMap<FuncId, SSABuilder>,
    pub symbol_table: SymbolTable,
}

impl CFGBuilder {
    pub fn new(
        symbol_table: SymbolTable,
        current_func: FuncId,
        func_data: FuncData,
        func_pool: FuncPool,
    ) -> Self {
        let mut cfg: Graph<BasicBlock, Option<bool>> = Graph::new();

        let mut func_to_cfg = SecondaryMap::new();
        let mut func_to_ssa = SecondaryMap::new();

        let current_node = cfg.add_node(BasicBlock::default());
        cfg[current_node].node_index = current_node;

        let mut ssa = SSABuilder::new(current_node, &cfg);

        ssa.add_block(current_node, &cfg, false).unwrap();

        func_to_cfg.insert(current_func, cfg);
        func_to_ssa.insert(current_func, ssa);
        CFGBuilder {
            symbol_table,
            current_func,
            current_node,
            func_data,
            func_pool,
            func_to_ssa,
            func_to_cfg,
        }
    }

    pub fn build_cfgs(&mut self) -> Result<()> {
        let funcs = self
            .func_data
            .parent_to_children
            .remove(self.func_data.main)
            .unwrap();
        for func in funcs {
            self.build_func_cfg(func)?;
        }
        Ok(())
    }

    pub fn new_graph(&mut self) -> CFG {
        todo!()
    }

    pub fn build_func_cfg(&mut self, func: FuncId) -> Result<()> {
        self.current_func = func;
        let mut cfg: Graph<BasicBlock, Option<bool>> = Graph::new();

        let current_node = cfg.add_node(BasicBlock::default());
        cfg[current_node].node_index = current_node;
        self.current_node = current_node;

        let mut ssa = SSABuilder::new(current_node, &cfg);
        ssa.add_block(current_node, &cfg, false)?;

        self.func_to_cfg.insert(func, cfg);
        self.func_to_ssa.insert(func, ssa);

        let body = std::mem::take(&mut self.func_pool[func].body.body);

        self.stmts(&body)?;

        let phis: std::collections::HashSet<ssa::PhiId> =
            self.func_to_ssa[self.current_func].phis.0.keys().collect();
        self.remove_redundant_phis(&phis);
        Ok(())
    }

    fn get_expr_pool(&self, func_id: FuncId) -> &ExprPool {
        &self.func_data.expr_pools[func_id]
    }

    fn get_expr_pool_mut(&mut self, func_id: FuncId) -> &mut ExprPool {
        &mut self.func_data.expr_pools[func_id]
    }

    fn get_current_expr_pool(&self) -> &ExprPool {
        self.get_expr_pool(self.current_func)
    }

    fn get_current_expr_pool_mut(&mut self) -> &mut ExprPool {
        self.get_expr_pool_mut(self.current_func)
    }

    fn get_stmt_pool_mut(&mut self, func_id: FuncId) -> &mut StmtPool {
        &mut self.func_data.stmt_pools[func_id]
    }

    fn get_current_stmt_pool_mut(&mut self) -> &mut StmtPool {
        &mut self.func_data.stmt_pools[self.current_func]
    }

    fn get_stmt_pool(&self, func_id: FuncId) -> &StmtPool {
        &self.func_data.stmt_pools[func_id]
    }

    fn get_current_stmt_pool(&self) -> &StmtPool {
        self.get_stmt_pool(self.current_func)
    }

    fn print_expr(&self, expr_id: ExprId) {
        let expr = self.get_current_expr_pool().get(expr_id).unwrap();
        println!("expr: {expr:?}");
    }

    fn get_current_bb(&self) -> &BasicBlock {
        self.func_to_cfg[self.current_func]
            .node_weight(self.current_node)
            .unwrap()
    }

    fn get_current_bb_mut(&mut self) -> &mut BasicBlock {
        self.func_to_cfg[self.current_func]
            .node_weight_mut(self.current_node)
            .unwrap()
    }

    fn is_current_node_empty(&self) -> bool {
        self.get_current_bb().is_empty()
    }

    fn add_empty_node(&mut self, seal: bool) -> NodeIndex {
        let node_index = self.func_to_cfg[self.current_func].add_node(BasicBlock::default());
        self.func_to_cfg[self.current_func]
            .node_weight_mut(node_index)
            .unwrap()
            .node_index = node_index;
        self.func_to_ssa[self.current_func]
            .add_block(node_index, &self.func_to_cfg[self.current_func], seal)
            .unwrap();
        node_index
    }

    fn optimize_hir(
        expr_pool: &mut ExprPool,
        ssa: &mut SSABuilder,
        cfg: &CFG,
        node: NodeIndex,
        hir: &mut HIR,
    ) -> Result<()> {
        let expr_id = hir.get_expr()?;
        CFGBuilder::fold_constant(expr_pool, expr_id)?;
        CFGBuilder::propagate_copy_hir(expr_pool, ssa, cfg, node, hir)?;

        Ok(())
    }

    fn fold_expr(&mut self, expr_id: ExprId) {}

    fn add_ir_stmt_to_node(
        ssa: &mut SSABuilder,
        cfg: &mut CFG,
        node: NodeIndex,
        expr_pool: &mut ExprPool,
        hir_stmt: &mut HIR,
    ) -> Result<()> {
        CFGBuilder::optimize_hir(expr_pool, ssa, cfg, node, hir_stmt)?;
        if let Some(current_node) = cfg.node_weight_mut(node) {
            current_node.append_body(*hir_stmt);
            return Ok(());
        }

        Err(anyhow!("could not add ir stmt to node {node:?}"))
    }

    fn add_cond_to_node(
        expr_pool: &mut ExprPool,
        ssa: &mut SSABuilder,
        cfg: &mut CFG,
        node: NodeIndex,
        hir_stmt: &mut HIR,
    ) -> Result<bool> {
        CFGBuilder::optimize_hir(expr_pool, ssa, cfg, node, hir_stmt)?;
        if let Some(current_node) = cfg.node_weight_mut(node) {
            let cond = hir_stmt.get_value(expr_pool)?.is_bool();
            current_node.append_body(*hir_stmt);
            return Ok(cond);
        }
        Err(anyhow!("could not add cond to node {node:?}"))
    }

    fn get_cond_id<T: Conditional>(&self, cond: T) -> ExprId {
        cond.cond()
    }

    fn get_cond_ref<T: Conditional>(&self, cond: T) -> &Expr {
        self.get_current_expr_pool()
            .get(cond.cond())
            .expect("cond id")
    }

    fn expr_to_ir(&self, cond: ExprId) -> HIR {
        HIR::try_from(&Stmt::Expr(cond)).unwrap()
    }

    fn add_empty_node_and_set_current(&mut self, seal: bool) -> NodeIndex {
        self.current_node = self.add_empty_node(seal);
        self.current_node
    }

    fn set_term_kind(&mut self, node: NodeIndex, term_kind: TermKind) {
        self.func_to_cfg[self.current_func][node].term_kind = Some(term_kind);
    }

    fn process_if_stmt(&mut self, if_stmt: &If) -> Result<()> {
        CFGBuilder::fold_constant(self.get_current_expr_pool_mut(), if_stmt.cond())?;
        // if cond is true proceed normally

        // if cond is false do not process first branch
        let do_than_branch = self.get_current_expr_pool()[if_stmt.cond()]
            .get_bool()
            .is_ok_and(|res| res);

        // if cond is false but second branch exists, do not process first branch, process second branch as unconditional
        // todo: fix empty entry exit ndoes on skippign of than block
        let if_entry = self.current_node;
        self.set_term_kind(if_entry, TermKind::If);

        let if_exit = self.add_empty_node(false);

        // todo: eliminate `than` branch if condition folds to false
        self.func_to_ssa[self.current_func]
            .seal_block(if_entry, &self.func_to_cfg[self.current_func])?;

        let mut if_entry_else_entry_edge: (NodeIndex, Option<bool>) = (self.current_node, None);
        if do_than_branch {
            let mut cond_ir = self.expr_to_ir(if_stmt.cond());
            CFGBuilder::add_cond_to_node(
                &mut self.func_data.expr_pools[self.current_func],
                &mut self.func_to_ssa[self.current_func],
                &mut self.func_to_cfg[self.current_func],
                if_entry,
                &mut cond_ir,
            )?;
            //if `than` branch exists, edge from entry to `else` block is `false`

            let then_entry = self.add_empty_node_and_set_current(false);
            self.func_to_cfg[self.current_func].add_edge(if_entry, then_entry, Some(true));
            self.func_to_ssa[self.current_func]
                .seal_block(then_entry, &self.func_to_cfg[self.current_func])?;
            self.stmts(&if_stmt.first_block().body)?;

            self.func_to_cfg[self.current_func].add_edge(self.current_node, if_exit, None);
            if_entry_else_entry_edge = (self.current_node, Some(false));
        }

        if let Some(second_branch_block) = if_stmt.second_block() {
            let else_entry = if !do_than_branch {
                self.current_node
            } else {
                self.add_empty_node_and_set_current(false)
            };
            if if_entry != else_entry {
                self.func_to_cfg[self.current_func].add_edge(
                    if_entry,
                    else_entry,
                    if_entry_else_entry_edge.1,
                );
            }
            self.func_to_ssa[self.current_func]
                .seal_block(else_entry, &self.func_to_cfg[self.current_func])?;

            self.stmts(&second_branch_block.body)?;
            self.func_to_cfg[self.current_func].add_edge(self.current_node, if_exit, None);
        }

        self.current_node = if_exit;

        Ok(())
    }

    fn process_while_stmt(&mut self, while_stmt: &While) -> Result<()> {
        CFGBuilder::fold_constant(self.get_current_expr_pool_mut(), while_stmt.cond())?;

        let do_while_body = self.get_current_expr_pool()[while_stmt.cond()]
            .get_bool()
            .is_ok_and(|res| res);

        let while_entry = self.current_node;

        self.func_to_ssa[self.current_func]
            .seal_block(while_entry, &self.func_to_cfg[self.current_func])?;

        if !do_while_body {
            return Ok(());
        }
        let mut cond_ir = self.expr_to_ir(while_stmt.cond());

        let while_header = self.add_empty_node(false);
        self.set_term_kind(while_header, TermKind::While);

        CFGBuilder::add_cond_to_node(
            &mut self.func_data.expr_pools[self.current_func],
            &mut self.func_to_ssa[self.current_func],
            &mut self.func_to_cfg[self.current_func],
            while_header,
            &mut cond_ir,
        )?;
        self.func_to_cfg[self.current_func].add_edge(while_entry, while_header, None);
        let body_entry = self.add_empty_node(false);
        let while_exit = self.add_empty_node(false);
        self.func_to_cfg[self.current_func].add_edge(while_header, body_entry, Some(true));
        self.func_to_cfg[self.current_func].add_edge(while_header, while_exit, Some(false));
        self.func_to_ssa[self.current_func]
            .seal_block(body_entry, &self.func_to_cfg[self.current_func])?;
        self.current_node = body_entry;
        self.stmts(&while_stmt.first_block().body)?;

        let body_exit = self.current_node;
        self.func_to_ssa[self.current_func]
            .seal_block(body_exit, &self.func_to_cfg[self.current_func])?;
        self.func_to_cfg[self.current_func].add_edge(body_exit, while_header, None);
        self.func_to_ssa[self.current_func]
            .seal_block(while_header, &self.func_to_cfg[self.current_func])?;
        self.func_to_ssa[self.current_func]
            .seal_block(while_exit, &self.func_to_cfg[self.current_func])?;
        self.current_node = while_exit;
        Ok(())
    }

    fn ret_0(&mut self) {
        self.func_to_cfg[self.current_func][self.current_node].append_body(HIR::Return0);
        self.func_to_cfg[self.current_func][self.current_node].term_kind = Some(TermKind::Return0);
    }

    fn ret_1(&mut self, val: ExprId) {
        self.func_to_cfg[self.current_func][self.current_node].append_body(HIR::Return1(val));
        self.func_to_cfg[self.current_func][self.current_node].term_kind = Some(TermKind::Return1);
    }

    fn block_stmt(&mut self, block: &Block) -> Result<()> {
        let prev = self.current_node;
        let block_stmt_node = self.add_empty_node_and_set_current(false);
        self.func_to_cfg[self.current_func].add_edge(prev, block_stmt_node, None);
        self.stmts(&block.body)
        //        self.process_split_vec(&block.get_body_split_at_leaders());
    }

    fn term_stmt(&mut self, stmt: StmtId) -> Result<()> {
        assert!(self.get_current_stmt_pool()[stmt].is_term());
        if let Some(stmt) = self.get_current_stmt_pool_mut().remove(stmt) {
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
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, bin.lhs, vec);
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, bin.rhs, vec);
            }
            Expr::Unary(unary) => {
                let opnd = unary.opnd;
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, opnd, vec);
            }
            Expr::Stmt(_) => {
                todo!()
            }
            Expr::Call(_) => {
                todo!()
            }
            Expr::Assign(assign) => {
                CFGBuilder::get_all_vars_used_in_expr(expr_pool, assign.val, vec);
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
                self.func_to_ssa[self.current_func]
                    .seal_block(self.current_node, &self.func_to_cfg[self.current_func])?;
            }

            seal_prev = self.stmt(*stmt)?;
        }
        Ok(())
    }

    fn process_all_vars_in_stmt(&mut self, stmt_id: StmtId) -> Result<()> {
        let mut vec = Vec::new();
        let stmt = &self.func_data.stmt_pools[self.current_func][stmt_id];
        match stmt {
            Stmt::Block(blck) => for stmt in &blck.body {},
            Stmt::Expr(expr_id) => {
                if let Expr::Assign(assign) =
                    &self.func_data.expr_pools[self.current_func][*expr_id]
                {
                    self.func_to_ssa[self.current_func].write_variable(
                        assign.name,
                        self.current_node,
                        ssa::PhiOrExpr::Expr(assign.val),
                    )?;
                }
                self.func_to_ssa[self.current_func].process_all_vars_in_expr(
                    &self.func_data.expr_pools[self.current_func],
                    *expr_id,
                    &mut vec,
                    self.current_node,
                    &self.func_to_cfg[self.current_func],
                );
            }
            Stmt::Var(assign) => {
                self.func_to_ssa[self.current_func].write_variable(
                    assign.name,
                    self.current_node,
                    ssa::PhiOrExpr::Expr(assign.val),
                )?;
                self.func_to_ssa[self.current_func].process_all_vars_in_expr(
                    &self.func_data.expr_pools[self.current_func],
                    assign.val,
                    &mut vec,
                    self.current_node,
                    &self.func_to_cfg[self.current_func],
                );
            }
            _ => {}
        }
        Ok(())
    }

    fn stmt(&mut self, stmt_id: StmtId) -> Result<bool> {
        self.process_all_vars_in_stmt(stmt_id)?;
        let stmt = &self.get_current_stmt_pool()[stmt_id];
        if stmt.is_func() {
            return Ok(false);
        }
        if stmt.is_term() {
            self.term_stmt(stmt_id)?;
            return Ok(true);
        }

        if let Ok(mut ir_stmt) = HIR::try_from(&self.get_current_stmt_pool()[stmt_id]) {
            CFGBuilder::add_ir_stmt_to_node(
                &mut self.func_to_ssa[self.current_func],
                &mut self.func_to_cfg[self.current_func],
                self.current_node,
                &mut self.func_data.expr_pools[self.current_func],
                &mut ir_stmt,
            )?;
            return Ok(true);
        }
        Ok(false)
    }
}

#[cfg(test)]
mod cfg_tests {
    use super::*;
    use parser::Parser;
    use parser::scanner::Scanner;
    use petgraph::dot::{Config, Dot};
    // test_var * 2

    fn prep_parser_cfg() -> Parser {
        static SOURCE: &str = "
            func test_func(first_param, second_param) {
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
            }";
        let mut scanner = Scanner::new(SOURCE);
        scanner.scan();

        let mut parser = Parser::new(scanner.tokens, scanner.interner);
        parser
    }
    fn build_cfg_builder() -> CFGBuilder {
        let mut parser = prep_parser_cfg();
        parser.build_ast().unwrap();
        let ast = parser.ast;
        let main = parser.func_data.main;
        let func_data = parser.func_data;
        let func_pool = parser.func_pool;

        let mut cfg_builder = CFGBuilder::new(parser.interner, main, func_data, func_pool);
        cfg_builder
    }

    #[test]
    fn test_build_cfgs() {
        let mut cfg_builder = build_cfg_builder();

        cfg_builder.build_cfgs().unwrap();
        //        println!(
        //            "{:?}",
        //            Dot::with_config(&cfg_builder.cfg, &[Config::EdgeIndexLabel])
        //        );
        let phis: std::collections::HashSet<ssa::PhiId> = cfg_builder.func_to_ssa
            [cfg_builder.current_func]
            .phis
            .0
            .keys()
            .collect();
        cfg_builder.remove_redundant_phis(&phis);

        assert_eq!(
            cfg_builder.func_to_cfg[cfg_builder.current_func].node_count(),
            cfg_builder.func_to_ssa[cfg_builder.current_func]
                .sealed_blocks
                .len(),
        );
    }
}
