// Braun, M., Buchwald, S., Hack, S., Leißa, R., Mallon, C., Zwinkau, A. (2013). Simple and Efficient
// Construction of Static Single Assignment Form. In: Jhala, R., De Bosschere, K. (eds) Compiler
// Construction. CC 2013. Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg.
// https://doi.org/10.1007/978-3-642-37051-9_6

use crate::CFG;
use crate::phi::*;
use anyhow::{Result, anyhow};
use benlang_parser::{
    ExprPool,
    expr_parser::ExprId,
    scanner::{Symbol, SymbolTable},
};
use petgraph::graph::{Edges, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::{Directed, Direction};
use slotmap::new_key_type;
use std::cmp::PartialEq;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PhiOrExpr {
    Expr(ExprId),
    Phi(PhiId),
}

impl PhiOrExpr {
    fn is_a_phi(&self) -> bool {
        matches!(self, PhiOrExpr::Phi(_))
    }
}

new_key_type! {pub struct PhiId;}

pub struct SSABuilder {
    symbol_table: SymbolTable,
    pub incomplete_phis: IncompletePhis,
    pub var_defs: VarDefs,
    pub sealed_blocks: SealedBlocks,
    pub phis: Phis,
    pub phis_to_block: PhisToBlock,
    pub phi_operands: PhiOperands,
    pub phi_users: PhiUsers,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct User(NodeIndex, Symbol);

impl<'a> SSABuilder {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            incomplete_phis: IncompletePhis::new(),
            var_defs: VarDefs::new(),
            sealed_blocks: HashSet::new(),
            phis: Phis::new(),
            phis_to_block: PhisToBlock::new(),
            phi_operands: PhiOperands::new(),
            phi_users: PhiUsers::new(),
        }
    }


    // incomplete_phis: &mut IncompletePhis, var_defs: &mut VarDefs

    pub fn add_block(&mut self, block: NodeIndex, cfg: &CFG, seal: bool) -> Result<()> {
        self.borrow_inc_phis_mut_pub().insert(block, HashSet::new());
        self.borrow_var_defs_mut_pub().insert(block, HashMap::new());
        self.borrow_inc_phis_mut_pub().insert(block, HashSet::new());
        if seal {
            self.seal_block(block, cfg)?;
        };
        Ok(())
    }

    // sus
    pub fn reset(self) -> Self {
        SSABuilder::new(self.symbol_table)
    }

    pub fn add_user(&self, phi_users: &mut PhiUsers, usee: PhiId, name: Symbol, block: NodeIndex) {
        phi_users.get_mut(usee).push(User(block, name));
    }

    pub fn get_val_from_user(&self, user: User) -> &PhiOrExpr {
        &self.var_defs[user.0][&user.1]
    }

    pub fn remove_phi_own_user(
        &self,
        var_defs: &mut VarDefs,
        phi_users: &mut PhiUsers,
        phis_to_block: &PhisToBlock,
        phi: PhiId,
    ) {
        let phi_block = phis_to_block[phi];
        let users = phi_users[phi].iter();
        let mut trivial_idx = None;
        for (idx, user) in users.enumerate() {
            let name: Symbol = user.1;
            let to_test = var_defs[phi_block][&name];
            if to_test == PhiOrExpr::Phi(phi) {
                trivial_idx = Some(idx);
                break;
            }
        }
        if let Some(idx) = trivial_idx {
            phi_users[phi].swap_remove(idx);
            return;
        };
        unreachable!();
    }

    pub fn remove_user(
        &self,
        phi_users: &mut PhiUsers,
        usee: PhiId,
        name: Symbol,
        block: NodeIndex,
    ) {
        let user_vec = phi_users.get_mut(usee);
        let user = User(block, name);
        user_vec.retain(|&User(id, _)| id != user.0);
    }

    pub fn reroute_all_uses(
        &self,
        from: PhiId,
        to: PhiOrExpr,
        phi_users: &mut PhiUsers,
        phis_to_block: &mut PhisToBlock,
    ) {
        if let PhiOrExpr::Phi(to) = to {
            let mut users = std::mem::take(phi_users.get_mut(from));
            phi_users[to].append(&mut users);
            let to_block = self.phis_to_block[from];
            phis_to_block[from] = to_block;
        }
    }

    pub fn write_variable(
        &mut self,
        variable: Symbol,
        block: NodeIndex,
        value: PhiOrExpr,
    ) -> Result<()> {
        if let Some(map) = self.borrow_var_defs_mut_pub().get_mut(block) {
            if let Some(old_val) = map.get_mut(&variable) {
                *old_val = value;
                return Ok(());
            }
        }
        Err(anyhow!(
            "count not write value {:?} to variable {:?} in block {:?} ",
            value,
            variable,
            block
        ))
    }

    pub fn read_variable(
        &mut self,
        variable: Symbol,
        block: NodeIndex,
        cfg: &CFG,
    ) -> Result<PhiOrExpr> {
        if let Some(map) = self.var_defs.get(block) {
            if let Some(phi_or_expr) = map.get(&variable) {
                return Ok(*phi_or_expr);
            }
        }
        //        if var_defs.contains_key(*block) {
        //            return Ok(var_defs[*block][variable]);
        //        };
        self.read_variable_recursive(variable, block, cfg)
    }
    //    fn get_preds(&self, block: BBId, cfg: &CFG) -> Edges<Option<bool>, Directed> {
    fn get_preds_count(block: NodeIndex, cfg: &CFG) -> usize {
        SSABuilder::get_preds(block, cfg).count()
    }
    fn get_preds(block: NodeIndex, cfg: &'a CFG) -> Edges<'a, Option<bool>, Directed> {
        cfg.edges_directed(block, Direction::Incoming)
    }

    fn get_single_pred(block: NodeIndex, cfg: &CFG) -> NodeIndex {
        cfg.neighbors_directed(block, Direction::Incoming)
            .next()
            .unwrap()
    }

    fn new_phi(&mut self) -> PhiId {
        self.borrow_phis_mut_pub().new_phi()
    }

    fn add_new_phi_to_block(&mut self, block: NodeIndex) -> PhiId {
        let id = self.phis.new_phi();
        self.phis_to_block.insert(id, block);
        id
    }

    fn get_users(&self, phi: PhiId) -> &[User] {
        self.phi_users[phi].as_slice()
    }

    pub fn seal_block(&mut self, block: NodeIndex, cfg: &CFG) -> Result<()> {
        let mut to_process = Vec::new();
        while let Some(&var) = self.incomplete_phis[block].iter().next() {
            to_process.push(var);
        }

        for var in to_process {
            let dummy_phi = self.new_phi();
            self.add_phi_operands(var, dummy_phi, cfg)?;
        }
        self.borrow_sealed_blocks_mut_pub().insert(block);
        Ok(())
    }

    fn insert_phi_opnd(&self, phi_id: PhiId, val: PhiOrExpr) {}

    fn add_phi_operands(&mut self, variable: Symbol, phi_id: PhiId, cfg: &CFG) -> Result<()> {
        let phi_block = self.phis_to_block[phi_id];
        for pred in SSABuilder::get_preds(phi_block, cfg) {
            let src = pred.source();
            let val = self.read_variable(variable, pred.source(), cfg);
            if let Some(phi_opnds) = self.borrow_phi_opnds_mut_pub().get_mut(phi_id) {
                phi_opnds.push(val?);
            }
        }
        Ok(())
    }

    fn try_remove_trivial_phi(
        &self,
        phi: PhiId,
        phis: &mut Phis,
        var_defs: &mut VarDefs,
        phi_users: &mut PhiUsers,
        phis_to_block: &mut PhisToBlock,
    ) -> Result<PhiOrExpr> {
        let mut same: Option<PhiOrExpr> = None;
        let opnds = &self.phi_operands[phi];
        for opnd in opnds {
            if same.is_some_and(|same_val| same_val == *opnd) || opnd == &PhiOrExpr::Phi(phi) {
                continue;
            }
            if same.is_some() {
                return Ok(PhiOrExpr::Phi(phi));
            }
            same = Some(*opnd);
        }
        if same.is_none() {
            phis.remove(phi);
        }
        self.remove_phi_own_user(var_defs, phi_users, phis_to_block, phi);
        if let Some(same) = same {
            self.reroute_all_uses(phi, same, phi_users, phis_to_block);
        }

        let mut to_remove: Vec<PhiId> = Vec::new();
        for use_ in phi_users.get_mut(phi).iter() {
            if let PhiOrExpr::Phi(use_phi) = self.var_defs[use_.0][&use_.1] {
                to_remove.push(use_phi);
            }
        }
        for use_phi in to_remove {
            self.try_remove_trivial_phi(use_phi, phis, var_defs, phi_users, phis_to_block)?;
        }

        return Ok(same.unwrap());

        todo!()
    }

    pub fn process_all_vars_in_expr(
        &mut self,
        expr_pool: &ExprPool,
        expr_id: ExprId,
        vec: &mut Vec<Symbol>,
        block: NodeIndex,
        cfg: &CFG,
    ) {
        crate::CFGBuilder::get_all_vars_used_in_expr(expr_pool, expr_id, vec);
        for var in vec {
            let _ = self.read_variable(*var, block, cfg);
        }
    }

    fn read_variable_recursive(
        &mut self,
        variable: Symbol,
        block: NodeIndex,
        cfg: &CFG,
    ) -> Result<PhiOrExpr> {
        let mut val: Option<PhiOrExpr> = None;
        if !self.sealed_blocks.contains(&block) {
            let phi_id = self.add_new_phi_to_block(block);
            val = Some(PhiOrExpr::Phi(phi_id));
            if let Some(inc_phi) = self.borrow_inc_phis_mut_pub().0.get_mut(&block) {
                inc_phi.insert(variable);
            } else {
                self.borrow_inc_phis_mut_pub()
                    .0
                    .insert(block, HashSet::new());
                self.borrow_inc_phis_mut_pub()
                    .insert_at_block(block, variable);
            }
        } else if SSABuilder::get_preds_count(block, cfg) == 1 {
            let pred = SSABuilder::get_single_pred(block, cfg);
            val = Some(self.read_variable(variable, pred, cfg)?)
        } else {
            let phi_id = self.add_new_phi_to_block(block);
            val = Some(PhiOrExpr::Phi(phi_id));
            self.write_variable(variable, block, val.unwrap())?;
            self.add_phi_operands(variable, phi_id, cfg)?;
        }
        let val = val.unwrap();
        self.write_variable(variable, block, val)?;
        Ok(val)
    }






}

#[cfg(test)]
mod ssa_tests {

//    use super::*;
//    use crate::CFGBuilder;
//    use crate::cfg_tests::{build_cfg, prep_parser};
//    use benlang_parser::Parser;
//    use benlang_parser::scanner::Scanner;
//    use petgraph::dot::{Config, Dot};
//    #[test]
//    fn test_ssa() {
//        let parser = prep_parser();
//        let cfg: CFGBuilder = build_cfg();
//        let block = cfg.cfg.node_indices().last().unwrap();
//
//        let mut ssa_builder = SSABuilder::new(parser.interner);
//    }
}
