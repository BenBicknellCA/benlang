// Braun, M., Buchwald, S., Hack, S., Leißa, R., Mallon, C., Zwinkau, A. (2013). Simple and Efficient
// Construction of Static Single Assignment Form. In: Jhala, R., De Bosschere, K. (eds) Compiler
// Construction. CC 2013. Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg.
// https://doi.org/10.1007/978-3-642-37051-9_6

use crate::CFG;
use crate::phi::*;
use anyhow::{Error, Result, anyhow, ensure};
use benlang_parser::{
    ExprPool, StmtPool,
    expr_parser::ExprId,
    scanner::{Symbol, SymbolTable},
};
use petgraph::graph::{EdgeReference, Edges, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::{Directed, Direction};
use slotmap::{SecondaryMap, SlotMap, new_key_type};
use std::cmp::PartialEq;
use std::collections::{HashMap, HashSet};
use std::ops::Index;
use thiserror::Error;

#[derive(Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Error)]
pub enum SSAError {
    #[error("Cannot write SSA variable {symbol:?} to block {node_index:?}")]
    CannotWrite {
        symbol: Symbol,
        node_index: NodeIndex,
    },
    #[error("Cannot read SSA variable {symbol:?} from block {node_index:?}")]
    CannotRead {
        symbol: Symbol,
        node_index: NodeIndex,
    },
}

pub struct SSABuilder {
    symbol_table: SymbolTable,
    incomplete_phis: IncompletePhis,
    var_defs: VarDefs,
    sealed_blocks: SealedBlocks,
    phis: Phis,
    phis_to_block: PhisToBlock,
    phi_operands: PhiOperands,
    phi_users: PhiUsers,
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

    pub fn add_block(
        &self,
        block: NodeIndex,
        incomplete_phis: &mut IncompletePhis,
        phi_operands: &mut PhiOperands,
        cfg: &CFG,
        phis: &mut Phis,
        var_defs: &mut VarDefs,
        phis_to_block: &mut PhisToBlock,
        seal: bool,
        sealed_blocks: &mut SealedBlocks,
    ) -> Result<()> {
        incomplete_phis.insert(block, HashSet::new());
        var_defs.insert(block, HashMap::new());
        if seal {
            self.seal_block(
                block,
                incomplete_phis,
                phi_operands,
                cfg,
                phis,
                var_defs,
                phis_to_block,
                sealed_blocks,
            )?;
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

    fn write_variable(
        &self,
        var_defs: &mut VarDefs,
        variable: &Symbol,
        block: NodeIndex,
        value: PhiOrExpr,
    ) -> Result<()> {
        if let Some(map) = var_defs.get_mut(block) {
            if let Some(old_val) = map.get_mut(variable) {
                if let PhiOrExpr::Phi(id) = value {}
                *old_val = value;
                return Ok(());
            }
        }
        Err(SSAError::CannotWrite {
            symbol: *variable,
            node_index: block,
        }
        .into())
    }

    fn read_variable(
        &self,
        incomplete_phis: &mut IncompletePhis,
        variable: &Symbol,
        block: &NodeIndex,
        cfg: &CFG,
        phis: &mut Phis,
        phi_operands: &mut PhiOperands,
        var_defs: &mut VarDefs,
        phis_to_block: &mut PhisToBlock,
    ) -> Result<PhiOrExpr> {
        if self.var_defs.contains_key(*block) {
            return Ok(self.var_defs[*block][variable]);
        };
        self.read_variable_recursive(
            incomplete_phis,
            variable,
            block,
            cfg,
            phis,
            phi_operands,
            var_defs,
            phis_to_block,
        )
    }
    //    fn get_preds(&self, block: BBId, cfg: &CFG) -> Edges<Option<bool>, Directed> {
    fn get_preds_count(block: &NodeIndex, cfg: &CFG) -> usize {
        SSABuilder::get_preds(block, cfg).count()
    }
    fn get_preds(block: &NodeIndex, cfg: &'a CFG) -> Edges<'a, Option<bool>, Directed> {
        cfg.edges_directed(*block, Direction::Incoming)
    }

    fn get_single_pred(&self, block: &NodeIndex, cfg: &CFG) -> NodeIndex {
        cfg.neighbors_directed(*block, Direction::Incoming)
            .next()
            .unwrap()
    }

    fn new_phi(phis: &mut Phis) -> PhiId {
        phis.new_phi()
    }

    fn add_new_phi_to_block(
        &self,
        phis: &mut Phis,
        phis_to_block: &mut PhisToBlock,
        block: &NodeIndex,
    ) -> PhiId {
        let id = SSABuilder::new_phi(phis);
        phis_to_block[id] = *block;

        id
    }

    fn get_users(&self, phi: PhiId) -> &[User] {
        self.phi_users[phi].as_slice()
    }

    fn seal_block(
        &self,
        block: NodeIndex,
        incomplete_phis: &mut IncompletePhis,
        phi_operands: &mut PhiOperands,
        cfg: &CFG,
        phis: &mut Phis,
        var_defs: &mut VarDefs,
        phis_to_block: &mut PhisToBlock,
        sealed_blocks: &mut SealedBlocks,
    ) -> Result<()> {
        for variable in &self.incomplete_phis[&block] {
            let dummy_phi = SSABuilder::new_phi(phis);
            self.add_phi_operands(
                incomplete_phis,
                phi_operands,
                *variable,
                dummy_phi,
                cfg,
                phis,
                var_defs,
                phis_to_block,
            )?;
        }
        sealed_blocks.insert(block);
        Ok(())
    }

    fn add_phi_operands(
        &self,
        incomplete_phis: &mut IncompletePhis,
        phi_operands: &mut PhiOperands,
        variable: Symbol,
        phi_id: PhiId,
        cfg: &CFG,
        phis: &mut Phis,
        var_defs: &mut VarDefs,
        phis_to_block: &mut PhisToBlock,
    ) -> Result<()> {
        let phi_block = &self.phis_to_block[phi_id];
        for pred in SSABuilder::get_preds(phi_block, cfg) {
            let src = pred.source();
            let val = self.read_variable(
                incomplete_phis,
                &variable,
                &pred.source(),
                cfg,
                phis,
                phi_operands,
                var_defs,
                phis_to_block,
            );
            phi_operands[phi_id].push(val?);
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

    fn read_variable_recursive(
        &self,
        incomplete_phis: &mut IncompletePhis,
        variable: &Symbol,
        block: &NodeIndex,
        cfg: &CFG,
        phis: &mut Phis,
        phi_operands: &mut PhiOperands,
        var_defs: &mut VarDefs,
        phis_to_block: &mut PhisToBlock,
    ) -> Result<PhiOrExpr> {
        let mut val: Option<PhiOrExpr> = None;
        if !self.sealed_blocks.contains(block) {
            let phi_id = self.add_new_phi_to_block(phis, phis_to_block, block);
            val = Some(PhiOrExpr::Phi(phi_id));
            incomplete_phis.get_mut(block).unwrap().insert(*variable);
        } else if SSABuilder::get_preds_count(block, cfg) == 1 {
            let pred = self.get_single_pred(block, cfg);
            val = Some(self.read_variable(
                incomplete_phis,
                variable,
                &pred,
                cfg,
                phis,
                phi_operands,
                var_defs,
                phis_to_block,
            )?)
        } else {
            let phi_id = self.add_new_phi_to_block(phis, phis_to_block, block);
            val = Some(PhiOrExpr::Phi(phi_id));
            self.write_variable(var_defs, variable, *block, val.unwrap())?;
            self.add_phi_operands(
                incomplete_phis,
                phi_operands,
                *variable,
                phi_id,
                cfg,
                phis,
                var_defs,
                phis_to_block,
            )?;
        }
        let val = val.unwrap();
        self.write_variable(var_defs, variable, *block, val)?;
        Ok(val)
    }
}

#[cfg(test)]
mod cfg_tests {

    use super::*;
    use crate::cfg_tests::{build_cfg, prep_parser};
    use benlang_parser::Parser;
    use benlang_parser::scanner::Scanner;
    use petgraph::dot::{Config, Dot};
    #[test]
    fn test_ssa() {
        let parser = prep_parser();
        let cfg = build_cfg();

        let ssa_builder = SSABuilder::new(parser.interner);

//        let var_1_sym = ssa_builder.symbol_table[]
    }
}
