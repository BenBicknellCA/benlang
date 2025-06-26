// Braun, M., Buchwald, S., Hack, S., Leißa, R., Mallon, C., Zwinkau, A. (2013). Simple and Efficient
// Construction of Static Single Assignment Form. In: Jhala, R., De Bosschere, K. (eds) Compiler
// Construction. CC 2013. Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg.
// https://doi.org/10.1007/978-3-642-37051-9_6

use crate::CFG;
use crate::phi::*;
use anyhow::{Result, anyhow};
use parser::{ExprPool, expr_parser::ExprId, scanner::Symbol};
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use slotmap::new_key_type;
use std::cmp::PartialEq;
use std::collections::{BTreeSet, HashMap, HashSet};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum PhiOrExpr {
    Expr(ExprId),
    Phi(PhiId),
}

impl PhiOrExpr {
    pub fn is_a_phi(&self) -> bool {
        matches!(self, PhiOrExpr::Phi(_))
    }

    pub fn get_expr(&self) -> Result<ExprId> {
        match self {
            PhiOrExpr::Expr(expr) => Ok(*expr),
            _ => Err(anyhow!("PhiOrExpr is a phi")),
        }
    }
}

new_key_type! {pub struct PhiId;}

pub struct SSABuilder {
    pub incomplete_phis: IncompletePhis,
    pub var_defs: VarDefs,
    pub sealed_blocks: SealedBlocks,
    pub phis: Phis,
    pub phis_to_block: PhisToBlock,
    pub phi_operands: PhiOperands,
    pub phi_users: PhiUsers,
    pub unique_vars: BTreeSet<Symbol>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct User(NodeIndex, Symbol);

impl SSABuilder {
    pub fn new(inital_node: NodeIndex, cfg: &CFG) -> Self {
        let mut ssa = Self {
            unique_vars: BTreeSet::new(),
            incomplete_phis: IncompletePhis::new(),
            var_defs: VarDefs::new(),
            sealed_blocks: HashSet::new(),
            phis: Phis::new(),
            phis_to_block: PhisToBlock::new(),
            phi_operands: PhiOperands::new(),
            phi_users: PhiUsers::new(),
        };
        ssa.add_block(inital_node, cfg, false).unwrap();
        ssa
    }

    pub fn add_block(&mut self, block: NodeIndex, cfg: &CFG, seal: bool) -> Result<NodeIndex> {
        self.borrow_var_defs_mut_pub().insert(block, HashMap::new());
        self.borrow_inc_phis_mut_pub().insert(block, HashMap::new());
        if seal {
            self.seal_block(block, cfg)?;
        };
        Ok(block)
    }

    pub fn add_user(&self, phi_users: &mut PhiUsers, usee: PhiId, name: Symbol, block: NodeIndex) {
        phi_users.get_mut(usee).push(User(block, name));
    }

    pub fn get_val_from_user(&self, user: User) -> &PhiOrExpr {
        &self.var_defs[user.0][&user.1]
    }

    pub fn remove_phi_own_user(&mut self, phi: PhiId) {
        let phi_block = self.phis_to_block[phi];
        let users = self.phi_users[phi].iter();
        let mut trivial_idx = None;
        for (idx, user) in users.enumerate() {
            let name: Symbol = user.1;
            let to_test = self.var_defs[phi_block][&name];
            if to_test == PhiOrExpr::Phi(phi) {
                trivial_idx = Some(idx);
                break;
            }
        }
        if let Some(idx) = trivial_idx {
            self.borrow_phi_users_mut_pub()[phi].swap_remove(idx);
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

    pub fn reroute_all_uses(&mut self, replace: PhiId, replace_with: PhiOrExpr) {
        // switch all uses of `replace` to `replace_with`
        if let PhiOrExpr::Phi(replace_with) = replace_with {
            let mut users = std::mem::take(&mut self.phi_users[replace]);
            self.borrow_phi_users_mut_pub()[replace_with].append(&mut users);
            self.phis_to_block[replace] = self.phis_to_block[replace_with];
        }
    }

    pub fn write_variable(
        &mut self,
        variable: Symbol,
        block: NodeIndex,
        value: PhiOrExpr,
    ) -> Result<()> {
        if let Some(map) = self.var_defs.get_mut(block) {
            map.insert(variable, value);
            self.unique_vars.insert(variable);
            return Ok(());
        }
        Err(anyhow!(
            "could not write value {:?} to variable {:?} in block {:?} ",
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
        if let Some(map) = self.var_defs.get(block)
            && let Some(phi_or_expr) = map.get(&variable)
        {
            return Ok(*phi_or_expr);
        }
        self.read_variable_recursive(variable, block, cfg)
    }
    fn get_preds_count(block: NodeIndex, cfg: &CFG) -> usize {
        SSABuilder::get_preds(block, cfg).count()
    }
    fn get_preds(block: NodeIndex, cfg: &CFG) -> petgraph::graph::Neighbors<Option<bool>> {
        cfg.neighbors_directed(block, Direction::Incoming)
    }

    fn get_single_pred(block: NodeIndex, cfg: &CFG) -> NodeIndex {
        cfg.neighbors_directed(block, Direction::Incoming)
            .next()
            .unwrap()
    }

    fn add_new_phi_to_block(&mut self, block: NodeIndex) -> PhiId {
        let id = self.borrow_phis_mut_pub().insert(Phi);
        self.phis_to_block.insert(id, block);
        self.phi_operands.insert(id, Vec::new());
        id
    }

    pub fn seal_block(&mut self, block: NodeIndex, cfg: &CFG) -> Result<()> {
        let inc_phis: HashMap<Symbol, PhiId> = std::mem::take(&mut self.incomplete_phis[block]);
        for (var, phi) in inc_phis {
            self.add_phi_operands(var, phi, cfg)?;
        }
        self.borrow_sealed_blocks_mut_pub().insert(block);
        Ok(())
    }

    fn add_phi_operands(&mut self, variable: Symbol, phi_id: PhiId, cfg: &CFG) -> Result<()> {
        for pred in SSABuilder::get_preds(self.phis_to_block[phi_id], cfg) {
            let opnd = self.read_variable(variable, pred, cfg)?;

            self.borrow_phi_opnds_mut_pub()[phi_id].push(opnd);
        }
        self.try_remove_trivial_phi(phi_id)?;
        Ok(())
    }

    fn try_remove_trivial_phi(&mut self, phi: PhiId) -> Result<PhiOrExpr> {
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
            self.borrow_phis_mut_pub().remove(phi);
        }
        self.remove_phi_own_user(phi);
        if let Some(same) = same {
            self.reroute_all_uses(phi, same);
        }

        let mut to_remove: Vec<PhiId> = Vec::new();
        for use_ in self.phi_users.get_mut(phi).iter() {
            if let PhiOrExpr::Phi(use_phi) = self.var_defs[use_.0][&use_.1] {
                to_remove.push(use_phi);
            }
        }
        for use_phi in to_remove {
            self.try_remove_trivial_phi(use_phi)?;
        }

        Ok(same.unwrap())
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
            self.read_variable(*var, block, cfg).unwrap();
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
            let phi_or_expr = PhiOrExpr::Phi(phi_id);
            self.borrow_inc_phis_mut_pub()[block].insert(variable, phi_id);
            val = Some(phi_or_expr);
        } else if SSABuilder::get_preds_count(block, cfg) == 1 {
            let pred = SSABuilder::get_single_pred(block, cfg);
            val = Some(self.read_variable(variable, pred, cfg)?)
        } else {
            let phi_id = self.add_new_phi_to_block(block);
            val = Some(PhiOrExpr::Phi(phi_id));
            self.write_variable(variable, block, val.unwrap())?;
            assert_eq!(self.var_defs[block][&variable], val.unwrap());
            self.add_phi_operands(variable, phi_id, cfg)?;
        }
        let val = val.unwrap();

        self.write_variable(variable, block, val)?;
        assert_eq!(self.var_defs[block][&variable], val);
        Ok(val)
    }
}

//#[cfg(test)]
//mod ssa_tests {

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
//}
