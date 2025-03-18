use crate::ssa::{PhiId, PhiOrExpr, User};
use benlang_parser::scanner::Symbol;
use petgraph::graph::NodeIndex;
use slotmap::{SecondaryMap, SlotMap};
use std::collections::{HashMap, HashSet};
use std::ops::Index;

#[derive(Debug, Default)]
pub struct Phi;
pub type PhiOperands = SecondaryMap<PhiId, Vec<PhiOrExpr>>;

#[derive(Debug)]
pub struct IncompletePhis(pub HashMap<NodeIndex, HashMap<Symbol, PhiId>>);
impl IncompletePhis {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&self, node_index: NodeIndex) -> &HashMap<Symbol, PhiId> {
        self.0.get(&node_index).unwrap()
    }
    pub fn insert(&mut self, node_index: NodeIndex, map: HashMap<Symbol, PhiId>) {
        self.0.insert(node_index, map);
    }

    pub fn insert_at_block(&mut self, node_index: NodeIndex, variable: Symbol, phi_id: PhiId) {
        self.0
            .get_mut(&node_index)
            .unwrap()
            .insert(variable, phi_id);
    }
}

impl std::ops::IndexMut<NodeIndex> for IncompletePhis {
    fn index_mut(&mut self, node_index: NodeIndex) -> &mut HashMap<Symbol, PhiId> {
        self.0.get_mut(&node_index).unwrap()
    }
}

impl Index<NodeIndex> for IncompletePhis {
    type Output = HashMap<Symbol, PhiId>;
    fn index(&self, node_index: NodeIndex) -> &Self::Output {
        &self.0[&node_index]
    }
}

pub type SealedBlocks = HashSet<NodeIndex>;
#[derive(Debug)]
pub struct VarDefs(HashMap<NodeIndex, HashMap<Symbol, PhiOrExpr>>);
impl VarDefs {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn insert(&mut self, block: NodeIndex, map: HashMap<Symbol, PhiOrExpr>) {
        self.0.insert(block, map);
    }
    pub fn get(&self, block: NodeIndex) -> Option<&HashMap<Symbol, PhiOrExpr>> {
        self.0.get(&block)
    }

    pub fn get_mut(&mut self, block: NodeIndex) -> Option<&mut HashMap<Symbol, PhiOrExpr>> {
        self.0.get_mut(&block)
    }

    pub fn contains_key(&self, block: NodeIndex) -> bool {
        self.0.contains_key(&block)
    }
}
pub struct PhiUsers(pub SecondaryMap<PhiId, Vec<User>>);
impl PhiUsers {
    pub fn new() -> Self {
        PhiUsers(SecondaryMap::new())
    }

    pub fn insert(&mut self, phi_node: PhiId, user: User) {
        self.0[phi_node].push(user);
    }

    pub fn append(&mut self, phi_node: PhiId, mut users: Vec<User>) {
        self.0[phi_node].append(&mut users);
    }

    pub fn get_mut(&mut self, phi_node: PhiId) -> &mut Vec<User> {
        self.0[phi_node].as_mut()
    }

    pub fn iter_mut(&mut self, phi_id: PhiId) -> std::slice::IterMut<User> {
        self.0[phi_id].iter_mut()
    }
}
pub struct Phis(pub SlotMap<PhiId, Phi>);
impl Phis {
    pub fn new() -> Self {
        Phis(SlotMap::with_key())
    }

    pub fn insert(&mut self, phi: Phi) -> PhiId {
        self.0.insert(phi)
    }

    fn new_phi(&mut self) -> PhiId {
        self.0.insert(Phi)
    }

    pub fn remove(&mut self, phi_id: PhiId) -> Option<Phi> {
        self.0.remove(phi_id)
    }
}
pub struct PhisToBlock(SecondaryMap<PhiId, NodeIndex>);
impl PhisToBlock {
    pub fn new() -> Self {
        PhisToBlock(SecondaryMap::new())
    }
    pub fn insert(&mut self, phi: PhiId, node_index: NodeIndex) {
        self.0.insert(phi, node_index);
    }
}

impl std::ops::IndexMut<PhiId> for PhisToBlock {
    fn index_mut(&mut self, phi_node: PhiId) -> &mut NodeIndex {
        &mut self.0[phi_node]
    }
}

impl Index<PhiId> for PhisToBlock {
    type Output = NodeIndex;
    fn index(&self, phi_id: PhiId) -> &Self::Output {
        &self.0[phi_id]
    }
}

impl std::ops::IndexMut<PhiId> for Phis {
    fn index_mut(&mut self, phi_node: PhiId) -> &mut Phi {
        &mut self.0[phi_node]
    }
}

impl Index<PhiId> for Phis {
    type Output = Phi;
    fn index(&self, phi_id: PhiId) -> &Self::Output {
        &self.0[phi_id]
    }
}

impl std::ops::IndexMut<PhiId> for PhiUsers {
    fn index_mut(&mut self, phi_node: PhiId) -> &mut Vec<User> {
        self.0[phi_node].as_mut()
    }
}

impl Index<PhiId> for PhiUsers {
    type Output = Vec<User>;
    fn index(&self, phi_node: PhiId) -> &Self::Output {
        &self.0[phi_node]
    }
}

impl std::ops::IndexMut<NodeIndex> for VarDefs {
    fn index_mut(&mut self, node_index: NodeIndex) -> &mut HashMap<Symbol, PhiOrExpr> {
        self.0.get_mut(&node_index).unwrap()
    }
}

impl Index<NodeIndex> for VarDefs {
    type Output = HashMap<Symbol, PhiOrExpr>;
    fn index(&self, node_index: NodeIndex) -> &Self::Output {
        &self.0[&node_index]
    }
}

impl crate::SSABuilder {
    fn borrow_inc_phis_mut(incomplete_phis: &mut IncompletePhis) -> &mut IncompletePhis {
        incomplete_phis
    }
    pub fn borrow_inc_phis_mut_pub(&mut self) -> &mut IncompletePhis {
        Self::borrow_inc_phis_mut(&mut self.incomplete_phis)
    }
    fn borrow_var_defs_mut(var_defs: &mut VarDefs) -> &mut VarDefs {
        var_defs
    }
    pub fn borrow_var_defs_mut_pub(&mut self) -> &mut VarDefs {
        Self::borrow_var_defs_mut(&mut self.var_defs)
    }
    fn borrow_sealed_blocks_mut(sealed_blocks: &mut SealedBlocks) -> &mut SealedBlocks {
        sealed_blocks
    }
    pub fn borrow_sealed_blocks_mut_pub(&mut self) -> &mut SealedBlocks {
        Self::borrow_sealed_blocks_mut(&mut self.sealed_blocks)
    }
    fn borrow_phis_mut(phis: &mut Phis) -> &mut Phis {
        phis
    }
    pub fn borrow_phis_mut_pub(&mut self) -> &mut Phis {
        Self::borrow_phis_mut(&mut self.phis)
    }
    fn borrow_phis_to_block_mut(phis_to_block: &mut PhisToBlock) -> &mut PhisToBlock {
        phis_to_block
    }
    pub fn borrow_phis_to_block_mut_pub(&mut self) -> &mut PhisToBlock {
        Self::borrow_phis_to_block_mut(&mut self.phis_to_block)
    }
    fn borrow_phi_opnds_mut(phi_opnds: &mut PhiOperands) -> &mut PhiOperands {
        phi_opnds
    }

    pub fn borrow_phi_opnds_mut_pub(&mut self) -> &mut PhiOperands {
        Self::borrow_phi_opnds_mut(&mut self.phi_operands)
    }

    fn borrow_phi_users_mut(phi_users: &mut PhiUsers) -> &mut PhiUsers {
        phi_users
    }

    pub fn borrow_phi_users_mut_pub(&mut self) -> &mut PhiUsers {
        Self::borrow_phi_users_mut(&mut self.phi_users)
    }
}
