use crate::CFGBuilder;
use crate::ssa::PhiId;
use crate::ssa::PhiOrExpr;
use petgraph::Graph;
use petgraph::algo::tarjan_scc;
use petgraph::graph::NodeIndex;
use std::collections::{HashMap, HashSet};

impl CFGBuilder {
    pub fn remove_redundant_phis(&mut self, phi_functions: &HashSet<PhiId>) {
        let mut blocks_to_phi: HashMap<NodeIndex, Vec<PhiId>> = HashMap::new();
        let blocks: Vec<NodeIndex> = phi_functions
            .iter()
            .map(|phi| {
                let block = self.func_to_ssa[self.current_func].phis_to_block[*phi];
                let vec: &mut Vec<PhiId> = if let Some(phi_vec) = blocks_to_phi.get_mut(&block) {
                    phi_vec
                } else {
                    blocks_to_phi.insert(block, Vec::new());
                    blocks_to_phi.get_mut(&block).unwrap()
                };
                vec.push(*phi);
                block
            })
            .collect();
        let subgraph = self.induced_subgraph(&blocks);
        let mut sscs = tarjan_scc(&subgraph);
        sscs.reverse();
        for scc in sscs {
            self.process_scc(&scc, &blocks_to_phi);
        }
    }

    pub fn replace_scc_by_value(
        &mut self,
        scc: &[NodeIndex],
        blocks_to_phi: &HashMap<NodeIndex, Vec<PhiId>>,
        op: PhiOrExpr,
    ) {
        for component in scc {
            let phis_in_component = &blocks_to_phi[component];
            for phi in phis_in_component {
                self.func_to_ssa[self.current_func].phi_operands[*phi] = vec![op];
            }
        }
    }

    pub fn process_scc(
        &mut self,
        scc: &[NodeIndex],
        blocks_to_phi: &HashMap<NodeIndex, Vec<PhiId>>,
    ) {
        if scc.len() == 1 {
            return;
        }
        let mut inner: HashSet<PhiId> = HashSet::new();
        let mut outer_ops = Vec::new();
        for (node, phi_vec) in blocks_to_phi {
            for phi in phi_vec {
                let mut is_inner = true;
                for opnd in &self.func_to_ssa[self.current_func].phi_operands[*phi] {
                    if !scc.contains(node) {
                        outer_ops.push(opnd);
                        is_inner = false;
                    }
                }
                if is_inner {
                    inner.insert(*phi);
                }
            }
        }
        let outer_ops_len = outer_ops.len();
        if outer_ops_len == 1 {
            self.replace_scc_by_value(scc, blocks_to_phi, *outer_ops.pop().unwrap());
            //replace scc by value
        } else if outer_ops_len > 2 {
            self.remove_redundant_phis(&inner);
        }
    }

    //}

    //impl CFGBuilder {
    pub fn induced_subgraph(
        &self,
        node_map: &[NodeIndex],
    ) -> Graph<NodeIndex, petgraph::graph::EdgeIndex> {
        let mut edges = HashSet::new();

        for node in node_map {
            let mut neighbours = self.func_to_cfg[self.current_func]
                .neighbors_undirected(*node)
                .detach();
            while let Some(edge) = neighbours.next_edge(&self.func_to_cfg[self.current_func]) {
                edges.insert(edge);
            }
        }

        self.func_to_cfg[self.current_func].filter_map(
            |node_index, weight| {
                if node_map.contains(&node_index) {
                    Some(node_index)
                } else {
                    None
                }
            },
            |edge_index, weight| {
                if edges.contains(&edge_index) {
                    Some(edge_index)
                } else {
                    None
                }
            },
        )
    }
}
