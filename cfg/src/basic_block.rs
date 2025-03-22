use crate::ir::HIR;
use anyhow::{Result, anyhow};
use petgraph::graph::NodeIndex;

#[derive(Default, Debug)]
pub struct BasicBlock {
    statements: Vec<HIR>,
    pub node_index: NodeIndex,
}

impl BasicBlock {
    pub fn append_body(&mut self, ir_stmt: HIR) {
        self.statements.push(ir_stmt);
    }

    pub fn get_last_hir(&self) -> Result<&HIR> {
        if let Some(hir) = self.statements.last() {
            return Ok(hir);
        }
        Err(anyhow!(
            "could not get last element in basicblock {:?}",
            self.node_index
        ))
    }

    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }
}
