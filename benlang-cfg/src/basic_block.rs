use crate::ir::Ir;
use petgraph::graph::NodeIndex;

#[derive(Default, Debug)]
pub struct BasicBlock {
    pub statements: Vec<Ir>,
    pub terminator: Option<Ir>,
    pub node_index: NodeIndex,
}

impl BasicBlock {
    pub fn append_body(&mut self, ir_stmt: Ir) {
        self.statements.push(ir_stmt);
    }

    pub fn set_term(&mut self, terminator: Ir) {
        self.terminator = Some(terminator)
    }

    pub fn is_empty(&self) -> bool {
        self.statements.is_empty() && self.terminator.is_none()
    }
}
