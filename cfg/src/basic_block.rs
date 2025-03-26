use crate::ir::HIR;
use anyhow::{Result, anyhow};
use petgraph::graph::NodeIndex;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TermKind {
    If,
    While,
    Return0,
    Return1,
    // Goto,
}

#[derive(Default, Debug)]
pub struct BasicBlock {
    pub statements: Vec<HIR>,
    pub node_index: NodeIndex,
    pub term_kind: Option<TermKind>,
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

    pub fn borrow_stmts(&self) -> &[HIR] {
        &self.statements
    }

    pub fn set_term_kind(&mut self, term_kind: TermKind) {
        self.term_kind = Some(term_kind);
    }

    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }
}
