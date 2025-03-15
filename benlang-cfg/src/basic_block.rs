use crate::ir::Ir;
use benlang_parser::stmt::Stmt;

#[derive(Default, Debug)]
pub struct BasicBlock {
    pub statements: Vec<Ir>,
    pub terminator: Option<Ir>,
}

impl BasicBlock {
    pub fn is_stmt_term(stmt: &Stmt) -> bool {
        matches!(
            stmt,
            Stmt::If(_) | Stmt::While(_) | Stmt::Return0 | Stmt::Return1(_) | Stmt::Block(_)
        )
    }
}
