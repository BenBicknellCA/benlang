use crate::expr_parser::ExprId;
use crate::object::Function;
use crate::scanner::Symbol;
use crate::stmt_parser::StmtId;

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum Stmt {
    If(If),
    While(While),
    Expr(ExprId),
    Return0,
    Return1(ExprId),
    Var(Symbol, ExprId),
    Print(ExprId),
    Function(Function),
    Block(Block),
}

#[derive(Debug, Clone, PartialOrd, Ord, Eq, PartialEq)]
pub struct While {
    cond: ExprId,
    body: Block,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct Block(Vec<StmtId>);
impl Block {
    pub fn new(stmts: Vec<StmtId>) -> Self {
        Block(stmts)
    }
}

impl While {
    pub fn new(cond: ExprId, body: Block) -> Self {
        While { cond, body }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, Eq, PartialEq)]
pub struct If {
    pub cond: ExprId,
    pub then_: Block,
    pub else_: Option<Block>,
}

impl If {
    pub fn new(cond: ExprId, then_: Block, else_: Option<Block>) -> Self {
        If { cond, then_, else_ }
    }
}
