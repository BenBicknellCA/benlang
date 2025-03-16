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

impl Stmt {
    pub fn is_conditional(&self) -> bool {
        matches!(self, Stmt::If(_) | Stmt::While(_))
    }

    pub fn is_term(&self) -> bool {
        matches!(
            self,
            Stmt::If(_) | Stmt::While(_) | Stmt::Return0 | Stmt::Return1(_) | Stmt::Block(_)
        )
    }
}

pub trait Conditional {
    fn cond(&self) -> ExprId;
    fn first_block(&self) -> &Block;
    fn second_block(&self) -> Option<&Block>;
    // could use bool if max 2 branches, u32 leave option for more open
    fn has_two_branches(&self) -> bool;
    fn is_while() -> bool;
}

impl Conditional for While {
    fn cond(&self) -> ExprId {
        self.cond
    }
    fn first_block(&self) -> &Block {
        &self.body
    }
    fn second_block(&self) -> Option<&Block> {
        None
    }
    fn has_two_branches(&self) -> bool {
        false
    }
    fn is_while() -> bool {
        true
    }
}

impl Conditional for If {
    fn cond(&self) -> ExprId {
        self.cond
    }
    fn first_block(&self) -> &Block {
        &self.then_
    }
    fn second_block(&self) -> Option<&Block> {
        self.else_.as_ref()
    }
    fn has_two_branches(&self) -> bool {
        if self.else_.is_some() {
            return true;
        };
        false
    }
    fn is_while() -> bool {
        false
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, Eq, PartialEq)]
pub struct While {
    cond: ExprId,
    body: Block,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct Block {
    pub body: Vec<StmtId>,
    pub leaders: Vec<usize>,
}
impl Block {
    pub fn new(body: Vec<StmtId>) -> Self {
        Block {
            body,
            leaders: Vec::new(),
        }
    }

    pub fn get_body_split_at_leaders(&self) -> Vec<&[StmtId]> {
        let len: usize = self.leaders.len();

        if self.leaders.is_empty() {
            return vec![&self.body];
        }

        let mut splits: Vec<&[StmtId]> = Vec::with_capacity(len * 2);

        for idx in &self.leaders {
            let (first, second): (&[StmtId], &[StmtId]) = self.body.split_at(*idx);
            splits.push(first);
            splits.push(second);
        }
        splits
    }

    pub fn add_stmt_id(&mut self, stmt_id: StmtId) {
        self.body.push(stmt_id);
    }

    pub fn get_body_ref(&self) -> &[StmtId] {
        self.body.as_slice()
    }

    pub fn is_empty(&self) -> bool {
        self.body.is_empty()
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
