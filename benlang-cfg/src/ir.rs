use benlang_parser::expr_parser::ExprId;
use benlang_parser::object::Function;
use benlang_parser::scanner::Symbol;
use benlang_parser::stmt::Stmt;
use petgraph::stable_graph::NodeIndex;

#[derive(Debug)]
pub enum Ir {
    Jump(NodeIndex),
    Expr(ExprId),
    Return0,
    Return1(ExprId),
    Print(ExprId),
    Var(Symbol, ExprId),
    DeclareFunc(Function),
}

impl TryFrom<&Stmt> for Ir {
    type Error = ();
    fn try_from(stmt: &Stmt) -> Result<Self, ()> {
        let val = match stmt {
            Stmt::Expr(expr) => Ir::Expr(*expr),
            Stmt::Print(val) => Ir::Print(*val),
            Stmt::Var(name, val) => Ir::Var(*name, *val),
            _ => return Err(()),
        };
        Ok(val)
    }
}
