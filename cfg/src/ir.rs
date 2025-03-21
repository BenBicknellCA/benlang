use parser::expr::Expr;
use parser::expr_parser::ExprId;
use parser::object::Function;
use parser::scanner::Symbol;
use parser::stmt::Stmt;

#[derive(Debug)]
pub enum Ir {
    Expr(ExprId),
    Return0,
    Return1(ExprId),
    Print(ExprId),
    Var(Symbol, ExprId),
    DeclareFunc(Function),
}

impl Ir {
    pub fn get_name_val(stmt: &Stmt, expr_pool: &crate::ExprPool) -> Option<(Symbol, ExprId)> {
        match stmt {
            Stmt::Var(name, val) => Some((*name, *val)),
            Stmt::Expr(expr) => {
                if let Expr::Assign(assgn) = &expr_pool[*expr] {
                    return Some((assgn.name, assgn.val));
                };
                None
            }
            _ => None,
        }
    }
    pub fn get_expr(&self) -> Option<ExprId> {
        let id = match self {
            Ir::Expr(expr) => expr,
            Ir::Return1(expr) => expr,
            Ir::Print(expr) => expr,
            Ir::Var(_, expr) => expr,
            _ => return None,
        };
        Some(*id)
    }
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
