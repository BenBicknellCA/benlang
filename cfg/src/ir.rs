use anyhow::{Result, anyhow};
use parser::ExprPool;
use parser::expr::Expr;
use parser::value::Value;
use parser::expr_parser::ExprId;
use parser::object::Function;
use parser::scanner::Symbol;
use parser::stmt::Stmt;

#[derive(Debug)]
pub enum HIR {
    Expr(ExprId),
    Return0,
    Return1(ExprId),
    Print(ExprId),
    Var(Symbol, ExprId),
    DeclareFunc(Function),
}

impl HIR {
    pub fn replace_expr(self, new_expr: ExprId) -> Result<HIR> {
        let val = match self {
            HIR::Expr(_) => HIR::Expr(new_expr),
            HIR::Return1(_) => HIR::Return1(new_expr),
            HIR::Print(_) => HIR::Print(new_expr),
            HIR::Var(name, val) => HIR::Var(name, new_expr),
            _ => {
                return Err(anyhow!("cannot swap expr from {:?}", self));
            }
        };
        Ok(val)
    }
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
    pub fn get_expr(&self) -> Result<ExprId> {
        let id = match self {
            HIR::Expr(expr) => expr,
            HIR::Return1(expr) => expr,
            HIR::Print(expr) => expr,
            HIR::Var(_, expr) => expr,
            _ => return Err(anyhow!("cannot get expr from {:?}", self)),
        };
        Ok(*id)
    }

    pub fn get_value<'a>(&self, expr_pool: &'a ExprPool) -> Result<&'a Value> {
        let expr = self.get_expr()?;
        if let Expr::Value(val) = &expr_pool[expr] {
            return Ok(val);
        }
        Err(anyhow!("cannot get value from {:?}", self))
    }
}

impl TryFrom<&Stmt> for HIR {
    type Error = ();
    fn try_from(stmt: &Stmt) -> Result<Self, ()> {
        let val = match stmt {
            Stmt::Expr(expr) => HIR::Expr(*expr),
            Stmt::Print(val) => HIR::Print(*val),
            Stmt::Var(name, val) => HIR::Var(*name, *val),
            _ => return Err(()),
        };
        Ok(val)
    }
}
