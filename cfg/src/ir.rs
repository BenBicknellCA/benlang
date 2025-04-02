use anyhow::{Result, anyhow};
use parser::ExprPool;
use parser::expr::Assign;
use parser::expr::Expr;
use parser::expr_parser::ExprId;
use parser::scanner::Symbol;
use parser::stmt::Stmt;
use parser::value::Value;
use petgraph::graph::NodeIndex;
use slotmap::new_key_type;

new_key_type! {pub struct ConstId;}

#[derive(Debug, Clone, Copy)]
pub enum HIR {
    Expr(ExprId),
    Return0,
    Return1(ExprId),
    Print(ExprId),
    Assign(Assign),
    Jmp(NodeIndex),
}

impl HIR {
    //    pub fn replace_expr(self, new_expr: ExprId) -> Result<HIR> {
    //        let val = match self {
    //            HIR::Binary()
    //            HIR::Return1(_) => HIR::Return1(new_expr),
    //            HIR::Print(_) => HIR::Print(new_expr),
    //            HIR::Var(assign) => HIR::Var(Assign::new(assign.name, new_expr)),
    //            HIR::Assign(assign) => HIR::Assign(Assign::new(assign.name, new_expr)),
    //
    //            _ => {
    //                return Err(anyhow!("cannot swap expr from {:?}", self));
    //            }
    //        };
    //        Ok(val)
    //    }
    pub fn get_name_val(stmt: &Stmt, expr_pool: &crate::ExprPool) -> Option<(Symbol, ExprId)> {
        match stmt {
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
            HIR::Return1(expr) => expr,
            HIR::Print(expr) => expr,
            HIR::Assign(assign) => &assign.val,
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
