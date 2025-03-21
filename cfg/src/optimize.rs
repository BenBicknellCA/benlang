use crate::Expr;
use crate::ExprPool;
use crate::{CFG, CFGBuilder};
use anyhow::{Error, Result, anyhow};
use parser::expr::{Binary, BinaryOp, Unary, UnaryOp, Value};
use parser::expr_parser::ExprId;

impl CFGBuilder {
    pub fn fold_constant<'a>(expr_pool: &mut ExprPool, expr_id: ExprId) -> Result<ExprId, ()> {
        let expr = &expr_pool[expr_id];
        match expr {
            Expr::Binary(_) => CFGBuilder::fold_binary(expr_pool, expr_id),
            Expr::Unary(_) => CFGBuilder::fold_unary(expr_pool, expr_id),
            Expr::Value(_) => Ok(expr_id),
            _ => {
                return Err(());
            }
        }
    }

    pub fn fold_binary<'a>(expr_pool: &mut ExprPool, binary: ExprId) -> Result<ExprId, ()> {
        let binary = expr_pool.get(binary).unwrap().get_binary()?;

        let folded_lhs = CFGBuilder::fold_constant(expr_pool, binary.lhs)?;
        let folded_rhs = CFGBuilder::fold_constant(expr_pool, binary.rhs)?;
        if let Expr::Value(lhs) = &expr_pool[folded_lhs] {
            if let Expr::Value(rhs) = &expr_pool[folded_rhs] {
                if !lhs.same_variant(&rhs) {
                    return Err(());
                }
                if lhs.is_string_lit() && binary.op != BinaryOp::Plus {
                    return Err(());
                }
            }
        }

        let lhs = expr_pool[folded_lhs].get_value()?;
        let rhs = expr_pool[folded_rhs].get_value()?;

        let folded = match binary.op {
            BinaryOp::Plus => lhs + rhs,
            BinaryOp::Minus => lhs - rhs,
            BinaryOp::Slash => lhs / rhs,
            BinaryOp::Star => lhs * rhs,
            _ => return Err(()),
        };

        Ok(expr_pool.insert(Expr::Value(folded)))
    }

    pub fn fold_unary<'a>(expr_pool: &mut ExprPool, un: ExprId) -> Result<ExprId, ()> {
        let un = expr_pool.get(un).unwrap().get_unary()?;
        let opnd = CFGBuilder::fold_constant(expr_pool, un.opnd)?;
        let folded_opnd = expr_pool[opnd].get_value()?;
        match un.op {
            UnaryOp::Bang => {
                return Ok(expr_pool.insert(Expr::Value(!folded_opnd)));
            }
            UnaryOp::Minus => {
                return Ok(expr_pool.insert(Expr::Value(-folded_opnd)));
            }
        };
    }
}
