use crate::CFGBuilder;
use crate::Expr;
use crate::ExprPool;
use anyhow::{Result, anyhow};
use parser::expr::{BinaryOp, UnaryOp};
use parser::expr_parser::ExprId;
use parser::value::Value;

impl CFGBuilder {
    pub fn fold_constant<'a>(expr_pool: &mut ExprPool, expr_id: ExprId) -> Result<ExprId> {
        let expr = &expr_pool[expr_id];
        match expr {
            Expr::Binary(_) => CFGBuilder::fold_binary(expr_pool, expr_id),
            Expr::Unary(_) => CFGBuilder::fold_unary(expr_pool, expr_id),
            Expr::Value(_) => Ok(expr_id),
            _ => Err(anyhow!("cannot fold {:?}", expr)),
        }
    }

    pub fn fold_binary<'a>(expr_pool: &mut ExprPool, binary: ExprId) -> Result<ExprId> {
        let binary = expr_pool.get(binary).unwrap().get_binary()?;

        let folded_lhs = CFGBuilder::fold_constant(expr_pool, binary.lhs)?;
        let folded_rhs = CFGBuilder::fold_constant(expr_pool, binary.rhs)?;

        let lhs = expr_pool[folded_lhs].get_value()?.get_literal()?;
        let rhs = expr_pool[folded_rhs].get_value()?.get_literal()?;

        if !lhs.same_discriminant(&rhs) {
            return Err(anyhow!("cannot fold values of different discriminant"));
        }

        if lhs.is_string() && binary.op != BinaryOp::Plus {
            return Err(anyhow!(
                "can't perform `{:?}` on Literal::Strings",
                binary.op
            ));
        }

        let folded = match binary.op {
            BinaryOp::Plus => lhs + rhs,
            BinaryOp::Minus => lhs - rhs,
            BinaryOp::Slash => lhs / rhs,
            BinaryOp::Star => lhs * rhs,
            _ => {
                return Err(anyhow!(
                    "cannot perform `{:?}` on {:?} and {:?}",
                    binary.op,
                    lhs,
                    rhs
                ));
            }
        };

        Ok(expr_pool.insert(Expr::Value(Value::Literal(folded))))
    }

    pub fn fold_unary<'a>(expr_pool: &mut ExprPool, un: ExprId) -> Result<ExprId> {
        let un = expr_pool.get(un).unwrap().get_unary()?;
        let opnd = CFGBuilder::fold_constant(expr_pool, un.opnd)?;
        let folded_opnd = expr_pool[opnd].get_value()?.get_literal()?;
        match un.op {
            UnaryOp::Bang => Ok(expr_pool.insert(Expr::Value(Value::Literal(!folded_opnd)))),
            UnaryOp::Minus => Ok(expr_pool.insert(Expr::Value(Value::Literal(-folded_opnd)))),
        }
    }
}
