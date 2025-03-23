use crate::CFGBuilder;
use crate::Expr;
use crate::ExprPool;
use anyhow::{Result, anyhow};
use parser::expr::{Assign, Binary, BinaryOp, UnaryOp};
use parser::expr_parser::ExprId;
use parser::value::{Literal, Value};

impl CFGBuilder {
    pub fn fold_constant<'a>(expr_pool: &mut ExprPool, expr_id: ExprId) -> Result<()> {
        match expr_pool[expr_id] {
            Expr::Binary(_) => CFGBuilder::fold_binary(expr_pool, expr_id),
            Expr::Unary(_) => CFGBuilder::fold_unary(expr_pool, expr_id),
            Expr::Value(_) => Ok(()),
            Expr::Variable(_) => Ok(()),
            Expr::Assign(assign) => {
                CFGBuilder::fold_constant(expr_pool, assign.val)?;
                let new_assign = Expr::Assign(Assign::new(assign.name, assign.val));
                expr_pool[expr_id] = new_assign;
                Ok(())
            }
            _ => Ok(()),
        }
    }
    pub fn is_lhs_or_rhs_variable(expr_pool: &ExprPool, lhs: ExprId, rhs: ExprId) -> bool {
        expr_pool[lhs].is_variable() || expr_pool[rhs].is_variable()
    }

    pub fn fold_binary(expr_pool: &mut ExprPool, binary_id: ExprId) -> Result<()> {
        let binary = expr_pool.get(binary_id).unwrap().get_binary()?;

        CFGBuilder::fold_constant(expr_pool, binary.lhs)?;
        CFGBuilder::fold_constant(expr_pool, binary.rhs)?;

        let folded_rhs = binary.rhs;
        let folded_lhs = binary.lhs;

        if CFGBuilder::is_lhs_or_rhs_variable(expr_pool, folded_lhs, folded_rhs) {
            expr_pool[binary_id] = Binary::new(folded_lhs, binary.op, folded_rhs).into();
        }

        if let Expr::Value(lhs) = &expr_pool[folded_lhs] {
            if let Expr::Value(rhs) = &expr_pool[folded_rhs] {
                if !lhs.same_variant(rhs) {
                    return Err(anyhow!("cannot fold different variants"));
                }
                if lhs.is_string_lit() && binary.op != BinaryOp::Plus {
                    return Err(anyhow!("strings can only be concat"));
                }
            }
        }

        let lhs_expr = &expr_pool[folded_lhs];
        let rhs_expr = &expr_pool[folded_rhs];

        if lhs_expr.is_variable() || rhs_expr.is_variable() {
            expr_pool[binary_id] = Binary::new(folded_lhs, binary.op, folded_rhs).into();
            return Ok(());
        }

        let lhs = lhs_expr.get_value()?.get_literal()?;
        let rhs = rhs_expr.get_value()?.get_literal()?;

        let folded = match binary.op {
            BinaryOp::Plus => lhs + rhs,
            BinaryOp::Minus => lhs - rhs,
            BinaryOp::Slash => lhs / rhs,
            BinaryOp::Star => lhs * rhs,

            BinaryOp::And | BinaryOp::Or if lhs.get_bool()? && rhs.get_bool()? => {
                Literal::Bool(lhs.fold_and_or(binary.op, &rhs)?)
            }

            _ => return Err(anyhow!("cannot fold {lhs:?} and {rhs:?}")),
        };
        expr_pool[binary_id] = Expr::Value(Value::Literal(folded));
        Ok(())
    }

    pub fn fold_unary(expr_pool: &mut ExprPool, un: ExprId) -> Result<()> {
        let unary = expr_pool.get(un).unwrap().get_unary()?;
        CFGBuilder::fold_constant(expr_pool, unary.opnd)?;
        let folded_expr = &expr_pool[un];

        if folded_expr.is_variable() {
            return Ok(());
        }

        let folded_opnd = folded_expr.get_value()?.get_literal()?;
        let val: Literal = match unary.op {
            UnaryOp::Bang => {
                assert!(folded_opnd.is_bool());
                !folded_opnd
            }

            UnaryOp::Minus => {
                assert!(folded_opnd.is_number());
                -folded_opnd
            }
        };
        expr_pool[un] = Expr::Value(Value::Literal(val));
        Ok(())
    }
}
