use crate::expr::Expr;
use crate::expr::{Assign, BinaryOp, Call, Unary, UnaryOp, Variable};
use crate::scanner::Symbol;
use crate::scanner::Token;
use crate::value::{Literal, Value};
//do not remove
use crate::Precedence;
use crate::{ParseError, Parser};
use anyhow::Result;
use slotmap::new_key_type;

new_key_type! {pub struct ExprId;}

// temp, so clippy does not remove Expr import needed for enum_dispatch to function in this file
type ExprPlaceholder = Expr;

impl Parser {
    pub fn parse_expression(&mut self, prec: Precedence) -> Result<ExprId> {
        let mut token = self.advance()?;
        let can_assign = prec <= Precedence::Assignment;
        let mut left: ExprId = self.prefix(token, can_assign)?;

        while prec < self.current_prec() {
            token = self.advance()?;
            left = self.infix(token, left, can_assign)?;
        }
        Ok(left)
    }

    pub fn expression(&mut self) -> Result<ExprId> {
        self.parse_expression(Precedence::Assignment)
    }

    fn unary(&mut self, can_assign: bool) -> Result<ExprId> {
        let op: UnaryOp = UnaryOp::try_from(self.iter.prev)?;
        let expr = self.parse_expression(Precedence::Unary)?;
        self.insert_expr(Unary::new(op, expr).into())
    }

    fn string_lit(&mut self, string_key: Symbol, can_assign: bool) -> Result<ExprId> {
        self.insert_expr(Value::Literal(Literal::String(string_key)).into())
    }

    fn number(&mut self, num: u32, can_assign: bool) -> Result<ExprId> {
        self.insert_expr(Value::Literal(Literal::Number(num)).into())
    }

    fn float(&mut self, flt: f32, can_assign: bool) -> Result<ExprId> {
        self.insert_expr(Value::Literal(Literal::Float(flt)).into())
    }

    fn primary(&mut self, token: Token, can_assign: bool) -> Result<ExprId> {
        match token {
            Token::Number(num) => self.number(num, can_assign),
            Token::StringLiteral(strng) => self.string_lit(strng, can_assign),
            Token::True => {
                Ok(self.insert_expr_in_current_func(Value::Literal(Literal::Bool(true)).into()))
            }
            Token::False => {
                Ok(self.insert_expr_in_current_func(Value::Literal(Literal::Bool(false)).into()))
            }
            Token::Float(flt) => self.float(flt, can_assign),
            _ => Err(ParseError::InvalidPrimary { primary: token }.into()),
        }
    }
    fn grouping(&mut self) -> anyhow::Result<ExprId> {
        let expr = self.parse_expression(Precedence::Assignment)?;
        self.consume(Token::RightParen)?;
        Ok(expr)
    }
    fn variable(&mut self, iden: Symbol, can_assign: bool) -> Result<ExprId> {
        if self.consume(Token::Equal).is_ok_and(|_| can_assign) {
            let expr = self.expression()?;
            return self.insert_expr(Assign::new(iden, expr).into());
        }
        self.insert_expr(Variable(iden).into())
    }

    fn prefix(&mut self, token: Token, can_assign: bool) -> Result<ExprId> {
        match token {
            Token::LeftParen => self.grouping(),
            Token::Minus | Token::Bang => self.unary(can_assign),
            Token::Identifier(iden) => self.variable(iden, can_assign),
            Token::Number(_) | Token::True | Token::False | Token::StringLiteral(_) => {
                self.primary(token, can_assign)
            }
            _ => Err(ParseError::InvalidPrefix { prefix: token }.into()),
        }
    }

    fn infix(&mut self, token: Token, left: ExprId, can_assign: bool) -> Result<ExprId> {
        match token {
            Token::Plus
            | Token::And
            | Token::Or
            | Token::Minus
            | Token::Slash
            | Token::Star
            | Token::EqualEqual
            | Token::LessEqual
            | Token::GreaterEqual
            | Token::Greater
            | Token::Mod
            | Token::Less => self.binary(left, can_assign),
            Token::LeftParen => self.call(left),
            _ => Err(ParseError::InvalidInfix { infix: token }.into()),
        }
    }

    fn call(&mut self, token: ExprId) -> Result<ExprId> {
        let mut args = Vec::new();
        let mut arg_count = 0;

        if self.check(Token::RightParen).is_err() {
            args.push(self.expression()?);
            arg_count += 1;
            while self.check(Token::RightParen).is_err() {
                self.consume(Token::Comma)?;
                args.push(self.expression()?);
                arg_count += 1;
            }
        }
        let name = if let Some(Expr::Variable(iden)) =
            self.func_data.expr_pools[self.current_func()].get(token)
        {
            Some(iden.0)
        } else {
            None
        };
        self.consume(Token::RightParen)?;

        //        Ok(self.insert_expr_in_current_func(Call::new(name, args, arg_count).into()))
        self.insert_expr(Call::new(name, args, arg_count).into())
    }
    fn binary(&mut self, pre_lhs: ExprId, can_assign: bool) -> Result<ExprId> {
        let op: Token = self.iter.prev;

        let prec = Precedence::try_from(self.iter.prev_prec() as u32 + 1)?;
        let rhs = self.parse_expression(prec)?;

        let bin_op = BinaryOp::try_from(op)?;
        Ok(self.insert_expr_in_current_func(crate::expr::Binary::new(pre_lhs, bin_op, rhs).into()))
    }
}
