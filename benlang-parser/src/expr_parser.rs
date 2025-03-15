use crate::Precedence;
use crate::expr::{BinaryOp, Unary, UnaryOp, Value, Variable};
use crate::scanner::Symbol;
use crate::scanner::Token;
use crate::{ParseError, Parser};
use anyhow::Result;
use slotmap::new_key_type;

new_key_type! {pub struct ExprId;}

impl Parser {
    fn parse_expression(&mut self, prec: Precedence) -> Result<ExprId> {
        let mut token = self.advance()?;
        let mut left: ExprId = self.prefix(token)?;

        while prec < self.current_prec() {
            token = self.advance()?;
            left = self.infix(token, left)?;
        }
        Ok(left)
    }

    pub fn expression(&mut self) -> Result<ExprId> {
        self.parse_expression(Precedence::Assignment)
    }

    fn unary(&mut self) -> Result<ExprId> {
        let op: UnaryOp = UnaryOp::try_from(self.iter.prev)?;
        let expr = self.parse_expression(Precedence::Unary)?;
        self.insert_expr(Unary(op, expr).into())
    }

    fn string_lit(&mut self, string_key: Symbol) -> Result<ExprId> {
        self.insert_expr(Value::StringLiteral(string_key).into())
    }

    fn number(&mut self, num: f32) -> Result<ExprId> {
        self.insert_expr(Value::Number(num).into())
    }

    fn primary(&mut self, token: Token) -> Result<ExprId> {
        match token {
            Token::Number(num) => self.number(num),
            Token::StringLiteral(strng) => self.string_lit(strng),
            Token::True => Ok(self.expr_pool.insert(Value::Bool(true).into())),
            Token::False => Ok(self.expr_pool.insert(Value::Bool(false).into())),
            _ => Err(ParseError::InvalidPrimary { primary: token }.into()),
        }
    }
    fn grouping(&mut self) -> anyhow::Result<ExprId> {
        let expr = self.parse_expression(Precedence::Assignment)?;
        self.consume(Token::RightParen)?;
        Ok(expr)
    }
    fn variable(&mut self, iden: Symbol) -> Result<ExprId> {
        // TODO
        self.insert_expr(Variable(iden).into())
    }

    fn prefix(&mut self, token: Token) -> Result<ExprId> {
        match token {
            Token::LeftParen => self.grouping(),
            Token::Minus | Token::Bang => self.unary(),
            Token::Identifier(iden) => self.variable(iden),
            Token::Number(_) | Token::True | Token::False | Token::StringLiteral(_) => {
                self.primary(token)
            }
            _ => Err(ParseError::InvalidPrefix { prefix: token }.into()),
        }
    }

    fn infix(&mut self, token: Token, left: ExprId) -> Result<ExprId> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Star
            | Token::EqualEqual
            | Token::LessEqual
            | Token::GreaterEqual
            | Token::Greater
            | Token::Less => self.binary(left),
            _ => Err(ParseError::InvalidInfix { infix: token }.into()),
        }
    }

    fn binary(&mut self, pre_lhs: ExprId) -> Result<ExprId> {
        let op: Token = self.iter.prev;

        let prec = Precedence::try_from(self.iter.prev_prec() as u32 + 1)?;
        let rhs = self.parse_expression(prec)?;

        let bin_op = BinaryOp::try_from(op)?;
        Ok(self
            .expr_pool
            .insert(crate::expr::Binary(pre_lhs, bin_op, rhs).into()))
    }
}
