use crate::Object;
use crate::ParseError;
use crate::Stmt;
use crate::expr_parser::ExprId;
use crate::scanner::{Symbol, Token};
use anyhow::{Error, Result, anyhow};
use enum_dispatch::enum_dispatch;

#[derive(Debug, PartialEq, Clone, PartialOrd, Hash, Eq, Copy)]
pub enum BinaryPrec {
    Factor,
    Term,
    Comparison,
    Logical,
    Equality,
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Hash, Eq, Copy)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterEqual,
    LessThan,
    LessEqual,
}

impl TryFrom<Token> for UnaryOp {
    type Error = Error;
    fn try_from(token: Token) -> Result<UnaryOp, Error> {
        let op = match token {
            Token::Minus => UnaryOp::Minus,
            Token::Bang => UnaryOp::Bang,
            _ => return Err(anyhow!("cannot get unary op from {token}")),
        };
        Ok(op)
    }
}

impl TryFrom<Token> for BinaryOp {
    type Error = Error;
    fn try_from(token: Token) -> Result<BinaryOp, Error> {
        let op = match token {
            Token::Plus => BinaryOp::Plus,
            Token::Minus => BinaryOp::Minus,
            Token::Star => BinaryOp::Star,
            Token::Slash => BinaryOp::Slash,
            Token::EqualEqual => BinaryOp::Equal,
            Token::BangEqual => BinaryOp::NotEqual,
            Token::GreaterEqual => BinaryOp::GreaterEqual,
            Token::LessEqual => BinaryOp::LessEqual,
            Token::Greater => BinaryOp::GreaterThan,
            Token::Less => BinaryOp::LessThan,

            _ => return Err(ParseError::InvalidOp { op: token }.into()),
        };
        Ok(op)
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Hash, Eq, Copy)]
pub enum UnaryOp {
    Bang,
    Minus,
}

//    Binary(ExprKey, BinaryOp, ExprKey),
//    Unary(UnaryOp, ExprKey),
//    Value(Value),
//    //    Literal(Literal),
//    Identifier(Symbol),
//    Assign(Token, ExprKey),
//    Stmt(StmtKey),
//    Variable(Symbol),
//    Call(Call),
//    Grouping,

#[enum_dispatch(ToExpr)]
#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Expr {
    Binary,
    Unary,
    Value,
    //    Literal(Literal),
    Identifier,
    Assign,
    Stmt,
    Variable,
    Call,
    Grouping,
}

#[enum_dispatch]
trait ToExpr {}

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub struct Binary(pub ExprId, pub BinaryOp, pub ExprId);

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub struct Unary(pub UnaryOp, pub ExprId);

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub struct Assign(pub Symbol, pub ExprId);

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub struct Identifier(pub Symbol);

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub struct Variable(pub Symbol);

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub struct Grouping(pub Symbol);

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub struct Call {
    callee: ExprId,
    paren: Token,
    args: Vec<ExprId>,
}

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Value {
    Object(Object),
    Number(f32),
    StringLiteral(Symbol),
    Bool(bool),
    Nil,
}
