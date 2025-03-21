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

impl Expr {
    pub fn get_value(&self) -> Result<&Value, ()> {
        if let Expr::Value(val) = self {
            return Ok(val);
        }
        Err(())
    }

    pub fn get_binary(&self) -> Result<Binary, ()> {
        if let Expr::Binary(binary) = self {
            return Ok(*binary);
        }
        Err(())
    }
    pub fn get_unary(&self) -> Result<Unary, ()> {
        if let Expr::Unary(unary) = self {
            return Ok(*unary);
        }
        Err(())
    }

    pub fn can_concat(&self, other: &Expr) -> bool {
        if let Expr::Value(Value::StringLiteral(_)) = self {
            return std::mem::discriminant(self) == std::mem::discriminant(other);
        };
        false
    }
}

#[enum_dispatch]
trait ToExpr {}

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy)]
pub struct Binary {
    pub lhs: ExprId,
    pub op: BinaryOp,
    pub rhs: ExprId,
}

impl Binary {
    pub fn new(lhs: ExprId, op: BinaryOp, rhs: ExprId) -> Self {
        Self { lhs, op, rhs }
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy)]
pub struct Unary {
    pub op: UnaryOp,
    pub opnd: ExprId,
}
impl Unary {
    pub fn new(op: UnaryOp, opnd: ExprId) -> Self {
        Self { op, opnd }
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy)]
pub struct Assign {
    pub name: Symbol,
    pub val: ExprId,
}
impl Assign {
    pub fn new(name: Symbol, val: ExprId) -> Self {
        Self { name, val }
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy)]
pub struct Identifier(pub Symbol);

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy)]
pub struct Variable(pub Symbol);

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy)]
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
