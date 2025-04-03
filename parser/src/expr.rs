use crate::ParseError;
use crate::Stmt;
use crate::expr_parser::ExprId;

use crate::scanner::{Symbol, Token};
use crate::value::{Literal, Value};

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
    Or,
    And,
    Mod,
}

impl TryFrom<Token> for UnaryOp {
    type Error = Error;
    fn try_from(token: Token) -> Result<UnaryOp> {
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
    fn try_from(token: Token) -> Result<BinaryOp> {
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
            Token::And => BinaryOp::And,
            Token::Or => BinaryOp::Or,
            Token::Mod => BinaryOp::Mod,

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
    pub const fn is_variable(&self) -> bool {
        matches!(self, Expr::Variable(_))
    }

    pub fn is_cmp(&self) -> bool {
        if let Expr::Binary(bin) = self {
            return bin.is_cmp();
        };
        false
    }

    pub fn get_bool(&self) -> Result<bool> {
        if let Expr::Value(Value::Literal(Literal::Bool(boolval))) = self {
            return Ok(*boolval);
        }
        Err(anyhow!("cannot get bool from {:?}:", self))
    }

    pub const fn is_bool(&self) -> bool {
        matches!(self, Expr::Value(Value::Literal(Literal::Bool(_))))
    }
    pub fn can_fold(&self, expr_pool: &crate::ExprPool) -> bool {
        match self {
            Expr::Unary(un) => expr_pool[un.opnd].can_fold(expr_pool),
            Expr::Binary(bin) => {
                expr_pool[bin.lhs].can_fold(expr_pool) && expr_pool[bin.lhs].can_fold(expr_pool)
            }
            Expr::Value(_) => false,
            Expr::Variable(_) => false,
            Expr::Assign(assign) => expr_pool[assign.val].can_fold(expr_pool),
            _ => false,
        }
    }
    pub fn get_value(&self) -> Result<&Value> {
        if let Expr::Value(val) = self {
            return Ok(val);
        }
        Err(anyhow!("cannot get value from {:?}", self))
    }

    pub fn get_binary(&self) -> Result<Binary> {
        if let Expr::Binary(binary) = self {
            return Ok(*binary);
        }
        Err(anyhow!("cannot get binary from {:?}", self))
    }
    pub fn get_unary(&self) -> Result<Unary> {
        if let Expr::Unary(unary) = self {
            return Ok(*unary);
        }
        Err(anyhow!("cannot get unary from {:?}", self))
    }

    pub fn is_string_val(&self) -> bool {
        if let Expr::Value(Value::Literal(Literal::String(_))) = self {
            return true;
        }
        false
    }

    pub fn can_concat(&self, other: &Expr) -> bool {
        self.is_string_val() && other.is_string_val()
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

    pub fn is_cmp(&self) -> bool {
        matches!(
            self.op,
            BinaryOp::GreaterThan
                | BinaryOp::GreaterEqual
                | BinaryOp::LessEqual
                | BinaryOp::LessThan
                | BinaryOp::Or
                | BinaryOp::NotEqual
                | BinaryOp::Equal
                | BinaryOp::And
        )
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

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy, Eq, Ord)]
pub struct Assign {
    pub name: Symbol,
    pub val: ExprId,
}

impl Assign {
    pub fn new(name: Symbol, val: ExprId) -> Self {
        Self { name, val }
    }
    pub fn update_val(&mut self, new_val: ExprId) {
        self.val = new_val;
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
    pub callee: Option<Symbol>,
    pub args: Vec<ExprId>,
    pub arg_count: usize,
}

impl Call {
    pub fn new(callee: Option<Symbol>, args: Vec<ExprId>, arg_count: usize) -> Self {
        Self {
            callee,
            args,
            arg_count,
        }
    }
}
