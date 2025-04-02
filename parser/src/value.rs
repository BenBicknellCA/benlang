use crate::expr::BinaryOp;

use crate::object::FuncId;
use crate::object::Object;
use crate::scanner::{Symbol, SymbolTable};
use anyhow::{anyhow, Result};
use std::mem::discriminant;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy)]
pub enum Value {
    Object(Object),
    Literal(Literal),
}

impl Default for Value {
    fn default() -> Self {
        Value::Literal(Literal::Nil)
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy, Default)]
pub enum Literal {
    Function(FuncId),
    String(Symbol),
    Number(u32),
    Float(f32),
    Bool(bool),
    #[default]
    Nil,
}

impl Literal {
    pub fn is_bool(self) -> bool {
        matches!(self, Literal::Bool(_))
    }

    pub fn can_neg(&self) -> bool {
        matches!(self, Literal::Float(_))
    }

    pub fn get_bool(self) -> Result<bool> {
        if let Literal::Bool(boolval) = self {
            return Ok(boolval);
        }
        Err(anyhow!("cannot get bool from {:?}", self))
    }
    pub fn get_func(self) -> Result<FuncId> {
        if let Literal::Function(func) = self {
            return Ok(func);
        }
        Err(anyhow!("cannot get function from {:?}", self))
    }

    pub fn is_string(&self) -> bool {
        if let Literal::String(_) = *self {
            return true;
        }
        false
    }
    pub fn same_discriminant(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }
    pub fn add_string(self, other: Literal, symbol_table: &mut SymbolTable) -> Result<Self> {
        if let Literal::String(lhs) = self {
            if let Literal::String(rhs) = other {
                let lhs = symbol_table.resolve(lhs).unwrap();
                let rhs = symbol_table.resolve(rhs).unwrap();
                let concat = symbol_table.get_or_intern(lhs.to_string() + rhs);
                return Ok(Literal::String(concat));
            }
        }
        Err(anyhow!("Cannot {:?} add {:?}", self, other))
    }
    pub fn get_symbol(&self) -> Result<Symbol> {
        if let Literal::String(sym) = self {
            return Ok(*sym);
        }
        Err(anyhow!("Cannot get symbol from: {:?}", self))
    }
    pub fn get_number(&self) -> Result<u32> {
        if let Literal::Number(num) = self {
            return Ok(*num);
        }
        Err(anyhow!("Cannot get number from: {:?}", self))
    }
    pub fn is_number(&self) -> bool {
        matches!(self, Literal::Number(_) | Literal::Float(_))
    }

    pub fn fold_and_or(&self, op: BinaryOp, rhs: &Literal) -> Result<bool> {
        let lhs = self.get_bool()?;
        let rhs = rhs.get_bool()?;
        match op {
            BinaryOp::And => Ok(lhs && rhs),
            BinaryOp::Or => Ok(lhs || rhs),
            _ => Err(anyhow!("cannot fold {lhs:?} and {rhs:?} // {op:?}")),
        }
    }
}
impl Mul for Literal {
    type Output = Literal;

    fn mul(self, other: Literal) -> Literal {
        assert!(self.is_number() && other.is_number());

        if let Literal::Number(lhs) = self {
            let rhs = other.get_number().unwrap();
            return Literal::Number(lhs / rhs);
        }
        panic!("Cannot mul {:?} * {:?}", self, other)
    }
}


impl Div for Literal {
    type Output = Literal;

    fn div(self, other: Literal) -> Literal {
        assert!(self.is_number() && other.is_number());

        if let Literal::Number(lhs) = self {
            let rhs = other.get_number().unwrap();
            return Literal::Number(lhs / rhs);
        }
        panic!("Cannot div {:?} / {:?}", self, other)
    }
}

impl Add for Literal {
    type Output = Literal;

    fn add(self, other: Literal) -> Literal {
        assert!(self.is_number() && other.is_number() || self.is_string() && other.is_string());
        if let Literal::Number(lhs) = self {
            let rhs = other.get_number().unwrap();
            return Literal::Number(lhs + rhs);
        }
        panic!("Cannot add {:?} and {:?} with `+` operator", self, other)
    }
}

impl Sub for Literal {
    type Output = Literal;

    fn sub(self, other: Literal) -> Literal {
        assert!(self.is_number() && other.is_number() || self.is_string() && other.is_string());
        if let Literal::Number(lhs) = self {
            let rhs = other.get_number().unwrap();
            return Literal::Number(lhs - rhs);
        }
        panic!("Cannot add {:?} and {:?} with `+` operator", self, other)
    }
}

impl Not for Literal {
    type Output = Self;

    fn not(self) -> Self {
        Literal::Bool(!self.get_bool().unwrap())
    }
}

impl Neg for Literal {
    type Output = Self;

    fn neg(self) -> Self {
        if let Literal::Float(num) = self {
            return Literal::Float(num.neg());
        };
        panic!("Cannot negate {:?}", self)
    }
}

impl Value {
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Literal(Literal::Bool(_)))
    }

    pub fn same_variant(&self, other: &Value) -> bool {
        discriminant(self) == discriminant(other)
    }
    pub fn get_literal(&self) -> Result<Literal> {
        if let Value::Literal(lit) = self {
            return Ok(*lit);
        }
        Err(anyhow!("Cannot get literal from: {:?}", self))
    }
    pub fn add_literals(&self, rhs: &Value) -> Result<Value> {
        if let Value::Literal(lhs) = self {
            let rhs = rhs.get_literal()?;
            return Ok(Value::Literal(*lhs + rhs));
        }
        Err(anyhow!("Cannot add literals {:?} and {:?}", self, rhs))
    }

    pub const fn is_string_lit(&self) -> bool {
        matches!(self, Value::Literal(Literal::String(_)))
    }

    pub fn concat_value_strings(
        &self,
        rhs: Value,
        symbol_table: &mut SymbolTable,
    ) -> Result<Value> {
        let concat = rhs
            .get_literal()?
            .add_string(rhs.get_literal()?, symbol_table)?;
        Ok(Value::Literal(concat))
    }
}
