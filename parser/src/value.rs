use crate::ExprPool;
use crate::Value;
use crate::expr;
use crate::scanner::{Symbol, SymbolTable};
use anyhow::{Error, Result, anyhow};
use std::ops::{Add, Div, Mul, Neg, Not, Sub};
macro_rules! value_impl_bin_ops_ref {
    ($ident:path, $op:tt, $sym:tt, $variant:tt) => {
        impl $ident for &Value {
            type Output = Value;
            fn $op(self, other: Self) -> Self::Output{
                if let Value::$variant(lhs) = self {
                    if let Value::$variant(rhs) = other {
                        return Value::$variant(lhs $sym rhs);
                    }
                }
            panic!("Cannot perform operation between {:?} and {:?} // strings must be manually \
            concat with the function in impl Value", self, other)
            }
        }
    }
}

macro_rules! value_impl_bin_ops {
    ($ident:path, $op:tt, $sym:tt, $variant:tt) => {
        impl $ident for Value {
            type Output = Value ;
            fn $op(self, other: Self) -> Self::Output {
                if let Value::$variant(lhs) = self {
                    if let Value::$variant(rhs) = other {
                        return Value::$variant(lhs $sym rhs);
                    }
                }
            panic!("Cannot perform operation between {:?} and {:?} // strings must be manually \
            concat with the function in impl Value", self, other)
            }
        }
    }
}
macro_rules! value_impl {
    ($op:tt, $variant:tt, $sym:tt) => {

            type Output = Self;
            fn $op(self) -> Self::Output {
                if let Value::$variant(val) = self {
                    return Value::$variant($sym val);
                }
                panic!()}
            }

    }

value_impl_bin_ops_ref!(Add, add, +, Number);
value_impl_bin_ops_ref!(Sub, sub, -, Number);
value_impl_bin_ops_ref!(Mul, mul, *, Number);
value_impl_bin_ops_ref!(Div, div, /, Number);

value_impl_bin_ops!(Add, add, +, Number);
value_impl_bin_ops!(Sub, sub, -, Number);
value_impl_bin_ops!(Mul, mul, *, Number);
value_impl_bin_ops!(Div, div, /, Number);

impl Not for Value {
    type Output = Value;
    fn not(self) -> Self::Output {
        if let Value::Bool(boolval) = self {
            return Value::Bool(!boolval);
        }
        panic!()
    }
}

impl Neg for Value {
    type Output = Value;
    fn neg(self) -> Self::Output {
        if let Value::Number(num) = self {
            return Value::Number(num.neg());
        }
        panic!()
    }
}

impl Not for &Value {
    type Output = Value;
    fn not(self) -> Self::Output {
        if let Value::Bool(boolval) = self {
            return Value::Bool(!boolval);
        }
        panic!()
    }
}

impl Neg for &Value {
    type Output = Value;
    fn neg(self) -> Self::Output {
        if let Value::Number(num) = self {
            return Value::Number(num.neg());
        }
        panic!()
    }
}

impl Value {
    //    value_impl ! (and, &&, Bool);
    //    value_impl ! (or, ||, Bool);

    pub fn is_string_lit(&self) -> bool {
        matches!(self, Value::StringLiteral(_))
    }

    pub fn same_variant(&self, other: &Value) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    pub fn add_string(self, other: Value, symbol_table: &mut SymbolTable) -> Self {
        if let Value::StringLiteral(lhs) = self {
            if let Value::StringLiteral(rhs) = other {
                let lhs = symbol_table.resolve(lhs).unwrap();
                let rhs = symbol_table.resolve(rhs).unwrap();
                let concat = symbol_table.get_or_intern(lhs.to_string() + rhs);
                return Value::StringLiteral(concat);
            }
        }
        panic!("Cannot {:?} add {:?}", self, other)
    }
}
