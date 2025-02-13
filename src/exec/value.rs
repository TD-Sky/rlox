use std::rc::Rc;

use smol_str::SmolStr;

use super::{
    call::LoxCallable,
    intp::{LoxClass, LoxFunction, LoxInstance},
};
use crate::parse::types::Literal;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Number(f64),
    String(SmolStr),
    Null,
    Callable(Rc<dyn LoxCallable>),
    Instance(LoxInstance),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::Number(lhs), Self::Number(rhs)) => (lhs.is_nan() && rhs.is_nan()) || lhs == rhs,
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            (Self::Null, Self::Null) => true,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Value {
    pub fn as_bool(&self) -> bool {
        // Weak type
        match self {
            Self::Bool(b) => *b,
            Self::Null => false,
            _ => true,
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            Self::Number(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_fun(&self) -> Option<&LoxFunction> {
        match self {
            Value::Callable(c) => c.as_ref().downcast_ref(),
            _ => None,
        }
    }

    pub fn into_class(self) -> Option<Rc<LoxClass>> {
        match self {
            Value::Callable(c) => c.downcast_rc().ok(),
            _ => None,
        }
    }

    pub fn into_instance(self) -> Option<LoxInstance> {
        match self {
            Value::Instance(instance) => Some(instance),
            _ => None,
        }
    }

    pub fn into_fun(self) -> Option<Rc<LoxFunction>> {
        match self {
            Value::Callable(c) => c.downcast_rc().ok(),
            _ => None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(x) => write!(f, "{x}"),
            Value::String(s) => f.write_str(s),
            Value::Null => f.write_str("null"),
            Value::Callable(c) => write!(f, "{c}"),
            Value::Instance(it) => write!(f, "{it}"),
        }
    }
}

impl From<Literal> for Value {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Bool(b) => Self::Bool(b),
            Literal::Number(x) => Self::Number(x),
            Literal::String(s) => Self::String(s),
            Literal::Null => Self::Null,
        }
    }
}

impl<T> From<T> for Value
where
    T: LoxCallable + 'static,
{
    fn from(c: T) -> Self {
        Self::Callable(Rc::new(c))
    }
}

impl From<LoxInstance> for Value {
    fn from(value: LoxInstance) -> Self {
        Self::Instance(value)
    }
}
