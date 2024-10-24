use smol_str::SmolStr;

use crate::parse::expr::Literal;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Number(f64),
    String(SmolStr),
    Null,
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
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(b) => Some(*b),
            Self::Null => Some(false), // Weak type
            _ => None,
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
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(x) => write!(f, "{x}"),
            Value::String(s) => f.write_str(s),
            Value::Null => f.write_str("null"),
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
