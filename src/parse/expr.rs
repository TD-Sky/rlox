use std::mem;

use smol_str::SmolStr;

use super::stmt::Block;
use crate::scan::Lexeme;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Assign(Assign),
    Binary(Binary),
    Call(Call),
    Get(Get),
    Grouping(Grouping),
    Literal(Literal),
    Logical(Logical),
    Set(Set),
    Super(Super),
    This(This),
    Unary(Unary),
    Variable(Variable),
    Conditional(Conditional),
    Lambda(Lambda),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Assign {
    pub name: Variable,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Lexeme,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    pub callee: Box<Expr>,
    pub paren: Lexeme,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Get {
    pub object: Box<Expr>,
    pub name: Lexeme,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Number(f64),
    String(SmolStr),
    Null,
}

impl Eq for Literal {}

impl std::hash::Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            Self::Bool(b) => b.hash(state),
            Self::Number(x) => x.to_bits().hash(state),
            Self::String(s) => s.hash(state),
            Self::Null => (),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Lexeme,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Set {
    pub object: Box<Expr>,
    pub name: Lexeme,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Super {
    pub keyword: Lexeme,
    pub method: Lexeme,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct This {
    pub keyword: Lexeme,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Conditional {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub or_else: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Unary {
    pub operator: Lexeme,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: Lexeme,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lambda {
    pub fun: Lexeme,
    pub params: Vec<Lexeme>,
    pub body: Block,
}

impl From<Unary> for Expr {
    fn from(expr: Unary) -> Self {
        Self::Unary(expr)
    }
}

impl From<Binary> for Expr {
    fn from(expr: Binary) -> Self {
        Self::Binary(expr)
    }
}

impl From<Literal> for Expr {
    fn from(expr: Literal) -> Self {
        Self::Literal(expr)
    }
}

impl From<Grouping> for Expr {
    fn from(expr: Grouping) -> Self {
        Self::Grouping(expr)
    }
}

impl From<Conditional> for Expr {
    fn from(expr: Conditional) -> Self {
        Self::Conditional(expr)
    }
}

impl From<Variable> for Expr {
    fn from(var: Variable) -> Self {
        Self::Variable(var)
    }
}

impl From<Call> for Expr {
    fn from(expr: Call) -> Self {
        Self::Call(expr)
    }
}

impl From<Lambda> for Expr {
    fn from(lambda: Lambda) -> Self {
        Self::Lambda(lambda)
    }
}

impl From<Assign> for Expr {
    fn from(assign: Assign) -> Self {
        Self::Assign(assign)
    }
}
