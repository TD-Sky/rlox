use smol_str::SmolStr;

use super::stmt::Block;
use crate::scan::Lexeme;

#[derive(Debug, Clone)]
pub enum Expr {
    Assign(Box<Assign>),
    Binary(Box<Binary>),
    Call(Box<Call>),
    Get(Box<Get>),
    Grouping(Box<Grouping>),
    Literal(Literal),
    Logical(Box<Logical>),
    Set(Box<Set>),
    Super(Super),
    This(This),
    Unary(Box<Unary>),
    Variable(Variable),
    Conditional(Box<Conditional>),
    Lambda(Lambda),
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub name: Lexeme,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Expr,
    pub operator: Lexeme,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Expr,
    pub paren: Lexeme,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Get {
    pub object: Expr,
    pub name: Lexeme,
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Bool(bool),
    Number(f64),
    String(SmolStr),
    Null,
}

#[derive(Debug, Clone)]
pub struct Logical {
    pub left: Expr,
    pub operator: Lexeme,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub object: Expr,
    pub name: Lexeme,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Super {
    pub keyword: Lexeme,
    pub method: Lexeme,
}

#[derive(Debug, Clone)]
pub struct This {
    pub keyword: Lexeme,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub cond: Expr,
    pub then: Expr,
    pub or_else: Expr,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Lexeme,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Lexeme,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub fun: Lexeme,
    pub params: Vec<Lexeme>,
    pub body: Block,
}

impl From<Unary> for Expr {
    fn from(expr: Unary) -> Self {
        Self::Unary(Box::new(expr))
    }
}

impl From<Binary> for Expr {
    fn from(expr: Binary) -> Self {
        Self::Binary(Box::new(expr))
    }
}

impl From<Literal> for Expr {
    fn from(expr: Literal) -> Self {
        Self::Literal(expr)
    }
}

impl From<Grouping> for Expr {
    fn from(expr: Grouping) -> Self {
        Self::Grouping(Box::new(expr))
    }
}

impl From<Conditional> for Expr {
    fn from(expr: Conditional) -> Self {
        Self::Conditional(Box::new(expr))
    }
}

impl From<Variable> for Expr {
    fn from(var: Variable) -> Self {
        Self::Variable(var)
    }
}

impl From<Call> for Expr {
    fn from(expr: Call) -> Self {
        Self::Call(Box::new(expr))
    }
}

impl From<Lambda> for Expr {
    fn from(lambda: Lambda) -> Self {
        Self::Lambda(lambda)
    }
}
