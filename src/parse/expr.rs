use smol_str::SmolStr;

use crate::scan::Lexeme;

/// ```text
/// assignment  -> ( call "." )? IDENTIFIER "=" assignment
///                | logic_or ;
///
/// logic_or    -> logic_and ( "or" logic_and )* ;
///
/// logic_and   -> equality ( "and" equality )* ;
///
/// call        -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
///
/// ```
#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct Assign {
    pub name: Lexeme,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Binary {
    pub left: Expr,
    pub operator: Lexeme,
    pub right: Expr,
}

#[derive(Debug)]
pub struct Call {
    pub callee: Expr,
    pub paren: Lexeme,
    pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct Get {
    pub object: Expr,
    pub name: Lexeme,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Logical {
    pub left: Expr,
    pub operator: Lexeme,
    pub right: Expr,
}

#[derive(Debug)]
pub struct Set {
    pub object: Expr,
    pub name: Lexeme,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Super {
    pub keyword: Lexeme,
    pub method: Lexeme,
}

#[derive(Debug)]
pub struct This {
    pub keyword: Lexeme,
}

#[derive(Debug)]
pub struct Conditional {
    pub cond: Expr,
    pub question: Lexeme,
    pub then: Expr,
    pub colon: Lexeme,
    pub or_else: Expr,
}

#[derive(Debug)]
pub struct Unary {
    pub operator: Lexeme,
    pub right: Expr,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Lexeme,
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
