use smol_str::SmolStr;

use crate::scan::Token;

/// ```text
/// expression  -> assignment ;
///
/// assignment  -> ( call "." )? IDENTIFIER "=" assignment
///                | logic_or ;
///
/// logic_or    -> logic_and ( "or" logic_and )* ;
///
/// logic_and   -> equality ( "and" equality )* ;
///
/// equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
///
/// comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
///
/// term        -> factor ( ( "-" | "+" ) factor )* ;
///
/// factor      -> unary ( ( "/" | "*" ) unary )* ;
///
/// unary       -> ( "!" | "-" ) unary | call ;
///
/// call        -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
///
/// primary     -> "true" | "false" | "nil" | "this"
///                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
///                | "super" "." IDENTIFIER ;
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
}

#[derive(Debug)]
pub struct Assign {
    pub name: Token,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Binary {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct Call {
    pub callee: Expr,
    pub paren: Token,
    pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct Get {
    pub object: Expr,
    pub name: Token,
}

#[derive(Debug)]
pub struct Grouping {
    pub expression: Expr,
}

#[derive(Debug)]
pub enum Literal {
    Bool(bool),
    Number(f64),
    String(SmolStr),
}

#[derive(Debug)]
pub struct Logical {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct Set {
    pub object: Expr,
    pub name: Token,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Super {
    pub keyword: Token,
    pub method: Token,
}

#[derive(Debug)]
pub struct This {
    pub keyword: Token,
}

#[derive(Debug)]
pub struct Unary {
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Token,
}
