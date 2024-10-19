use crate::{expr::Expr, scan::Lexeme};

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Var),
}

#[derive(Debug)]
pub struct Var {
    pub name: Lexeme,
    pub init: Option<Expr>,
}

impl From<Var> for Stmt {
    fn from(var: Var) -> Self {
        Stmt::Var(var)
    }
}
