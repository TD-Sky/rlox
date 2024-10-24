use crate::{parse::expr::Expr, scan::Lexeme};

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Var),
    Block(Block),
}

#[derive(Debug)]
pub struct Var {
    pub name: Lexeme,
    pub init: Option<Expr>,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl From<Var> for Stmt {
    fn from(var: Var) -> Self {
        Stmt::Var(var)
    }
}

impl From<Block> for Stmt {
    fn from(block: Block) -> Self {
        Self::Block(block)
    }
}
