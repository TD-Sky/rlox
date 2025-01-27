use crate::{parse::types::Expr, scan::Lexeme};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Var),
    Block(Block),
    If(If),
    While(While),
    For(For),
    Break(Break),
    Fun(Function),
    Return(Return),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub name: Lexeme,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub left_brace: Lexeme,
    pub stmts: Vec<Stmt>,
    pub right_brace: Lexeme,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct If {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct While {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct For {
    pub init: Option<Box<Stmt>>,
    pub condition: Option<Expr>,
    pub change: Option<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Break {
    pub token: Lexeme,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub name: Lexeme,
    pub params: Vec<Lexeme>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Return {
    pub keyword: Lexeme,
    pub expr: Option<Expr>,
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

impl From<If> for Stmt {
    fn from(stmt: If) -> Self {
        Self::If(stmt)
    }
}

impl From<While> for Stmt {
    fn from(stmt: While) -> Self {
        Self::While(stmt)
    }
}

impl From<For> for Stmt {
    fn from(stmt: For) -> Self {
        Self::For(stmt)
    }
}

impl From<Break> for Stmt {
    fn from(b: Break) -> Self {
        Self::Break(b)
    }
}

impl From<Function> for Stmt {
    fn from(f: Function) -> Self {
        Self::Fun(f)
    }
}

impl From<Return> for Stmt {
    fn from(rt: Return) -> Self {
        Self::Return(rt)
    }
}
