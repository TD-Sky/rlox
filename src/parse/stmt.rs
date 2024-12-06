use crate::{parse::expr::Expr, scan::Lexeme};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Var),
    Block(Block),
    If(Box<If>),
    While(Box<While>),
    For(Box<For>),
    Break(Break),
    Fun(Function),
    Return(Return),
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: Lexeme,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub left_brace: Lexeme,
    pub stmts: Vec<Stmt>,
    pub right_brace: Lexeme,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expr,
    pub body: Stmt,
}

#[derive(Debug, Clone)]
pub struct For {
    pub init: Option<Stmt>,
    pub condition: Option<Expr>,
    pub change: Option<Expr>,
    pub body: Stmt,
}

#[derive(Debug, Clone)]
pub struct Break {
    pub token: Lexeme,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Lexeme,
    pub params: Vec<Lexeme>,
    pub body: Block,
}

#[derive(Debug, Clone)]
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
        Self::If(Box::new(stmt))
    }
}

impl From<While> for Stmt {
    fn from(stmt: While) -> Self {
        Self::While(Box::new(stmt))
    }
}

impl From<For> for Stmt {
    fn from(stmt: For) -> Self {
        Self::For(Box::new(stmt))
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
