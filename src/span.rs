use std::ops::Range;

use crate::parse::types::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Span {
    pub range: Range<usize>,
    pub line: usize,
}

impl Span {
    pub const fn new(range: Range<usize>, line: usize) -> Self {
        Self { range, line }
    }

    /// 以自身为起始，`other`为结束，圈起一部分区域
    pub fn enclose(&self, other: &Self) -> Self {
        debug_assert!(self.range.start < other.range.end);

        Self {
            range: self.range.start..other.range.end,
            line: self.line,
        }
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Assign(e) => e.span(),
            Expr::Binary(e) => e.span(),
            Expr::Call(e) => e.span(),
            Expr::Get(e) => e.span(),
            Expr::Grouping(e) => e.span(),
            Expr::Literal(e) => e.span(),
            Expr::Set(e) => e.span(),
            Expr::Super(e) => e.span(),
            Expr::This(e) => e.span(),
            Expr::Unary(e) => e.span(),
            Expr::Variable(e) => e.span(),
            Expr::Conditional(e) => e.span(),
            Expr::Lambda(e) => e.span(),
        }
    }
}

impl Spanned for Assign {
    fn span(&self) -> Span {
        self.name.name.span.enclose(&self.value.span())
    }
}

impl Spanned for Binary {
    fn span(&self) -> Span {
        self.left.span().enclose(&self.right.span())
    }
}

impl Spanned for Call {
    fn span(&self) -> Span {
        self.callee.span().enclose(&self.paren.span)
    }
}

impl Spanned for Get {
    fn span(&self) -> Span {
        self.object.span().enclose(&self.name.span)
    }
}

impl Spanned for Grouping {
    fn span(&self) -> Span {
        self.expression.span()
    }
}

impl Spanned for Literal {
    fn span(&self) -> Span {
        match self {
            Literal::Bool(_) => todo!(),
            Literal::Number(_) => todo!(),
            Literal::String(_) => todo!(),
            Literal::Null => todo!(),
        }
    }
}

impl Spanned for Set {
    fn span(&self) -> Span {
        self.object.span().enclose(&self.value.span())
    }
}

impl Spanned for Super {
    fn span(&self) -> Span {
        self.keyword.span.enclose(&self.method.span)
    }
}

impl Spanned for This {
    fn span(&self) -> Span {
        self.keyword.span.clone()
    }
}

impl Spanned for Unary {
    fn span(&self) -> Span {
        self.operator.span.enclose(&self.right.span())
    }
}

impl Spanned for Variable {
    fn span(&self) -> Span {
        self.name.span.clone()
    }
}

impl Spanned for Conditional {
    fn span(&self) -> Span {
        self.cond.span().enclose(&self.or_else.span())
    }
}

impl Spanned for Lambda {
    fn span(&self) -> Span {
        self.fun.span.enclose(&self.body.span())
    }
}

impl Spanned for Block {
    fn span(&self) -> Span {
        self.left_brace.span.enclose(&self.right_brace.span)
    }
}

impl Spanned for Return {
    fn span(&self) -> Span {
        if let Some(expr) = &self.expr {
            self.keyword.span.enclose(&expr.span())
        } else {
            self.keyword.span.clone()
        }
    }
}
