//! # 求值次序（从低到高）
//!
//! expression
//! equality
//! comparison
//! term
//! factor
//! unary
//! primary
//!
//! 低次序的规则包裹着高次序的规则，
//! 这样就能以极其简单的方式实现次序关系，同时还完成了解析。

use std::ops::Range;

use crate::{
    expr::{Binary, Conditional, Expr, Grouping, Literal, Unary},
    scan::{Lexeme, Span, Token},
    stmt::Stmt,
};

#[derive(Debug)]
pub struct Parser<'a> {
    cursor: Cursor<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexemes: &'a [Lexeme]) -> Self {
        let (eof, lexemes) = lexemes.split_last().unwrap();

        Self {
            cursor: Cursor {
                lexemes,
                eof,
                current: 0,
            },
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = vec![];
        while !self.cursor.is_at_end() {
            stmts.push(self.statement()?);
        }
        Ok(stmts)
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let kind = if self.cursor.next_if(|t| matches!(t, Token::Print)).is_some() {
            Stmt::Print
        } else {
            Stmt::Expr
        };

        let expr = self.expression()?;
        self.cursor
            .next_if(|t| matches!(t, Token::Semicolon))
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span().range.clone(),
                msg: "expect `;` after value".into(),
            })?;

        Ok(kind(expr))
    }

    /// ```text
    /// expression -> assignment
    /// ```
    pub fn expression(&mut self) -> Result<Expr, ParseError> {
        self.conditional()
    }

    /// ```text
    /// equality -> comparison ( ( "!=" | "==" ) comparison )*
    /// ```
    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while let Some(operator) = self
            .cursor
            .next_if(|t| matches!(t, Token::BangEqual | Token::EqualEqual))
        {
            let right = self.comparison()?;
            expr = Binary {
                left: expr,
                operator,
                right,
            }
            .into();
        }

        Ok(expr)
    }

    /// ```text
    /// comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )*
    /// ```
    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while let Some(operator) = self.cursor.next_if(|t| {
            matches!(
                t,
                Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual
            )
        }) {
            let right = self.term()?;
            expr = Binary {
                left: expr,
                operator,
                right,
            }
            .into();
        }

        Ok(expr)
    }

    /// ```text
    /// term -> factor ( ( "-" | "+" ) factor )*
    /// ```
    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while let Some(operator) = self
            .cursor
            .next_if(|t| matches!(t, Token::Minus | Token::Plus))
        {
            let right = self.factor()?;
            expr = Binary {
                left: expr,
                operator,
                right,
            }
            .into();
        }

        Ok(expr)
    }

    /// ```text
    /// factor -> unary ( ( "/" | "*" ) unary )*
    /// ```
    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while let Some(operator) = self
            .cursor
            .next_if(|t| matches!(t, Token::Slash | Token::Star))
        {
            let right = self.unary()?;
            expr = Binary {
                left: expr,
                operator,
                right,
            }
            .into();
        }

        Ok(expr)
    }

    /// ```text
    /// unary -> ( "!" | "-" ) unary | call
    /// ```
    fn unary(&mut self) -> Result<Expr, ParseError> {
        let expr = if let Some(operator) = self
            .cursor
            .next_if(|t| matches!(t, Token::Bang | Token::Minus))
        {
            let right = self.unary()?;
            Unary { operator, right }.into()
        } else {
            self.primary()?
        };

        Ok(expr)
    }

    /// ```text
    /// primary -> "true" | "false" | "nil" | "this"
    ///           | NUMBER | STRING | IDENTIFIER | "(" expression ")"
    ///           | "super" "." IDENTIFIER
    /// ```
    fn primary(&mut self) -> Result<Expr, ParseError> {
        let expr = match self
            .cursor
            .next_if(|t| {
                matches!(
                    t,
                    Token::False
                        | Token::True
                        | Token::Nil
                        | Token::Number(_)
                        | Token::String(_)
                        | Token::LeftParen
                )
            })
            .map(|lex| lex.token)
        {
            Some(Token::True) => Literal::Bool(true).into(),
            Some(Token::False) => Literal::Bool(false).into(),
            Some(Token::Nil) => Literal::Null.into(),
            Some(Token::Number(x)) => Literal::Number(x).into(),
            Some(Token::String(s)) => Literal::String(s[1..s.len() - 1].into()).into(),
            Some(Token::LeftParen) => {
                let expr = self.expression()?;
                self.cursor
                    .next_if(|t| matches!(t, Token::RightParen))
                    .ok_or_else(|| ParseError {
                        span: self.cursor.next_span().range.clone(),
                        msg: "Expect ')' after expression".into(),
                    })?;
                Grouping { expression: expr }.into()
            }
            _ => todo!(),
        };

        Ok(expr)
    }

    /// ```text
    /// comma -> expression ("," expression)*
    /// ```
    fn comma(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while let Some(comma) = self.cursor.next_if(|t| matches!(t, Token::Comma)) {
            expr = Binary {
                left: expr,
                operator: comma,
                right: self.expression()?,
            }
            .into();
        }

        Ok(expr)
    }

    /// ```text
    /// conditional -> equality ( "?" expression ":" conditional )?
    /// ```
    fn conditional(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        if self
            .cursor
            .next_if(|t| matches!(t, Token::Question))
            .is_some()
        {
            let then = self.expression()?;
            self.cursor
                .next_if(|t| matches!(t, Token::Colon))
                .ok_or_else(|| ParseError {
                    span: self.cursor.next_span().range.clone(),
                    msg: "expect `:` for ternary expression".into(),
                })?;
            let or_else = self.conditional()?;
            expr = Conditional {
                cond: expr,
                then,
                or_else,
            }
            .into()
        }

        Ok(expr)
    }
}

#[derive(Debug)]
struct Cursor<'a> {
    lexemes: &'a [Lexeme],
    eof: &'a Lexeme,
    current: usize,
}

impl Cursor<'_> {
    fn next_if(&mut self, func: impl FnOnce(&Token) -> bool) -> Option<Lexeme> {
        self.lexemes
            .get(self.current)
            .filter(|lex| func(&lex.token))
            .cloned()
            .inspect(|_| self.current += 1)
    }

    fn next_span(&self) -> &Span {
        self.lexemes
            .get(self.current)
            .map(|lex| &lex.span)
            .unwrap_or(&self.eof.span)
    }

    fn is_at_end(&self) -> bool {
        self.current == self.lexemes.len()
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub span: Range<usize>,
    pub msg: String,
}
