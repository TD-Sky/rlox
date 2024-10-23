use std::ops::Range;

use crate::{
    parse::{
        expr::{Assign, Binary, Conditional, Expr, Grouping, Literal, Unary, Variable},
        stmt::{Block, Stmt, Var},
    },
    scan::{Lexeme, Span, Token},
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

    /// 尽可能的解析语句，遇到纯表达式的情况恢复到最后一次成功后的位置并返回
    pub fn parse_ass(&mut self) -> Result<(Vec<Stmt>, usize), ParseError> {
        let mut last_ok = self.cursor.current;
        let mut stmts = vec![];
        while !self.cursor.is_at_end() {
            match self.statement() {
                Ok(s) => {
                    stmts.push(s);
                    last_ok = self.cursor.current;
                }
                Err(e) => {
                    self.cursor.current = last_ok;
                    if self.cursor.peek().is_some_and(|lex| {
                        matches!(lex.token, Token::Print | Token::Var | Token::LeftBrace)
                    }) {
                        return Err(e);
                    } else {
                        break;
                    }
                }
            }
        }
        Ok((stmts, self.cursor.nrest()))
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = vec![];
        while !self.cursor.is_at_end() {
            stmts.push(self.statement()?);
        }
        Ok(stmts)
    }

    /// # 求值次序（从低到高）
    ///
    /// expression
    /// comma
    /// assignment
    /// conditional
    /// equality
    /// comparison
    /// term
    /// factor
    /// unary
    /// primary
    ///
    /// 低次序的规则包裹着高次序的规则，
    /// 这样就能以极其简单的方式实现次序关系，同时还完成了解析。
    ///
    /// # rule
    ///
    /// ```text
    /// expression -> assignment
    /// ```
    pub fn expression(&mut self) -> Result<Expr, ParseError> {
        self.comma()
    }
}

impl<'a> Parser<'a> {
    /// ```text
    /// statement -> varDecl | printStmt | exprStmt | block
    ///
    /// exprStmt -> expression ";"
    /// ```
    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let stmt = match self
            .cursor
            .next_if(|t| matches!(t, Token::Print | Token::Var | Token::LeftBrace))
        {
            Some(t) if t.token == Token::Print => self.print()?,
            Some(t) if t.token == Token::Var => self.declaration()?,
            Some(t) if t.token == Token::LeftBrace => self.block()?,
            None => {
                let expr = self.expression()?;
                self.semicolon()?;
                Stmt::Expr(expr)
            }
            _ => unreachable!(),
        };

        Ok(stmt)
    }

    /// ```text
    /// printStmt -> "print" expression ";"
    /// ```
    fn print(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.semicolon()?;
        Ok(Stmt::Print(value))
    }

    /// ```text
    /// varDecl -> "var" IDENTIFIER ( "=" expression )? ";"
    /// ```
    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self
            .cursor
            .next_if(|t| matches!(t, Token::Identifier(_)))
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span().range.clone(),
                msg: "Expect variable name".into(),
            })?;

        let init = if self.cursor.next_if(|t| matches!(t, Token::Equal)).is_some() {
            Some(self.expression()?)
        } else {
            None
        };

        self.semicolon()?;

        Ok(Var { name, init }.into())
    }

    /// ```text
    /// block -> "{" statement* "}"
    /// ```
    fn block(&mut self) -> Result<Stmt, ParseError> {
        let mut stmts = vec![];

        while self
            .cursor
            .peek()
            .is_some_and(|lex| lex.token != Token::RightBrace)
        {
            stmts.push(self.statement()?);
        }

        self.cursor
            .next_if(|t| matches!(t, Token::RightBrace))
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span().range.clone(),
                msg: "expect `}` after block".into(),
            })?;

        Ok(Block { stmts }.into())
    }

    /// 辅助方法，用于解析`;`
    fn semicolon(&mut self) -> Result<Lexeme, ParseError> {
        self.cursor
            .next_if(|t| matches!(t, Token::Semicolon))
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span().range.clone(),
                msg: "expect `;` after value".into(),
            })
    }

    /// ```text
    /// assignment -> ( call "." )? IDENTIFIER "=" assignment | conditional
    /// ```
    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.conditional()?;

        if let Some(equals) = self.cursor.next_if(|t| matches!(t, Token::Equal)) {
            let value = self.assignment()?;

            return if let Expr::Variable(var) = expr {
                Ok(Expr::Assign(
                    Assign {
                        name: var.name,
                        value,
                    }
                    .into(),
                ))
            } else {
                Err(ParseError {
                    span: equals.span.range.clone(),
                    msg: "invalid assignment target".into(),
                })
            };
        }

        Ok(expr)
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
        let Some(lex) = self.cursor.next_if(|t| {
            matches!(
                t,
                Token::False
                    | Token::True
                    | Token::Nil
                    | Token::Number(_)
                    | Token::String(_)
                    | Token::LeftParen
                    | Token::Identifier(_)
            )
        }) else {
            unreachable!();
        };
        let expr = match &lex.token {
            Token::True => Literal::Bool(true).into(),
            Token::False => Literal::Bool(false).into(),
            Token::Nil => Literal::Null.into(),
            Token::Number(x) => Literal::Number(*x).into(),
            Token::String(s) => Literal::String(s[1..s.len() - 1].into()).into(),
            Token::LeftParen => {
                let expr = self.expression()?;
                self.cursor
                    .next_if(|t| matches!(t, Token::RightParen))
                    .ok_or_else(|| ParseError {
                        span: self.cursor.next_span().range.clone(),
                        msg: "Expect ')' after expression".into(),
                    })?;
                Grouping { expression: expr }.into()
            }
            Token::Identifier(_) => Variable { name: lex }.into(),
            _ => unreachable!(),
        };

        Ok(expr)
    }

    /// ```text
    /// comma -> assignment ("," expression)*
    /// ```
    fn comma(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.assignment()?;

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

        if let Some(question) = self.cursor.next_if(|t| matches!(t, Token::Question)) {
            let then = self.expression()?;
            let colon = self
                .cursor
                .next_if(|t| matches!(t, Token::Colon))
                .ok_or_else(|| ParseError {
                    span: self.cursor.next_span().range.clone(),
                    msg: "expect `:` for ternary expression".into(),
                })?;
            let or_else = self.conditional()?;
            expr = Conditional {
                cond: expr,
                question,
                then,
                colon,
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
    fn peek(&self) -> Option<&Lexeme> {
        self.lexemes.get(self.current)
    }

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

    const fn is_at_end(&self) -> bool {
        self.current == self.lexemes.len()
    }

    const fn nrest(&self) -> usize {
        self.lexemes.len() - self.current
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub span: Range<usize>,
    pub msg: String,
}