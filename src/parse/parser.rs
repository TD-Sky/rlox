use crate::{
    parse::types::*,
    scan::{Lexeme, Token},
    span::Span,
};

#[derive(Debug)]
pub struct Parser<'s> {
    cursor: Cursor<'s>,
}

impl<'s> Parser<'s> {
    pub fn new(lexemes: &'s [Lexeme]) -> Self {
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
                Ok(stmt) => {
                    stmts.push(stmt);
                    last_ok = self.cursor.current;
                }
                Err(e) => {
                    self.cursor.current = last_ok;
                    if self
                        .cursor
                        .peek()
                        .is_some_and(|lex| is_statement_begin(&lex.token))
                    {
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
    /// logic_or
    /// logic_and
    /// equality
    /// comparison
    /// term
    /// factor
    /// unary
    /// call
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

impl Parser<'_> {
    /// ```text
    /// statement -> exprStmt
    ///            | varDecl
    ///            | printStmt
    ///            | block
    ///            | ifStmt
    ///            | funDecl
    ///            | class
    ///
    /// funDecl -> "fun" function
    /// ```
    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let stmt = match self.cursor.next_if(is_statement_begin) {
            Some(t) if t.token == Token::Print => self.print()?,
            Some(t) if t.token == Token::Var => self.declaration()?.into(),
            Some(t) if t.token == Token::LeftBrace => self.block(t)?.into(),
            Some(t) if t.token == Token::If => self.if_stmt()?.into(),
            Some(t) if t.token == Token::While => self.while_stmt()?.into(),
            Some(t) if t.token == Token::For => self.for_stmt()?.into(),
            Some(t) if t.token == Token::Break => self.break_stmt(t)?.into(),
            Some(t) if t.token == Token::Fun => self.function("function")?.into(),
            Some(t) if t.token == Token::Return => self.return_stmt(t)?.into(),
            Some(t) if t.token == Token::Class => self.class(t)?.into(),
            None => Stmt::Expr(self.expr_stmt()?),
            _ => unreachable!(),
        };

        Ok(stmt)
    }

    /// ```text
    /// exprStmt -> expression ";"
    /// ```
    fn expr_stmt(&mut self) -> Result<Expr, ParseError> {
        let expr = self.expression()?;
        self.semicolon()?;
        Ok(expr)
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
    fn declaration(&mut self) -> Result<Var, ParseError> {
        let name = self
            .cursor
            .next_if(|t| matches!(t, Token::Identifier(_)))
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "Expect variable name".into(),
            })?;

        let init = if self.cursor.next_if(|t| matches!(t, Token::Equal)).is_some() {
            Some(self.expression()?)
        } else {
            None
        };

        self.semicolon()?;

        Ok(Var { name, init })
    }

    /// ```text
    /// block -> "{" statement* "}"
    /// ```
    fn block(&mut self, left_brace: Lexeme) -> Result<Block, ParseError> {
        let mut stmts = vec![];

        while self
            .cursor
            .peek()
            .is_some_and(|lex| lex.token != Token::RightBrace)
        {
            stmts.push(self.statement()?);
        }

        let right_brace = self
            .cursor
            .next_if(|t| matches!(t, Token::RightBrace))
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect `}` after block".into(),
            })?;

        Ok(Block {
            left_brace,
            stmts,
            right_brace,
        })
    }

    /// ```text
    /// ifStmt -> "if" "(" expression ")" statement ( "else" statement )?
    /// ```
    fn if_stmt(&mut self) -> Result<If, ParseError> {
        self.cursor
            .next_if_eq(Token::LeftParen)
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect `(` after `if`".into(),
            })?;
        let condition = self.expression()?;
        self.cursor
            .next_if_eq(Token::RightParen)
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect `)` after `if` condition".into(),
            })?;

        let then_branch = self.statement()?;
        let else_branch = if self.cursor.next_if_eq(Token::Else).is_some() {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    /// ```text
    /// whileStmt -> "while" "(" expression ")" statement
    /// ```
    fn while_stmt(&mut self) -> Result<While, ParseError> {
        self.cursor
            .next_if_eq(Token::LeftParen)
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect `(` after `while`".into(),
            })?;
        let condition = self.expression()?;
        self.cursor
            .next_if_eq(Token::RightParen)
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect `)` after `while` condition".into(),
            })?;

        Ok(While {
            condition,
            body: Box::new(self.statement()?),
        })
    }

    /// forStmt -> "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
    fn for_stmt(&mut self) -> Result<For, ParseError> {
        self.cursor
            .next_if_eq(Token::LeftParen)
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect `(` after `for`".into(),
            })?;

        let init = match self
            .cursor
            .next_if(|t| matches!(t, Token::Var | Token::Semicolon))
        {
            Some(t) if t.token == Token::Var => Some(self.declaration()?.into()),
            Some(t) if t.token == Token::Semicolon => None,
            _ => Some(Stmt::Expr(self.expr_stmt()?)),
        }
        .map(Box::new);

        let condition = match self.cursor.next_if_eq(Token::Semicolon) {
            Some(t) if t.token == Token::Semicolon => None,
            _ => Some(self.expr_stmt()?),
        };

        let change = if self
            .cursor
            .peek()
            .is_some_and(|lex| lex.token != Token::RightParen)
        {
            Some(self.expression()?)
        } else {
            None
        };

        self.cursor
            .next_if_eq(Token::RightParen)
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect `)` after `for` clauses".into(),
            })?;

        let body = self.statement()?;

        Ok(For {
            init,
            condition,
            change,
            body: Box::new(body),
        })
    }

    /// ```text
    /// class -> "class" IDENTIFIER "{" function* "}"
    /// ```
    fn class(&mut self, _class: Lexeme) -> Result<Class, ParseError> {
        let name = self
            .cursor
            .next_if(|t| matches!(t, Token::Identifier(_)))
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect class name".into(),
            })?;

        self.cursor
            .next_if_eq(Token::LeftBrace)
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect `{` before class body".into(),
            })?;

        let mut methods = vec![];
        while !self.cursor.is_at_end() && self.cursor.next_if_eq(Token::RightBrace).is_none() {
            methods.push(self.function("method")?);
        }

        self.cursor
            .next_if_eq(Token::RightBrace)
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "expect `}` after class body".into(),
            })?;

        Ok(Class { name, methods })
    }

    /// 辅助方法，用于解析`;`
    fn semicolon(&mut self) -> Result<Lexeme, ParseError> {
        self.cursor
            .next_if_eq(Token::Semicolon)
            .ok_or_else(|| ParseError {
                span: self.cursor.prev_span(),
                msg: "expect `;` after statement".into(),
            })
    }

    /// ```text
    /// assignment -> ( call "." )? IDENTIFIER "=" assignment | conditional
    /// ```
    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.conditional()?;

        if let Some(equals) = self.cursor.next_if(|t| matches!(t, Token::Equal)) {
            let value = self.assignment()?;

            return match expr {
                Expr::Variable(variable) => Ok(Assign {
                    name: variable,
                    value: Box::new(value),
                }
                .into()),
                Expr::Get(get) => Ok(Set {
                    object: get.object,
                    name: get.name,
                    value: Box::new(value),
                }
                .into()),
                _ => Err(ParseError {
                    span: equals.span.clone(),
                    msg: "invalid assignment target".into(),
                }),
            };
        }

        Ok(expr)
    }

    /// ```text
    /// logic_or -> logic_and ( "or" logic_and )*
    /// ```
    fn logic_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logic_and()?;

        while let Some(operator) = self.cursor.next_if_eq(Token::Or) {
            let right = self.logic_and()?;
            expr = Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
            .into();
        }

        Ok(expr)
    }

    /// ```text
    /// logic_and -> equality ( "and" equality )*
    /// ```
    fn logic_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while let Some(operator) = self.cursor.next_if_eq(Token::And) {
            let right = self.equality()?;
            expr = Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
            .into();
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
                left: Box::new(expr),
                operator,
                right: Box::new(right),
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
                left: Box::new(expr),
                operator,
                right: Box::new(right),
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
                left: Box::new(expr),
                operator,
                right: Box::new(right),
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
                left: Box::new(expr),
                operator,
                right: Box::new(right),
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
            Unary {
                operator,
                right: Box::new(right),
            }
            .into()
        } else {
            self.call()?
        };

        Ok(expr)
    }

    /// ```text
    /// call -> primary ( "(" arguments? ")" | "." IDENTIFIER )*
    ///
    /// arguments -> expression ( "," expression )*
    /// ```
    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.cursor.next_if_eq(Token::LeftParen).is_some() {
                expr = self.finish_call(expr)?;
            } else if self.cursor.next_if_eq(Token::Dot).is_some() {
                let name = self
                    .cursor
                    .next_if(|token| matches!(token, Token::Identifier(_)))
                    .ok_or_else(|| ParseError {
                        span: self.cursor.next_span(),
                        msg: "expect property name after '.'".into(),
                    })?;
                expr = Get {
                    object: expr.into(),
                    name,
                }
                .into();
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut arguments = vec![];

        if !self
            .cursor
            .peek()
            .is_some_and(|lex| lex.token == Token::RightParen)
        {
            arguments.push(self.expression()?);
            while self.cursor.next_if_eq(Token::Comma).is_some() {
                if arguments.len() >= 255 {
                    return Err(ParseError {
                        span: self.cursor.next_span(),
                        msg: "Can't have more than 255 arguments".into(),
                    });
                }
                arguments.push(self.expression()?);
            }
        }

        let paren = self
            .cursor
            .next_if_eq(Token::RightParen)
            .ok_or_else(|| ParseError {
                span: self.cursor.next_span(),
                msg: "Expect ')' after arguments".into(),
            })?;

        Ok(Call {
            callee: Box::new(callee),
            paren,
            arguments,
        }
        .into())
    }

    /// ```text
    /// primary -> "true" | "false" | "nil" | "this"
    ///           | NUMBER | STRING | IDENTIFIER | "(" expression ")"
    ///           | LAMBDA
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
                    | Token::Fun
                    | Token::This
            )
        }) else {
            return Err(ParseError {
                span: self.cursor.next_span(),
                msg: "unexpected token".into(),
            });
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
                        span: self.cursor.next_span(),
                        msg: "Expect ')' after expression".into(),
                    })?;
                Grouping {
                    expression: Box::new(expr),
                }
                .into()
            }
            Token::Fun => {
                self.cursor
                    .next_if(|t| matches!(t, Token::LeftParen))
                    .ok_or_else(|| ParseError {
                        span: self.cursor.prev_span(),
                        msg: "Expect '(' before parameters".into(),
                    })?;

                let mut params = vec![];
                if self
                    .cursor
                    .peek()
                    .is_some_and(|lex| lex.token != Token::RightParen)
                {
                    loop {
                        if params.len() >= 255 {
                            return Err(ParseError {
                                span: self.cursor.next_span(),
                                msg: "cannot have more than 255 parameters".into(),
                            });
                        }
                        params.push(
                            self.cursor
                                .next_if(|tk| matches!(tk, Token::Identifier(_)))
                                .ok_or_else(|| ParseError {
                                    span: self.cursor.prev_span(),
                                    msg: "expect parameter name".into(),
                                })?,
                        );
                        if self.cursor.next_if_eq(Token::Comma).is_none() {
                            break;
                        }
                    }
                }

                self.cursor
                    .next_if_eq(Token::RightParen)
                    .ok_or_else(|| ParseError {
                        span: self.cursor.prev_span(),
                        msg: "expected `)` after parameters".into(),
                    })?;

                let left_brace =
                    self.cursor
                        .next_if_eq(Token::LeftBrace)
                        .ok_or_else(|| ParseError {
                            span: self.cursor.prev_span(),
                            msg: "expected `{` before body".into(),
                        })?;

                let body = self.block(left_brace)?;

                Lambda {
                    fun: lex,
                    params,
                    body,
                }
                .into()
            }
            Token::Identifier(_) => Variable { name: lex }.into(),
            Token::This => This { keyword: lex }.into(),
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
            let right = self.expression()?;
            expr = Binary {
                left: Box::new(expr),
                operator: comma,
                right: Box::new(right),
            }
            .into();
        }

        Ok(expr)
    }

    /// ```text
    /// conditional -> logic_or ( "?" expression ":" conditional )?
    /// ```
    fn conditional(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logic_or()?;

        if self.cursor.next_if_eq(Token::Question).is_some() {
            let then = self.expression()?;
            self.cursor
                .next_if_eq(Token::Colon)
                .ok_or_else(|| ParseError {
                    span: self.cursor.next_span(),
                    msg: "expect `:` for ternary expression".into(),
                })?;
            let or_else = self.conditional()?;
            expr = Conditional {
                cond: Box::new(expr),
                then: Box::new(then),
                or_else: Box::new(or_else),
            }
            .into();
        }

        Ok(expr)
    }

    fn break_stmt(&mut self, brk: Lexeme) -> Result<Break, ParseError> {
        self.semicolon()?;
        Ok(Break { token: brk })
    }

    /// ```text
    /// function -> IDENTIFIER "(" parameters? ")" block
    ///
    /// parameters -> IDENTIFIER ( "," IDENTIFIER )*
    /// ```
    fn function(&mut self, kind: &str) -> Result<Function, ParseError> {
        let name = self
            .cursor
            .next_if(|tk| matches!(tk, Token::Identifier(_)))
            .ok_or_else(|| ParseError {
                span: self.cursor.prev_span(),
                msg: format!("expected {kind} name"),
            })?;

        self.cursor
            .next_if_eq(Token::LeftParen)
            .ok_or_else(|| ParseError {
                span: self.cursor.prev_span(),
                msg: format!("expected `(` after {kind} name"),
            })?;

        let mut params = vec![];
        if self
            .cursor
            .peek()
            .is_some_and(|lex| lex.token != Token::RightParen)
        {
            loop {
                if params.len() >= 255 {
                    return Err(ParseError {
                        span: self.cursor.next_span(),
                        msg: "cannot have more than 255 parameters".into(),
                    });
                }
                params.push(
                    self.cursor
                        .next_if(|tk| matches!(tk, Token::Identifier(_)))
                        .ok_or_else(|| ParseError {
                            span: self.cursor.prev_span(),
                            msg: "expect parameter name".into(),
                        })?,
                );
                if self.cursor.next_if_eq(Token::Comma).is_none() {
                    break;
                }
            }
        }

        self.cursor
            .next_if_eq(Token::RightParen)
            .ok_or_else(|| ParseError {
                span: self.cursor.prev_span(),
                msg: format!("expected `)` after {kind} parameters"),
            })?;

        let left_brace = self
            .cursor
            .next_if_eq(Token::LeftBrace)
            .ok_or_else(|| ParseError {
                span: self.cursor.prev_span(),
                msg: format!("expected `{{` before {kind} body"),
            })?;

        let body = self.block(left_brace)?;

        Ok(Function { name, params, body })
    }

    fn return_stmt(&mut self, rt: Lexeme) -> Result<Return, ParseError> {
        if self.cursor.next_if_eq(Token::Semicolon).is_some() {
            return Ok(Return {
                keyword: rt,
                expr: None,
            });
        }

        let expr = self.expression()?;
        self.semicolon()?;

        Ok(Return {
            keyword: rt,
            expr: Some(expr),
        })
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

    fn next_if_eq(&mut self, token: Token) -> Option<Lexeme> {
        self.next_if(|t| *t == token)
    }

    fn prev_span(&self) -> Span {
        self.lexemes[self.current - 1].span.clone()
    }

    fn next_span(&self) -> Span {
        self.lexemes
            .get(self.current)
            .map(|lex| &lex.span)
            .unwrap_or(&self.eof.span)
            .clone()
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
    pub span: Span,
    pub msg: String,
}

fn is_statement_begin(token: &Token) -> bool {
    {
        matches!(
            token,
            Token::Print
                | Token::Var
                | Token::LeftBrace
                | Token::While
                | Token::If
                | Token::For
                | Token::Break
                | Token::Fun
                | Token::Return
        )
    }
}
