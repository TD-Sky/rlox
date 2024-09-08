use std::iter::Peekable;
use std::ops::Range;
use std::str::CharIndices;

use smol_str::SmolStr;

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    tokens: Vec<Lexeme>,
    cursor: Range<usize>,
    line: usize,
    len: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            tokens: vec![],
            cursor: 0..0,
            line: 1,
            len: source.len(),
        }
    }

    pub fn scan(mut self) -> Result<Vec<Lexeme>, ScanError> {
        while !self.is_end() {
            self.cursor.start = self.cursor.end;
            self.scan_token()?;
        }

        self.tokens.push(Lexeme {
            token: Token::Eof,
            span: Span::new(self.len..self.len, self.line),
        });

        Ok(self.tokens)
    }
}

impl<'a> Scanner<'a> {
    const fn is_end(&self) -> bool {
        self.cursor.start == self.len
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(Lexeme {
            token,
            span: Span::new(self.cursor.clone(), self.line),
        });
    }

    fn scan_token(&mut self) -> Result<(), ScanError> {
        let Some(c) = self.advance() else {
            return Ok(());
        };

        match c {
            '(' => self.add_token(Token::LeftParen),
            ')' => self.add_token(Token::RightParen),
            '{' => self.add_token(Token::LeftBrace),
            '}' => self.add_token(Token::RightBrace),
            ',' => self.add_token(Token::Comma),
            '.' => self.add_token(Token::Dot),
            '-' => self.add_token(Token::Minus),
            '+' => self.add_token(Token::Plus),
            ';' => self.add_token(Token::Semicolon),
            '*' => self.add_token(Token::Star),
            '!' => {
                let tk = if self.expect_char('=') {
                    Token::BangEqual
                } else {
                    Token::Bang
                };
                self.add_token(tk)
            }
            '=' => {
                let tk = if self.expect_char('=') {
                    Token::EqualEqual
                } else {
                    Token::Equal
                };
                self.add_token(tk)
            }
            '<' => {
                let tk = if self.expect_char('=') {
                    Token::LessEqual
                } else {
                    Token::Less
                };
                self.add_token(tk)
            }
            '>' => {
                let tk = if self.expect_char('=') {
                    Token::GreaterEqual
                } else {
                    Token::Greater
                };
                self.add_token(tk)
            }
            '/' => {
                if self.expect_char('/') {
                    while self.peek().is_some_and(|c| c != '\n') && !self.is_end() {
                        self.advance();
                    }
                } else if self.expect_char('*') {
                    self.block_comment()?;
                } else {
                    self.add_token(Token::Slash);
                }
            }
            ' ' | '\r' | '\t' => (),
            '\n' => self.line += 1,
            '"' => {
                self.string()?;
            }
            c if c.is_ascii_digit() => {
                self.number()?;
            }
            c if c.is_ascii_alphabetic() => {
                self.identifier_or_keyword();
            }
            _ => {
                return Err(ScanError {
                    offset: self.cursor.start,
                    span: self.cursor.clone(),
                    msg: "unexpected character".into(),
                });
            }
        }

        Ok(())
    }

    fn string(&mut self) -> Result<(), ScanError> {
        loop {
            let Some(c) = self.peek() else {
                return Err(ScanError {
                    offset: self.cursor.start,
                    span: self.cursor.clone(),
                    msg: "unterminated string".into(),
                });
            };

            self.advance().unwrap();

            if c == '"' {
                break;
            }

            if c == '\n' {
                self.line += 1;
            }
        }

        self.add_token(Token::String(self.source[self.cursor.clone()].into()));

        Ok(())
    }

    fn number(&mut self) -> Result<(), ScanError> {
        while self.expect_pred(|c| c.is_ascii_digit()) {}

        if self.chars.peek().is_some_and(|(i, c)| {
            *c == '.'
                && self
                    .source
                    .get(i + 1..)
                    .is_some_and(|s| s.starts_with(|c: char| c.is_ascii_digit()))
        }) {
            let _ = self.advance();

            while self.expect_pred(|c| c.is_ascii_digit()) {}
        }

        let x = self.source[self.cursor.clone()]
            .parse::<f64>()
            .map_err(|e| ScanError {
                offset: self.cursor.start,
                span: self.cursor.clone(),
                msg: e.to_string(),
            })?;
        self.add_token(Token::Number(x));

        Ok(())
    }

    fn identifier_or_keyword(&mut self) {
        while self.expect_pred(|c| c.is_alphanumeric() || c == '_') {}

        let s = &self.source[self.cursor.clone()];

        let token = match s {
            "and" => Token::And,
            "class" => Token::Class,
            "else" => Token::Else,
            "false" => Token::False,
            "for" => Token::For,
            "fun" => Token::Fun,
            "if" => Token::If,
            "nil" => Token::Nil,
            "or" => Token::Or,
            "print" => Token::Print,
            "return" => Token::Return,
            "super" => Token::Super,
            "this" => Token::This,
            "true" => Token::True,
            "var" => Token::Var,
            "while" => Token::While,
            s => Token::Identifier(s.into()),
        };

        self.add_token(token)
    }

    fn block_comment(&mut self) -> Result<(), ScanError> {
        let mut nested = 0;

        loop {
            let Some(c) = self.peek() else {
                return Err(ScanError {
                    offset: self.cursor.start,
                    span: self.cursor.clone(),
                    msg: "unterminated block comment".into(),
                });
            };

            let _ = self.advance();

            if c == '*' && self.expect_char('/') {
                nested -= 1;
                if nested < 0 {
                    return Ok(());
                }
            } else if c == '\n' {
                self.line += 1;
            } else if c == '/' && self.expect_char('*') {
                nested += 1;
            }
        }
    }

    fn advance(&mut self) -> Option<char> {
        let (_, c) = self.chars.next()?;
        self.cursor.end = self.chars.peek().map(|(i, _)| *i).unwrap_or(self.len);
        Some(c)
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn expect_char(&mut self, expected: char) -> bool {
        self.expect_pred(|c| c == expected)
    }

    fn expect_pred<F>(&mut self, pred: F) -> bool
    where
        F: FnOnce(char) -> bool,
    {
        if self.peek().is_some_and(pred) {
            let _ = self.advance().is_some();
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexeme {
    token: Token,
    span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    pub range: Range<usize>,
    pub line: usize,
}

impl Span {
    pub const fn new(range: Range<usize>, line: usize) -> Self {
        Self { range, line }
    }
}

#[derive(Debug)]
pub struct ScanError {
    pub offset: usize,
    pub span: Range<usize>,
    pub msg: String,
}

#[derive(Debug, Clone)]
pub enum Token {
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `[`
    LeftBrace,
    /// `]`
    RightBrace,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `;`
    Semicolon,
    /// `/`
    Slash,
    /// `*`
    Star,
    /// `!`
    Bang,
    /// `!=`
    BangEqual,
    /// `=`
    Equal,
    /// `==`
    EqualEqual,
    /// `>`
    Greater,
    /// `>=`
    GreaterEqual,
    /// `<`
    Less,
    /// `<=`
    LessEqual,
    /// identifier
    Identifier(SmolStr),
    /// string literal
    String(SmolStr),
    /// number literal
    Number(f64),
    /// `and`
    And,
    /// `class`
    Class,
    /// `else`
    Else,
    /// `false`
    False,
    /// `fun`
    Fun,
    /// `for`
    For,
    /// `if`
    If,
    /// `nil`
    Nil,
    /// `or`
    Or,
    /// `print`
    Print,
    /// `return`
    Return,
    /// `super`
    Super,
    /// `this`
    This,
    /// `true`
    True,
    /// `var`
    Var,
    /// `while`
    While,
    /// End of file
    Eof,
}
