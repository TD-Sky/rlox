use std::iter::Peekable;
use std::ops::Range;
use std::str::CharIndices;

use smol_str::SmolStr;

#[derive(Debug)]
pub struct Scanner<'a> {
    source: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    cursor: Range<usize>,
    line: usize,
    len: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.char_indices().peekable(),
            tokens: vec![],
            cursor: 0..0,
            line: 1,
            len: source.len(),
        }
    }

    pub fn scan(mut self) -> Result<Vec<Token>, ScanError> {
        while !self.is_end() {
            self.cursor.start = self.cursor.end;
            self.scan_token()?;
        }

        self.tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span::new(self.len..self.len, self.line),
        });

        Ok(self.tokens)
    }
}

impl<'a> Scanner<'a> {
    const fn is_end(&self) -> bool {
        self.cursor.start == self.len
    }

    fn add_token(&mut self, token: TokenKind) {
        self.tokens.push(Token {
            kind: token,
            span: Span::new(self.cursor.clone(), self.line),
        });
    }

    fn scan_token(&mut self) -> Result<(), ScanError> {
        let Some(c) = self.advance() else {
            return Ok(());
        };

        match c {
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            ',' => self.add_token(TokenKind::Comma),
            '.' => self.add_token(TokenKind::Dot),
            '-' => self.add_token(TokenKind::Minus),
            '+' => self.add_token(TokenKind::Plus),
            ';' => self.add_token(TokenKind::Semicolon),
            '*' => self.add_token(TokenKind::Star),
            '!' => {
                let tk = if self.expect_char('=') {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                };
                self.add_token(tk)
            }
            '=' => {
                let tk = if self.expect_char('=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                };
                self.add_token(tk)
            }
            '<' => {
                let tk = if self.expect_char('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                };
                self.add_token(tk)
            }
            '>' => {
                let tk = if self.expect_char('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                };
                self.add_token(tk)
            }
            '/' => {
                if self.expect_char('/') {
                    while self.peek().is_some_and(|c| c != '\n') && !self.is_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Slash)
                }
            }
            ' ' | '\r' | '\t' => (),
            '\n' => self.line += 1,
            _ => {
                return Err(ScanError {
                    offset: self.cursor.start,
                    span: self.cursor.clone(),
                    msg: "unexpected character",
                });
            }
        }

        Ok(())
    }

    fn advance(&mut self) -> Option<char> {
        let (_, c) = self.source.next()?;
        self.cursor.end = self.source.peek().map(|(i, _)| *i).unwrap_or(self.len);
        Some(c)
    }

    fn peek(&mut self) -> Option<char> {
        self.source.peek().map(|(_, c)| *c)
    }

    fn expect_char(&mut self, expected: char) -> bool {
        if self.peek().is_some_and(|c| c == expected) {
            let _ = self.advance().is_some();
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    kind: TokenKind,
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
    pub msg: &'static str,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
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
