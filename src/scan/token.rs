use std::mem;

use smol_str::SmolStr;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lexeme {
    pub token: Token,
    pub span: Span,
}

impl Lexeme {
    pub fn ident(&self) -> &str {
        match &self.token {
            Token::Identifier(name) => name,
            Token::This => "this",
            _ => panic!("expected Identifier or This"),
        }
    }
}

/// ```text
/// NUMBER      -> DIGIT+ ( "." DIGIT+ )? ;
/// STRING      -> '"' <any char except '"'>* '"' ;
/// IDENTIFIER  -> ALPHA ( ALPHA | DIGIT )* ;
/// ALPHA       -> "a" ... "z" | "A" ... "Z" | "_" ;
/// DIGIT       -> "0" ... "9" ;
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `{`
    LeftBrace,
    /// `}`
    RightBrace,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
    /// `/`
    Slash,
    /// `*`
    Star,
    /// `!`
    Bang,
    /// `?`
    Question,
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
    /// `break`
    Break,
    /// End of file
    Eof,
}

impl Eq for Token {}

impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            Token::Identifier(s) => s.hash(state),
            Token::String(s) => s.hash(state),
            Token::Number(x) => x.to_bits().hash(state),
            _ => (),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Colon => ":",
            Token::Semicolon => ";",
            Token::Slash => "/",
            Token::Star => "*",
            Token::Bang => "!",
            Token::Question => "?",
            Token::BangEqual => "!=",
            Token::Equal => "=",
            Token::EqualEqual => "==",
            Token::Greater => ">",
            Token::GreaterEqual => ">=",
            Token::Less => "<",
            Token::LessEqual => "<=",
            Token::Identifier(s) => s.as_str(),
            Token::String(s) => return write!(f, r#""{s}""#),
            Token::Number(x) => return write!(f, "{x}"),
            Token::And => "and",
            Token::Class => "class",
            Token::Else => "else",
            Token::False => "false",
            Token::Fun => "fun",
            Token::For => "for",
            Token::If => "if",
            Token::Nil => "nil",
            Token::Or => "or",
            Token::Print => "print",
            Token::Return => "return",
            Token::Super => "super",
            Token::This => "this",
            Token::True => "true",
            Token::Var => "var",
            Token::While => "while",
            Token::Break => "break",
            Token::Eof => "<EOF>",
        };

        f.write_str(s)
    }
}
