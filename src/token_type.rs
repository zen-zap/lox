use std::borrow::Cow;
use std::fmt::{self};


pub struct Token<'de>
{
    /// holds the characters as &str
    pub origin: &'de str,
    /// holds the type
    pub kind: TokenType,
}

/// The `TokenType` enum represents the different types of tokens that can be recognized by the lexer.
/// Each variant corresponds to a specific type of token, such as keywords, literals, or punctuation.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENT,
    STRING,
    NUMBER(f64),

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
}

impl TokenType {
    /// This function is a placeholder for unescaping string literals.
    /// It takes a string slice and returns a `Cow` (Clone on Write) of the unescaped string.
    /// The `Cow` type allows for efficient handling of borrowed and owned data.
    pub fn unescape<'de>(_s: &'de str) -> Cow<'de, str> {
        todo!()
    }
}

impl fmt::Display for Token<'_> {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        let origin = self.origin;
        match self.kind {
            // Single-character tokens.
            TokenType::LEFT_PAREN => write!(f, "LEFT_PAREN {origin} null"),
            TokenType::RIGHT_PAREN => write!(f, "RIGHT_PAREN {origin} null"),
            TokenType::LEFT_BRACE => write!(f, "LEFT_BRACE {origin} null"),
            TokenType::RIGHT_BRACE => write!(f, "RIGHT_BRACE {origin} null"),
            TokenType::COMMA => write!(f, "COMMA {origin} null"),
            TokenType::DOT => write!(f, "DOT {origin} null"),
            TokenType::MINUS => write!(f, "MINUS {origin} null"),
            TokenType::PLUS => write!(f, "PLUS {origin} null"),
            TokenType::SEMICOLON => write!(f, "SEMICOLON {origin} null"),
            TokenType::SLASH => write!(f, "SLASH {origin} null"),
            TokenType::STAR => write!(f, "STAR {origin} null"),

            // One or two character tokens.
            TokenType::BANG => write!(f, "BANG {origin} null"),
            TokenType::BANG_EQUAL => write!(f, "BANG_EQUAL {origin} null"),
            TokenType::EQUAL => write!(f, "EQUAL {origin} null"),
            TokenType::EQUAL_EQUAL => write!(f, "EQUAL_EQUAL {origin} null"),
            TokenType::GREATER => write!(f, "GREATER {origin} null"),
            TokenType::GREATER_EQUAL => write!(f, "GREATER_EQUAL {origin} null"),
            TokenType::LESS => write!(f, "LESS {origin} null"),
            TokenType::LESS_EQUAL => write!(f, "LESS_EQUAL {origin} null"),

            // Literals.
            TokenType::IDENT => write!(f, "IDENTIFIER {origin} null"),
            TokenType::STRING => write!(f, "STRING {origin} {}", TokenType::unescape(origin)),
            TokenType::NUMBER(n) => write!(f, "NUMBER {origin} {n}"), // literal and float point number

            // Keywords.
            TokenType::AND => write!(f, "AND {origin} null"),
            TokenType::CLASS => write!(f, "CLASS {origin} null"),
            TokenType::ELSE => write!(f, "ELSE {origin} null"),
            TokenType::FALSE => write!(f, "FALSE {origin} null"),
            TokenType::FUN => write!(f, "FUN {origin} null"),
            TokenType::FOR => write!(f, "FOR {origin} null"),
            TokenType::IF => write!(f, "IF {origin} null"),
            TokenType::NIL => write!(f, "NIL {origin} null"),
            TokenType::OR => write!(f, "OR {origin} null"),
            TokenType::PRINT => write!(f, "PRINT {origin} null"),
            TokenType::RETURN => write!(f, "RETURN {origin} null"),
            TokenType::SUPER => write!(f, "SUPER {origin} null"),
            TokenType::THIS => write!(f, "THIS {origin} null"),
            TokenType::TRUE => write!(f, "TRUE {origin} null"),
            TokenType::VAR => write!(f, "VAR {origin} null"),
            TokenType::WHILE => write!(f, "WHILE {origin} null"),

            // End-of-file token.
            TokenType::EOF => write!(f, "EOF null null"),
        }
    }
}
