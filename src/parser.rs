// src/parser.rs
//
// We're gonna implement the Pratt Parser here .. 
#![allow(unused_parens)]
#![allow(dead_code)]
use miette::{Error, LabeledSpan, WrapErr, Report};
use std::{fmt, Borrow::Cow};
use crate::token_type::{Token, TokenType};
use crate::lexer::{Lexer, Eof};
use thiserror::Error;

/// defines the Abstract Syntax Tree
pub struct Ast;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'de>
{
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(&'de str),
    Super,
    This,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op
{
    MINUS,
    PLUS,
    SLASH,
    STAR,    
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    AND,
    OR,
    IF,
    FOR,
    FUN,
    PRINT,
    RETURN,
    VAR,
    WHILE,
    FIELD,
    CLASS,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Delimiter
{
    Paren,
    Brace,
}

#[derive(Debug, Clone, PartialEq)]
/// holds the variants for the expression?
enum TokenTree<'de> {
    /// represents a single character
    Atom(Atom<'de>),
    /// represents the `head` -> `rest`
    Cons(Op, Vec<TokenTree<'de>>),
    /// for representing the function blocks
    Fun(Atom<'de>, vec<TokenTree<'de>>, TokenTree<'de>),
}

impl fmt::Display for TokenTree<'_> {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        match self {

            TokenTree::Atom(i) => write!(f, "{}", i),
            TokenTree::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

/// defines the Parser Struct
///
/// whole: &'de str,
/// lexer: lexer::Lexer<'de str>,
pub struct Parser<'de>
{
    /// contains the input string
    whole: &'de str,
    /// holds the lexer::Lexer
    lexer: Lexer<'de>, 
}

impl<'de> Parser<'de>
{
    /// creates a new Parser
    ///
    /// input: &'de str
    ///
    /// returns Self
    pub fn new(input: &'de str) -> Self
    {
        Self{
            whole: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error>
    {
        self.parse_within(None, 0)
    }

    pub fn parse_block(&mut self) -> Result<TokenTree<'de>, Error>
    {
        // start the body parsing
        self.lexer.expect(TokenType::LEFT_BRACE, "Expected Left Brace `{`");
        let block = self.parse_within(0);
        self.lexer.expect(TokenType::RIGHT_BRACE, "Expected Right Brace '}'");
        // end of body parsing
        
        Ok(block)
    }

    pub fn parse_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error>
    {

        let lhs = match self.lexer.next() {
            Some(Ok(token)) => return Ok(token),
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                let msg = "parsing left-hand side";
                return Err(e).wrap_err(msg);
            }
        };

        let mut lhs = match lhs {

            // atoms
            Token {
                kind: TokenType::STRING,
                origin,
                ..
            } => TokenTree::Atom(Atom::String(TokenType::unescape(origin))),

            Token {
                kind: TokenType::NUMBER(n),
                origin,
                ..
            } => TokenTree::Atom(Atom::Number(n)),

            Token {
                kind: TokenType::FALSE,
                ..
            } => TokenTree::Atom(Atom::Bool(true)),

           Token {
                kind: TokenType::NIL,
                ..
            } => TokenTree::Atom(Atom::Nil),

            Token {
                kind: TokenType::IDENT,
                origin,
                ..
            } => TokenTree::Atom(Atom::Ident(origin)),

            // groups
            Token {
                kind: TokenType::LEFT_PAREN | TokenType::LEFT_BRACE,
                origin,
                ..
            } => {
                let terminator = match lhs.kind {
                    TokenType::LEFT_PAREN => TokenType::RIGHT_PAREN,
                    TokenType::LEFT_BRACE => TokenType::RIGHT_BRACE,
                    _ => unreachable!("by outer match arm pattern"),
                };

                let lhs = self.parse_within(0).wrap_err("in bracketed expression")?;

                self.lexer.expect(terminator, "Unexpected End to Bracketed Expression").wrap_err("after bracketed expression")?;

                lhs
            }            

            // unary prefix expressions
            Token { kind: TokenType::BANG | TokenType::PRINT | TokenType::MINUS | TokenType::RETURN, ..} => {

                let op = match lhs.kind {

                    TokenType::MINUS => Op::MINUS,
                    TokenType::BANG => Op::BANG,
                    TokenType::PRINT => Op::PRINT,
                    TokenType::RETURN => Op::RETURN,
                    _ => unreachable!("by outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_within(r_bp).wrap_err("in right-hand side")?;

                TokenTree::Cons(op, vec![rhs])
            }


            Token { kind: TokenType::CLASS | TokenType::VAR, ..} => 
            {         
 
                let op = match lhs.kind {
                    TokenType::CLASS => Op::CLASS,
                    TokenType::VAR => Op::VAR,
                    - => unreachable!("by the outer match arm pattern"),
                };

                let token = self.lexer.expect(TokenType::IDENT, "expected Identifier").wrap_err("in class name declaration")?;
                // we know that this is an IDENT
                assert_eq!(token.kind, TokenType::IDENT);
                
                let ident = TokenTree::Atom(Atom::Ident(token.origin));

                if lhs.kind == TokenType::VAR {

                    // the next one must be an equal sign
                    // the equal sign is not really an argument here .. just a thing we require in the parsing
                    self.lexer.expect(TokenType::EQUAL, "Missing =").wrap_err("in variable assignment")?;

                }

                let second = self.parse_within(0).wrap_err_with("in second argument of {op:?}"))?;

                TokenTree::Cons(op, vec![ident, second])
            }


            Token { kind: TokenType::FUN, ..} => {         
    
                let token = self.lexer.expect(TokenType::IDENT, "expected Identifier").wrap_err_with(|| format!("in function name declaration"));

                let name = token.origin;
                let ident = Atom::Ident(token.origin);

                let mut parameters = Vec::new();

                self.lexer.expect(TokenType::LEFT_PAREN, "Missing LEFT_PAREN `(`").wrap_err_with(|| format!("in function parameter list: {name}"))?;


                // check for immediate parameter list end
                if matches!(self.lexer.peek(), Some(Token{kind: TokenType::RIGHT_PAREN, ..}))
                {
                    // immediate parameter list end
                }
                else
                {
                    loop {

                        let parameter = self.parse_within(0).wrap_err_with(|| format!("in parameter #{} of function {name}", parameters.len() + 1))?;

                        let token = self.lexer.expect_where(|token| matches!(token.kind, TokenType::RIGHT_PAREN | TokenType::COMMA), "continuing paramter list")
                            .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                        if token.kind == TokenType::RIGHT_PAREN {
                            break;
                        }
                    }
                }

                let block = self.parse_block().wrap_err_with(|| format!("in body of function {name}"))?;

                TokenTree::Fun(ident, parameters, block)
            }

            // handle FOR separately
            Token { kind: TokenType::FOR, ..} => {

                // start the header parsing
                self.lexer.expect(TokenType::LEFT_PAREN, "Expected Left Paren `(`").wrap_err("in for loop")?;

                let init = self.parse_within(0).wrap_err("in initialization of for loop");
                self.lexer.expect(TokenType::SEMICOLON, "Expected ; after initialization in for loop");

                let cond = self.parse_within(0).wrap_err("in condition of for loop");
                self.lexer.expect(TokenType::SEMICOLON, "Expected ; after condition in for loop");

                let upd = self.parse_within(0).wrap_err("in update of for loop");

                self.lexer.expect(TokenType::RIGHT_PAREN, "Expected Left Paren `)`").wrap_err("in for loop")?;
                // end of header parsing

                let block = self.parse_block().wrap_err("in body of FOR loop")?;

                // return the tokentree made form this block
                TokenTree::Cons(Op::WHILE, vec![init, cond, upd, block])

            }

            // handle WHILE separately
            Token { kind: TokenType::WHILE, ..} => {

                // start the header parsing
                self.lexer.expect(TokenType::LEFT_PAREN, "Expected Left Paren `(`").wrap_err("in while loop condition")?;
                let cond = self.parse_within(0).wrap_err("in condition of while loop");
                self.lexer.expect(TokenType::RIGHT_PAREN, "Expected Left Paren `)`").wrap_err("in while loop condition")?;
                // end of header parsing

                let block = self.parse_block().wrap_err("in body of WHILE loop")?;
                
                // return the tokentree made form this block
                TokenTree::Cons(Op::WHILE, vec![cond, block])

            }

            // handling IF statements
            Token { kind: TokenType::IF, ..} => {

                // start the header parsing
                self.lexer.expect(TokenType::LEFT_PAREN, "Expected Left Paren `(`").wrap_err("in while loop condition")?;
                let cond = self.parse_within(0).wrap_err("in condition of if statement");
                self.lexer.expect(TokenType::RIGHT_PAREN, "Expected Left Paren `)`").wrap_err("in while loop condition")?;
                // end of header parsing

                let block = self.parse_block().wrap_err("in body of IF statement")?;

                // return the tokentree made form this block
                TokenTree::Cons(Op::WHILE, vec![cond, block])

            }


            t => panic!("bad token: {:?}", t),
        };


        loop {

            let op = self.lexer.peek();

            if op.map_or(false, |op| op.is_err()) {
                return Err(self.lexer.next().expect("Checked Some above").expect_err("Checked Err above")).wrap_err_with(looking_for_msg);
            }

            let op = match op.map(|res| res.expect("handled err above") {
                None => break,
                Some(Token{
                    // here we need to look for infix and postfix operators specifically
                    kind: TokenType::LEFT_PAREN |
                        | TokenType::BANG
                        | TokenType::BANG_EQUAL
                        | TokenType::EQUAL
                        | TokenType::EQUAL_EQUAL
                        | TokenType::GREATER
                        | TokenType::GREATER_EQUAL
                        | TokenType::LESS
                        | TokenType::SLASH
                        | TokenType::MINUS
                        | TokenType::DOT
                        | TokenType::PLUS
                        | TokenType::STAR
                        | TokenType::AND
                        | TokenType::LESS_EQUAL,
                       ..
                }) => op,

                t => panic!("bad token: {:?}", t),

            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {

                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                lhs = if op == '[' {
                    let rhs = self.parse_within(0);
                    assert_eq!(self.lexer.next(), TokenTP::Op(']'));
                    TokenTree::Cons(op, vec![lhs, rhs])

                } else {
                    TokenTree::Cons(op, vec![lhs])
                };          

                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {

                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                lhs = if op == '?' {
                    let mhs = self.parse_within(0);
                    assert_eq!(self.lexer.next(), TokenTP::Op(':'));
                    let rhs = self.parse_within(r_bp);
                    TokenTree::Cons(op, vec![lhs, mhs, rhs])
                } else {
                    let rhs = self.parse_within(r_bp);
                    TokenTree::Cons(op, vec![lhs, rhs])
                };

                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

fn prefix_binding_power(op: char) -> ((), u8) { 
    match op {
        Op::BANG | Op::RETURN | Op::MINUS | Op::PRINT => ((), 9),
        _ => panic!("bad op: {:?}", op),
    }
}

fn infix_binding_power(op: char) -> (u8, u8) {

    let res = match op {

        '+' | '-' => (5, 6),
        '*' | '/' => (7, 8),
        '.' => (14, 13),
        '=' => (2, 1),
        '?' => (4, 3),
        _ => return None, 
    };

    return Some(res);
}

fn postfix_binding_power(op: char) -> Option<(u8, ())> {
    let res = match op {
        '!' => (11, ()), // for factorials
        '[' => (11, ()),
        _ => return None,
    };
    Some(res)
}
