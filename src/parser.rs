// src/parser.rs
//
// We're gonna implement the Pratt Parser here .. 
#![allow(unused_parens)]
#![allow(dead_code)]
use miette::{Error, LabeledSpan, WrapErr};
use std::{fmt, borrow::Cow};
use crate::token_type::{Token, TokenType};
use crate::lexer::{Lexer, Eof};

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

#[allow(non_camel_case_types)]
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
    CALL,
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
    Fun(Atom<'de>, Vec<Token<'de>>, TokenTree<'de>),
    /// for representing IF statements
    If(TokenTree<'de>, TokenTree<'de>, Option<TokenTree<'de>>),
    /// for representing Function calls
    Call(TokenTree<'de>, Vec<TokenTree<'de>>),
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
        // TODO: In a loop
        self.parse_statement_within(0)
    }

    pub fn parse_block(&mut self) -> Result<TokenTree<'de>, Error>
    {
        // start the body parsing
        self.lexer.expect(TokenType::LEFT_BRACE, "Expected Left Brace `{`");
        // TODO: In a loop with semicolons? .. depends on class vs body
        let block = self.parse_statement_within(0)?;
        self.lexer.expect(TokenType::RIGHT_BRACE, "Expected Right Brace '}'");
        // end of body parsing
        
        Ok(block)
    }

    pub fn parse_expression_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error>
    {

        let lhs = match self.lexer.next() {
            Some(Ok(token)) => Ok(token),
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
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
                kind: TokenType::LEFT_PAREN,
                origin,
                ..
            } => {

                let lhs = self.parse_expression_within(0).wrap_err("in bracketed expression")?;

                self.lexer.expect(TokenType::RIGHT_PAREN, "Unexpected end to bracketed Expression").wrap_err("after bracketed expression")?;

                lhs
            }

            // unary prefix expressions
            Token { kind: TokenType::BANG | TokenType::MINUS, ..} => {

                let op = match lhs.kind {

                    TokenType::MINUS => Op::MINUS,
                    TokenType::BANG => Op::BANG,
                    _ => unreachable!("by outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression_within(r_bp).wrap_err("in right-hand side")?;

                TokenTree::Cons(op, vec![rhs])
            }

            Token { kind: TokenType::VAR, ..} => {

                // VAR takes an expression

                let token = self.lexer.expect(TokenType::IDENT, "expected Identifier").wrap_err("in variable assignment")?;
                
                assert_eq!(token.kind, TokenType::IDENT);
                
                let ident = TokenTree::Atom(Atom::Ident(token.origin));

                // the next one must be an equal sign
                // the equal sign is not really an argument here .. just a thing we require in the parsing
                self.lexer.expect(TokenType::EQUAL, "Missing =").wrap_err("in variable assignment")?;

                let second = self.parse_expression_within(0).wrap_err("in variable assignment expression")?;

                TokenTree::Cons(Op::VAR, vec![ident, second])
            }

            Token { kind: TokenType::CLASS, ..} => 
            {         
                // CLASS takes a block

                let token = self.lexer.expect(TokenType::IDENT, "expected Identifier").wrap_err("in class name")?;
                // we know that this is an IDENT
                assert_eq!(token.kind, TokenType::IDENT);
                
                let ident = TokenTree::Atom(Atom::Ident(token.origin));

                let block = self.parse_within(0).wrap_err("in class definition body");

                TokenTree::Cons(Op::CLASS, vec![ident, block])
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

                        let parameter = self.lexer.expect(TokenType::IDENT, "Unexpected Token")
                            .wrap_err_with(|| format!("in parameter #{} of function {name}", parameters.len() + 1))?;

                        parameters.push(parameter);

                        let token = self.lexer.expect_where(|token| matches!(token.kind, TokenType::RIGHT_PAREN | TokenType::COMMA), "continuing paramter list")
                            .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                        if token.kind == TokenType::RIGHT_PAREN {
                            break;
                        }
                    }
                }

                let block = self.parse_block().wrap_err_with(|| format!("in body of function {name}"))?;

                TokenTree::Fun(ident, parameter<M-D-Space><M-D-Space>s, block)
            }

            // handle FOR separately
            Token { kind: TokenType::FOR, ..} => {

                // start the header parsing
                self.lexer.expect(TokenType::LEFT_PAREN, "Expected Left Paren `(`").wrap_err("in for loop")?;

                let init = self.parse_within(0).wrap_err("in initialization of for loop");
                self.lexer.expect(TokenType::SEMICOLON, "Expected ; after initialization in for loop");

                let cond = self.parse_expression_within(0).wrap_err("in condition of for loop");
                self.lexer.expect(TokenType::SEMICOLON, "Expected ; after condition in for loop");

                let upd = self.parse_within(0).wrap_err("in update of for loop");

                self.lexer.expect(TokenType::RIGHT_PAREN, "Expected Left Paren `)`").wrap_err("in for loop")?;
                // end of header parsing

                let block = self.parse_block().wrap_err("in body of FOR loop")?;

                // return the tokentree made form this block
                TokenTree::Cons(Op::FOR, vec![init, cond, upd, block])

            }

            // handle WHILE separately
            Token { kind: TokenType::WHILE, ..} => {

                // start the header parsing
                self.lexer.expect(TokenType::LEFT_PAREN, "Expected Left Paren `(`").wrap_err("in while loop condition")?;
                let cond = self.parse_expression_within(0).wrap_err("in condition of while loop");
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

                // Do we have an ELSE?
                let mut otherwise = None;

                if matches!(self.lexer.peek(), Some(Ok(Token{
                    kind: TokenType::ELSE, 
                    ..
                })))
                {
                    self.lexer.next()?;

                    let otherwise = self.parse_block().wrap_err("in body of ELSE statement");
                }

                // return the tokentree made form this block
                TokenTree::If(cond, block, otherwise)

            }


            t => panic!("bad token: {:?}", t),
        };


        loop {

            let op = self.lexer.peek();

            if op.map_or(false, |op| op.is_err()) {
                
                return Err(self.lexer.next().expect("Checked Some above").expect_err("Checked Err above")).wrap_err_with("in place of expected operator");
            }

            let op = match op.map(|res| res.as_ref().expect("handled Err above")) {

                None => break,

                Some(Token{
                    kind: TokenType::LEFT_PAREN
                       ..
                    }) => Op::LESS_EQUAL,
                Some(Token{
                    kind: TokenType::BANG
                       ..
                    }) => Op::BANG,

                Some(Token{
                    kind: TokenType::BANG_EQUAL,
                    ..
                    }) => Op::BANG_EQUAL,

                Some(Token{
                    kind: TokenType::EQUAL,
                    ..
                    }) => Op::EQUAL,

                Some(Token{
                    kind: TokenType::EQUAL_EQUAL,
                    ..
                    }) => Op::EQUAL_EQUAL,

                Some(Token{
                    kind: TokenType::GREATER,
                    ..
                    }) => Op::LESS_EQUAL,

                Some(Token{
                    kind: TokenType::GREATER_EQUAL,
                    ..
                    }) => Op::GREATER_EQUAL,

                Some(Token{
                    kind: TokenType::LESS,
                    ..
                    }) => Op::LESS,

                Some(Token{
                    kind: TokenType::SLASH,
                    ..
                    }) => Op::SLASH,

                Some(Token{
                    kind: TokenType::MINUS,
                    ..
                    }) => Op::MINUS,

                Some(Token{
                    kind: TokenType::DOT,
                    ..
                    }) => Op::FIELD,

                Some(Token{
                    kind: TokenType::PLUS,
                    ..
                    }) => Op::PLUS,

                Some(Token{
                    kind: TokenType::STAR,
                    ..
                    }) => Op::STAR,

                Some(Token{
                    kind: TokenType::OR,
                    ..
                    }) => Op::OR,

               Some(Token{
                    kind: TokenType::AND,
                    ..
                    }) => Op::AND,

                Some(Token{
                    kind: TokenType::LESS_EQUAL,
                    ..
                    }) => Op::LESS_EQUAL,

                    t => panic!("bad token: {:?}", t),
                })
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {

                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                lhs = if Op::CALL {
                    let mut arguments = Vec::new();

                    self.lexer
                        .expect(TokenType::LEFT_PAREN, "missing '('")
                        .wrap_err("in argument list of function call");

                    if matches!(
                        self.lexer.peek(),
                        Some(Ok(Token {
                            kind: TokenType::RIGHT_PAREN,
                            ..
                        }))
                    ) {
                        // immediate argument list end
                        // THEY ARE ARGUMENTS WHEN YOU ARE CALLING
                    } else
                    {
                        loop {
                            
                            let argument = self.parse_expression_within(0).wrap_err_with(|| {
                                format!("in argument #{} of function call", arguments.len() + 1)
                            })?;

                            arguments.push(argument);

                            let token = self.lexer.expect_where(
                                |token| {
                                    matches!(token.kind, TokenType::RIGHT_PAREN | TokenType::COMMA)
                                },

                                "continuing argument list"
                            ).wrap_err("in argument list of function call")?;

                            if token.kind == TokenType::RIGHT_PAREN {
                                break;
                            }

                        }
                    }

                    TokenTree::Call(lhs, arguments) // lhs is the function to call
                }         

                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op.take()) {

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


    pub fn parse_statement_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error>
    {

        let lhs = match self.lexer.next() {
            Some(Ok(token)) => Ok(token),
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
            }
        };

        let mut lhs = match lhs {

            Token {
                kind: TokenType::IDENT,
                origin,
                ..
            } => TokenTree::Atom(Atom::Ident(origin)),

            Token {
                kind: TokenType::LEFT_PAREN | TokenType::LEFT_BRACE,
                origin,
                ..
            } => {

                let lhs = self.parse_expression_within(0).wrap_err("in bracketed expression")?;

                self.lexer.expect(TokenType::RIGHT_PAREN, "Unexpected End to Bracketed Expression").wrap_err("after bracketed expression")?;

                lhs
            } 

            Token { kind: TokenType::PRINT | TokenType::RETURN, ..} => {

                let op = match lhs.kind {

                    TokenType::PRINT => Op::PRINT,
                    TokenType::RETURN => Op::RETURN,
                    _ => unreachable!("by outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression_within(r_bp).wrap_err("in right-hand side")?;

                return Ok(TokenTree::Cons(op, vec![rhs]))
            }


            Token { kind: TokenType::CLASS | TokenType::VAR, ..} => 
            {         

                let op = match lhs.kind {
                    TokenType::CLASS => Op::CLASS,
                    TokenType::VAR => Op::VAR,
                    _ => unreachable!("by the outer match arm pattern"),
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

                let second = self.parse_within(0).wrap_err_with("in second argument of {op:?}")?;

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

                return Ok(TokenTree::Fun(ident, parameters, block))
            }

            // handle FOR separately
            Token { kind: TokenType::FOR, ..} => {

                // start the header parsing
                self.lexer.expect(TokenType::LEFT_PAREN, "Expected Left Paren `(`").wrap_err("in for loop")?;

                let init = self.parse_expression_within(0).wrap_err("in initialization of for loop");
                self.lexer.expect(TokenType::SEMICOLON, "Expected ; after initialization in for loop");

                let cond = self.parse_expression_within(0).wrap_err("in condition of for loop");
                self.lexer.expect(TokenType::SEMICOLON, "Expected ; after condition in for loop");

                let upd = self.parse_expression_within(0).wrap_err("in update of for loop");

                self.lexer.expect(TokenType::RIGHT_PAREN, "Expected Left Paren `)`").wrap_err("in for loop")?;
                // end of header parsing

                let block = self.parse_block().wrap_err("in body of FOR loop")?;

                // return the tokentree made form this block
                return Ok(TokenTree::Cons(Op::FOR, vec![init, cond, upd, block]))

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
                return Ok(TokenTree::Cons(Op::WHILE, vec![cond, block]))

            }

            // handling IF statements
            Token { kind: TokenType::IF, ..} => {

                // start the header parsing
                self.lexer.expect(TokenType::LEFT_PAREN, "Expected Left Paren `(`").wrap_err("in while loop condition")?;
                let cond = self.parse_within(0).wrap_err("in condition of if statement");
                self.lexer.expect(TokenType::RIGHT_PAREN, "Expected Left Paren `)`").wrap_err("in while loop condition")?;
                // end of header parsing

                let block = self.parse_block().wrap_err("in body of IF statement")?;

                // Do we have an ELSE?
                let mut otherwise = None;

                if matches!(self.lexer.peek(), Some(Ok(Token{
                    kind: TokenType::ELSE, 
                    ..
                })))
                {
                    self.lexer.next()?;

                    let otherwise = self.parse_block().wrap_err("in body of ELSE statement");
                }

                // return the tokentree made form this block
                return Ok(TokenTree::If(cond, block, otherwise))

            }

            token => return Err(miette::miette!{
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset+self.origin.len(), "here")
                    ],

                    help = format!("unexpected {token:?}")),
                    "Expected a statement",
                }.with_source_code(self.whole.to_string()));


        };


        loop {

            let op = self.lexer.peek();

            if op.map_or(false, |op| op.is_err()) {
                return Err(self.lexer.next().expect("checked Some above").expect_err("Checked Err above")).wrap_err("in place of expected operator");
            }

            let op = match op.map(|res| res.as_ref().expect("handled err above")) {

                None => break,

                Some(Token{
                    // here we need to look for infix and postfix operators specifically
                    kind: TokenType::DOT,
                    ..
                }) => Op::CALL,

                Some(Token{
                    // here we need to look for infix and postfix operators specifically
                    kind: TokenType::LEFT_PAREN,
                    ..
                }) => Op::FIELD,


                t => panic!("bad token: {:?}", t),

                });

            if let Some((l_bp, ())) = postfix_binding_power(op) {

                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                lhs = if Op::CALL {
                    let mut arguments = Vec::new();

                    self.lexer
                        .expect(TokenType::LEFT_PAREN, "missing '('")
                        .wrap_err("in argument list of function call");

                    if matches!(
                        self.lexer.peek(),
                        Some(Ok(Token {
                            kind: TokenType::RIGHT_PAREN,
                            ..
                        }))
                    ) {
                        // immediate argument list end
                        // THEY ARE ARGUMENTS WHEN YOU ARE CALLING
                    } else
                    {
                        loop {
                            
                            let argument = self.parse_expression_within(0).wrap_err_with(|| {
                                format!("in argument #{} of function call", arguments.len() + 1)
                            })?;

                            arguments.push(argument);

                            let token = self.lexer.expect_where(
                                |token| {
                                    matches!(token.kind, TokenType::RIGHT_PAREN | TokenType::COMMA)
                                },

                                "continuing argument list"
                            ).wrap_err("in argument list of function call")?;

                            if token.kind == TokenType::RIGHT_PAREN {
                                break;
                            }

                        }
                    }
         

                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {

                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                let rhs = self.parse_expression_within(r_bp).wrap_err("on the right hand side")?;
                lhs = TokenTree::Cons(op, vec![lhs, rhs])

                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

fn prefix_binding_power(op: Op) -> Option<((), u8)> { 
    match op {
        Op::BANG | Op::RETURN | Op::MINUS | Op::PRINT => ((), 9),
        _ => panic!("bad op: {:?}", op),
    }
}

fn infix_binding_power(op: Op) -> Option<(u8, u8)> {

    let res = match op {

        Op::PLUS | Op::MINUS => (5, 6),
        Op::STAR | Op::SLASH => (7, 8),
        Op::FIELD => (14, 13),
        Op::EQUAL => (2, 1),
        // '?' => (4, 3),
        _ => return None, 
    };

    return Some(res);
}

fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    let res = match op {
        Op::CALL => (11, ()),
        _ => return None,
    };
    Some(res)
}
