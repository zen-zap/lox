// src/parser.rs
//
// We're gonna implement the Pratt Parser here .. 
#![allow(unused_parens)]
#![allow(dead_code)]
use miette::{Error, LabeledSpan, WrapErr};
use std::{fmt, borrow::Cow};
use crate::token_type::{Token, TokenType};
use crate::lexer::{Lexer};

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

impl fmt::Display for Atom<'_> {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        match self {
            // Atom::String(s) => write!(f, "\"{s}\""),
            Atom::String(s) => write!(f, "{s}"),
            Atom::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            Atom::Nil => write!(f, "nil"),
            Atom::Bool(b) => write!(f, "{b:?}"),
            Atom::Ident(i) => write!(f, "{i}"),
            Atom::Super => write!(f, "super"),
            Atom::This => write!(f, "this"),
        }    }
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
    GROUP,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::MINUS => "-",
                Op::PLUS => "+",
                Op::STAR => "*",
                Op::BANG_EQUAL => "!=",
                Op::EQUAL_EQUAL => "==",
                Op::LESS_EQUAL => "<=",
                Op::GREATER_EQUAL => ">=",
                Op::LESS => "<",
                Op::GREATER => ">",
                Op::SLASH => "/",
                Op::BANG => "!",
                Op::AND => "and",
                Op::OR => "or",
                Op::FOR => "for",
                Op::CLASS => "class",
                Op::PRINT => "print",
                Op::RETURN => "return",
                Op::FIELD => ".",
                Op::VAR => "var",
                Op::WHILE => "while",
                Op::CALL => "call",
                Op::GROUP => "group",
                _ => {
                    todo!()
                },
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
/// holds the variants for the expression?
pub enum TokenTree<'de> {
    /// represents a single character
    Atom(Atom<'de>),
    /// represents the `head` -> `rest`
    Cons(Op, Vec<TokenTree<'de>>),
    /// for representing the function blocks
    Fun{
        name: Atom<'de>, 
        parameters: Vec<Token<'de>>, 
        body: Box<TokenTree<'de>>,
    },
    /// for representing IF statements
    If{
        cond: Box<TokenTree<'de>>, 
        yes: Box<TokenTree<'de>>, 
        no: Option<Box<TokenTree<'de>>>
    },
    /// for representing Function calls
    Call{
        callee: Box<TokenTree<'de>>, 
        arguments: Vec<TokenTree<'de>>,
    },
}

impl fmt::Display for TokenTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Atom(i) => write!(f, "{}", i),
            TokenTree::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {s}")?
                }
                write!(f, ")")
            }
            TokenTree::Fun {
                name,
                parameters,
                body,
            } => {
                write!(f, "(def {name}")?;
                for p in parameters {
                    write!(f, " {p}")?
                }
                write!(f, " {body})")
            }
            TokenTree::Call { callee, arguments } => {
                write!(f, "({callee}")?;
                for a in arguments {
                    write!(f, " {a}")?
                }
                write!(f, ")")
            }
            TokenTree::If { cond, yes, no } => {
                write!(f, "(if {cond} {yes}")?;
                if let Some(no) = no {
                    write!(f, " {no}")?
                }
                write!(f, ")")
            }
        }
    }
}/// defines the Parser Struct
///
/// whole: &'de str,
/// lexer: lexer::Lexer<'de str>,
pub struct Parser<'de>
{
    /// contains the input string
    pub whole: &'de str,
    /// holds the lexer::Lexer
    pub lexer: Lexer<'de>,
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
        Self {
            whole: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error>
    {
        // TODO: In a loop
        self.parse_statement_within(0)
    }

    pub fn parse_expression(mut self) -> Result<TokenTree<'de>, Error>
    {
        self.parse_expression_within(0)
    }

    pub fn parse_fun_call_arguments(&mut self) -> Result<Vec<TokenTree<'de>>, Error>
    {
        let mut arguments = Vec::new();

        if matches!(self.lexer.peek(), Some(Ok(Token{
            kind: TokenType::RIGHT_PAREN,
            ..
        }))) {
            // immediate argument list end
        } else {
            loop {
                // parse the arguments
                let mut argument = self
                    .parse_expression_within(0)
                    .wrap_err_with(|| format!("in argument #{} of function call", arguments.len() + 1))?;

                arguments.push(argument);

                let token = self.lexer.expect_where(
                    |token| 
                    matches!(token.kind, TokenType::RIGHT_PAREN | TokenType::COMMA), 
                    "continuing argument list",
                ).wrap_err("in argument list of function call")?;

                if token.kind == TokenType::RIGHT_PAREN
                {
                    break;
                }
            }
        }

        Ok(arguments)
    }

    pub fn parse_block(&mut self) -> Result<TokenTree<'de>, Error>
    {
        self.lexer.expect(TokenType::LEFT_BRACE, "missing {")?;
        let block = self.parse_statement_within(0)?;
        self.lexer.expect(TokenType::RIGHT_BRACE, "missing }")?;

        // hmm here .. how would you actually read the different statements ? ... mmmm ... like
        // separating the lines by semicolons? ... TODO

        Ok(block)
    }

    pub fn parse_expression_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error>
    {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
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
            } => TokenTree::Atom(Atom::String(Token::unescape(origin))),

            Token {
                kind: TokenType::NUMBER(n),
                origin,
                ..
            } => TokenTree::Atom(Atom::Number(n)),

            Token {
                kind: TokenType::FALSE,
                ..
            } => TokenTree::Atom(Atom::Bool(false)),

            Token {
                kind: TokenType::TRUE,
                ..
            } => TokenTree::Atom(Atom::Bool(true)),

            Token {
                kind: TokenType::NIL,
                ..
            } => TokenTree::Atom(Atom::Nil),

            Token {
                kind: TokenType::SUPER,
                ..
            } => TokenTree::Atom(Atom::Super),

            Token {
                kind: TokenType::THIS,
                ..
            } => TokenTree::Atom(Atom::This),

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

                TokenTree::Cons(Op::GROUP, vec![lhs])
            }

            // unary prefix expressions
            Token { 
                kind: TokenType::BANG | TokenType::MINUS, 
                ..
            } => {
                let op = match lhs.kind {
                    TokenType::MINUS => Op::MINUS,
                    TokenType::BANG => Op::BANG,
                    _ => unreachable!("by outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression_within(r_bp).wrap_err("in right-hand side")?;

                TokenTree::Cons(op, vec![rhs])
            }

            token => return Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here"),
                ],
                help = format!("Unexpected {token:?}"),
                "Expected an expression",
            }.with_source_code(self.whole.to_string())),
        };

        loop {
            let op = self.lexer.peek();

            if op.map_or(false, |op| op.is_err()) {
                return Err(self.lexer.next().expect("Checked Some above").expect_err("Checked Err above")).wrap_err("in place of expected operator");
            }

            let op = match op.map(|res| res.as_ref().expect("handled Err above")) {
                None => break,

                Some(Token {
                         kind: TokenType::LEFT_PAREN,
                         ..
                     }) => Op::CALL,
                Some(Token {
                         kind: TokenType::BANG,
                         ..
                     }) => Op::BANG,

                Some(Token {
                         kind: TokenType::BANG_EQUAL,
                         ..
                     }) => Op::BANG_EQUAL,

                Some(Token {
                         kind: TokenType::EQUAL,
                         ..
                     }) => Op::EQUAL,

                Some(Token {
                         kind: TokenType::EQUAL_EQUAL,
                         ..
                     }) => Op::EQUAL_EQUAL,

                Some(Token {
                         kind: TokenType::GREATER,
                         ..
                     }) => Op::GREATER,

                Some(Token {
                         kind: TokenType::GREATER_EQUAL,
                         ..
                     }) => Op::GREATER_EQUAL,

                Some(Token {
                         kind: TokenType::LESS,
                         ..
                     }) => Op::LESS,

                Some(Token {
                         kind: TokenType::SLASH,
                         ..
                     }) => Op::SLASH,

                Some(Token {
                         kind: TokenType::MINUS,
                         ..
                     }) => Op::MINUS,

                Some(Token {
                         kind: TokenType::DOT,
                         ..
                     }) => Op::FIELD,

                Some(Token {
                         kind: TokenType::PLUS,
                         ..
                     }) => Op::PLUS,

                Some(Token {
                         kind: TokenType::STAR,
                         ..
                     }) => Op::STAR,

                Some(Token {
                         kind: TokenType::OR,
                         ..
                     }) => Op::OR,

                Some(Token {
                         kind: TokenType::AND,
                         ..
                     }) => Op::AND,

                Some(Token {
                         kind: TokenType::LESS_EQUAL,
                         ..
                     }) => Op::LESS_EQUAL,

                Some(Token {
                         kind:
                         TokenType::RIGHT_PAREN
                         | TokenType::COMMA
                         | TokenType::SEMICOLON
                         | TokenType::RIGHT_BRACE,
                         ..
                     }) => break,

                Some(token) => return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset +token.origin.len(), "here"),
                    ],
                    help = format!("Unexpected {token:?}"),
                    "Expected an infix operator",
                }.with_source_code(self.whole.to_string())),
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                lhs = match op {
                    Op::CALL => TokenTree::Call {
                        callee: Box::new(lhs),
                        arguments: self
                            .parse_fun_call_arguments()
                            .wrap_err("in function call arguments")?,
                    },

                    _ => TokenTree::Cons(op, vec![lhs]),
                };

                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                //lhs = if op == '?' {
                //    let mhs = self.parse_within(0);
                //    assert_eq!(self.lexer.next(), TokenTP::Op(':'));
                //    let rhs = self.parse_within(r_bp);
                //    TokenTree::Cons(op, vec![lhs, mhs, rhs])
                //} else {
                //    let rhs = self.parse_within(r_bp);
                //    TokenTree::Cons(op, vec![lhs, rhs])
                //};
                //
                lhs = match op {
                    _ => {
                        let rhs = self.parse_expression_within(r_bp).wrap_err_with(|| format!("on the right-hand side of {lhs} {op}"))?;

                        TokenTree::Cons(op, vec![lhs, rhs])
                    }
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
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("on the left-hand side");
            }
        };

        let mut lhs = match lhs {
            Token {
                kind: TokenType::IDENT,
                origin,
                ..
            } => TokenTree::Atom(Atom::Ident(origin)),

            Token {
                kind: TokenType::SUPER,
                origin,
                ..
            } => TokenTree::Atom(Atom::Super),

            Token {
                kind: TokenType::THIS,
                origin,
                ..
            } => TokenTree::Atom(Atom::This),

            Token {
                kind: TokenType::LEFT_PAREN,
                ..
            } => {
                let lhs = self.parse_expression_within(0).wrap_err("in bracketed expression")?;

                self.lexer.expect(TokenType::RIGHT_PAREN, "Unexpected end to bracketed expression").wrap_err("after bracketed expression")?;

                TokenTree::Cons(Op::GROUP, vec![lhs])
            }

            Token { kind: TokenType::PRINT | TokenType::RETURN, .. } => {
                let op = match lhs.kind {
                    TokenType::PRINT => Op::PRINT,
                    TokenType::RETURN => Op::RETURN,
                    _ => unreachable!("by outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_expression_within(r_bp).wrap_err("in right-hand side")?;

                return Ok(TokenTree::Cons(op, vec![rhs]));
            }

            Token { kind: TokenType::CLASS, .. } =>
                {
                    let token = self.lexer.expect(TokenType::IDENT, "expected Identifier").wrap_err("in class name")?;
                    // we know that this is an IDENT
                    assert_eq!(token.kind, TokenType::IDENT);

                    let ident = TokenTree::Atom(Atom::Ident(token.origin));

                    if lhs.kind == TokenType::VAR {

                        // the next one must be an equal sign
                        // the equal sign is not really an argument here .. just a thing we require in the parsing
                        self.lexer.expect(TokenType::EQUAL, "Missing =").wrap_err("in variable assignment")?;
                    }

                    let block = self.parse_block().wrap_err("in class definition")?;

                    return Ok(TokenTree::Cons(Op::CLASS, vec![ident, block]));
                }

            Token { kind: TokenType::VAR, .. } =>
                {
                    let token = self.lexer.expect(TokenType::IDENT, "expected Identifier").wrap_err("in variable assignment")?;

                    assert_eq!(token.kind, TokenType::IDENT);

                    let ident = TokenTree::Atom(Atom::Ident(token.origin));

                    self.lexer.expect(TokenType::EQUAL, "Missing =").wrap_err("in variable assignment")?;

                    let second = self.parse_expression_within(0).wrap_err("in variable assignment expression")?;

                    return Ok(TokenTree::Cons(Op::VAR, vec![ident, second]));
                }

            Token { kind: TokenType::FUN, .. } => {
                let token = self.lexer.expect(TokenType::IDENT, "expected Identifier").wrap_err_with(|| format!("in function name declaration"))?;

                let name = token.origin;
                let ident = Atom::Ident(token.origin);

                let parameters = Vec::new();

                self.lexer.expect(TokenType::LEFT_PAREN, "missing (").wrap_err_with(|| format!("in parameter list of function {name}"))?;


                // check for immediate parameter list end
                if matches!(self.lexer.peek(), Some(Ok(Token{kind: TokenType::RIGHT_PAREN, ..})))
                {
                    // immediate parameter list end
                } else {
                    loop {
                        let parameter = self.lexer.expect(TokenType::IDENT, "unexpect token").wrap_err_with(|| format!("in parameter #{} of function {name}", parameters.len() + 1))?;

                        let token = self.lexer.expect_where(|token| matches!(token.kind, TokenType::RIGHT_PAREN | TokenType::COMMA), "continuing paramter list")
                            .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                        if token.kind == TokenType::RIGHT_PAREN {
                            break;
                        }
                    }
                }

                let block = self.parse_block().wrap_err_with(|| format!("in body of function {name}"))?;

                return Ok(TokenTree::Fun {
                    name: ident,
                    parameters,
                    body: Box::new(block),
                });
            }

            // handle FOR separately
            Token { kind: TokenType::FOR, .. } => {

                // start the header parsing
                self.lexer.expect(TokenType::LEFT_PAREN, "missing (").wrap_err("in for loop condition")?;

                let init = self.parse_expression_within(0).wrap_err("in initialization of for \
                loop")?;

                self.lexer.expect(TokenType::SEMICOLON, "missing ;").wrap_err("in for loop condition")?;

                let cond = self.parse_expression_within(0).wrap_err("in loop condition of for \
                loop")?;

                self.lexer.expect(TokenType::SEMICOLON, "missing ;").wrap_err("in for loop condition");

                let upd = self.parse_expression_within(0).wrap_err("in update of for loop")?;

                self.lexer.expect(TokenType::RIGHT_PAREN, "missing )").wrap_err("in for loop")?;
                // end of header parsing

                let block = self.parse_block().wrap_err("in body of for loop")?;

                // return the tokentree made form this block
                return Ok(TokenTree::Cons(Op::FOR, vec![init, cond, upd, block]))
            }

            Token { kind: TokenType::WHILE, .. } => {
                self.lexer.expect(TokenType::LEFT_PAREN, "missing (").wrap_err("in while loop condition")?;
                let cond = self.parse_expression_within(0).wrap_err("in condition of while loop")?;
                self.lexer.expect(TokenType::RIGHT_PAREN, "missing )").wrap_err("in while loop condition")?;

                let block = self.parse_block().wrap_err("in body of WHILE loop")?;

                return Ok(TokenTree::Cons(Op::WHILE, vec![cond, block]))
            }

            Token { kind: TokenType::IF, .. } => {

                // start the header parsing
                self.lexer.expect(TokenType::LEFT_PAREN, "missing (").wrap_err("in if condition")?;
                let cond = self.parse_expression_within(0).wrap_err("in if condition")?;
                self.lexer.expect(TokenType::RIGHT_PAREN, "missing )").wrap_err("in if condition")?;
                // end of header parsing

                let block = self.parse_block().wrap_err("in body of IF statement")?;

                // Do we have an ELSE?
                let mut otherwise = None;

                if matches!(self.lexer.peek(), Some(Ok(Token{
                    kind: TokenType::ELSE,
                    ..
                })))
                {
                    self.lexer.next();

                    otherwise = Some(self.parse_block().wrap_err("in body of ELSE statement")?);
                }

                // return the tokentree made form this block
                return Ok(TokenTree::If {
                    cond: Box::new(cond),
                    yes: Box::new(block),
                    no: otherwise.map(Box::new),
                });
            }

            token => {
                return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here")
                    ],

                    help = format!("unexpected {token:?}"),
                    "Expected a statement",
                }.with_source_code(self.whole.to_string()));
            }
        };


        loop {
            let op = self.lexer.peek();

            if op.map_or(false, |op| op.is_err()) {
                return Err(self.lexer.next().expect("checked Some above").expect_err("Checked Err above")).wrap_err("in place of expected operator");
            }

            let op = match op.map(|res| res.as_ref().expect("handled err above")) {
                None => break,

                Some(Token {
                         // here we need to look for infix and postfix operators specifically
                         kind: TokenType::DOT,
                         ..
                     }) => Op::FIELD,

                Some(Token {
                         // here we need to look for infix and postfix operators specifically
                         kind: TokenType::LEFT_PAREN,
                         ..
                     }) => Op::CALL,

                Some(token) => return Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here")
                        ],

                        help = format!("unexpected {token:?}"),
                        "Expected an operator",
                    }.with_source_code(self.whole.to_string())),
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                lhs = match op {
                    Op::CALL => TokenTree::Call {
                        callee: Box::new(lhs),
                        arguments: self.parse_fun_call_arguments().wrap_err("in function call arguments")?,
                    },

                    _ => TokenTree::Cons(op, vec![lhs]),
                };

                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }

                self.lexer.next();

                let rhs = self.parse_expression_within(r_bp).wrap_err_with(|| format!("on the right-hand side of {lhs} {op}"))?;

                lhs = TokenTree::Cons(op, vec![lhs, rhs]);

                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) { 
    match op {
        Op::BANG | Op::MINUS => ((), 11),
        Op::RETURN | Op::PRINT => ((), 1),
        _ => panic!("bad op: {:?}", op),
    }
}

fn infix_binding_power(op: Op) -> Option<(u8, u8)> {
    let res = match op {
        // '=' => (2, 1),
        // '?' => (4, 3),
        Op::AND | Op::OR => (3, 4),
        Op::BANG_EQUAL
        | Op::EQUAL_EQUAL
        | Op::LESS
        | Op::LESS_EQUAL
        | Op::GREATER
        | Op::GREATER_EQUAL => (5, 6),
        Op::PLUS | Op::MINUS => (7, 8),
        Op::STAR | Op::SLASH => (9, 10),
        Op::FIELD => (16, 15),
        _ => return None,
    };
    Some(res)
}

fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    let res = match op {
        Op::CALL => (13, ()),
        _ => return None,
    };
    Some(res)
}
