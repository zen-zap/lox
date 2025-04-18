/// src/asth.rs
/// helper module for the AST and parser
use crate::token_type::Token;
use std::borrow::Cow;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'de> {
	String(Cow<'de, str>),
	Number(f64),
	Nil,
	Bool(bool),
	Ident(&'de str),
	Super,
	This,
}

impl fmt::Display for Atom<'_> {
	fn fmt(
		&self,
		f: &mut fmt::Formatter<'_>,
	) -> fmt::Result {
		match self {
			// Atom::String(s) => write!(f, "\"{s}\""),
			Atom::String(s) => write!(f, "{s}"),
			Atom::Number(n) => {
				if *n == n.trunc() {
					write!(f, "{n}.0")
				} else {
					write!(f, "{n}")
				}
			},
			Atom::Nil => write!(f, "nil"),
			Atom::Bool(b) => write!(f, "{b:?}"),
			Atom::Ident(i) => write!(f, "{i}"),
			Atom::Super => write!(f, "super"),
			Atom::This => write!(f, "this"),
		}
	}
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
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
	fn fmt(
		&self,
		f: &mut fmt::Formatter<'_>,
	) -> fmt::Result {
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
	Fun {
		name: Atom<'de>,
		parameters: Vec<Token<'de>>,
		body: Box<TokenTree<'de>>,
	},
	/// for representing IF statements
	If {
		cond: Box<TokenTree<'de>>,
		yes: Box<TokenTree<'de>>,
		no: Option<Box<TokenTree<'de>>>,
	},
	/// for representing Function calls
	Call { callee: Box<TokenTree<'de>>, arguments: Vec<TokenTree<'de>> },
}

impl fmt::Display for TokenTree<'_> {
	fn fmt(
		&self,
		f: &mut fmt::Formatter<'_>,
	) -> fmt::Result {
		match self {
			TokenTree::Atom(i) => write!(f, "{}", i),
			TokenTree::Cons(head, rest) => {
				write!(f, "({}", head)?;
				for s in rest {
					write!(f, " {s}")?
				}
				write!(f, ")")
			},
			TokenTree::Fun { name, parameters, body } => {
				write!(f, "(def {name}")?;
				for p in parameters {
					write!(f, " {p}")?
				}
				write!(f, " {body})")
			},
			TokenTree::Call { callee, arguments } => {
				write!(f, "({callee}")?;
				for a in arguments {
					write!(f, " {a}")?
				}
				write!(f, ")")
			},
			TokenTree::If { cond, yes, no } => {
				write!(f, "(if {cond} {yes}")?;
				if let Some(no) = no {
					write!(f, " {no}")?
				}
				write!(f, ")")
			},
		}
	}
}
