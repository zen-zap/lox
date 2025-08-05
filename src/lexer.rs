#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(dead_code)]
use crate::token_type::{SingleTokenError, StringTerminationError, Token, TokenType};
use miette::{Context, Diagnostic, Error, LabeledSpan, NamedSource, Report, SourceSpan};
use std::fmt::format;
use thiserror::Error;

/// The `Lexer` struct is responsible for tokenizing the input string.
/// It holds the entire input string, the remaining unprocessed part of the string,
/// and the current byte index for tracking the position in the string.
/// The `Lexer` struct implements the `Iterator` trait, allowing it to produce tokens one at a time.
/// The `next` method is where the actual lexing happens.
/// It processes the input string character by character, identifying tokens based on the characters encountered
#[derive(Debug)]
pub struct Lexer<'de> {
	/// holds the entire String
	whole: &'de str,
	/// holds the remainder of the String
	rest: &'de str,
	/// to keep track of the index we're at
	byte: usize,
	/// holds the peeked tokens -- used for lookahead
	peeked: Option<Result<Token<'de>, miette::Error>>,
}

impl<'de> Lexer<'de> {
	/// Creates a new `Lexer` instance with the given input string.
	/// The `whole` field is initialized with the input string,
	/// the `rest` field is set to the same string, and the `byte` index is initialized to 0.
	/// This function is used to initialize the lexer before starting the tokenization process.
	pub fn new(input: &'de str) -> Self {
		Self { whole: input, rest: input, byte: 0, peeked: None }
	}
}

// This one defines helper functions for parsing
impl<'de> Lexer<'de> {
	/// helper function for unexpected token errors
	///
	/// returns a Token<'de>
	pub fn expect_where(
		&mut self,
		mut check: impl FnMut(&Token<'de>) -> bool,
		unexpected_msg: &str,
	) -> Result<Token<'de>, miette::Error> {
		match self.next() {
			Some(Ok(token)) if check(&token) => Ok(token),
			Some(Ok(token)) => Err(miette::miette! {
				labels = vec![
					LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here")
				],

				help = format!("Expected {token:?}"),
				"{unexpected_msg}",
			}
			.with_source_code(self.whole.to_string())),
			Some(Err(e)) => Err(e),
			None => Err(Eof.into()),
		}
	}

	/// helper function for unexpected token errors
	///
	/// returns a Token<'de>
	pub fn expect(
		&mut self,
		expected: TokenType,
		unexpected_msg: &str,
	) -> Result<Token<'de>, miette::Error> {
		self.expect_where(|next| next.kind == expected, unexpected_msg)
	}

	/// helper function for peeking into the Lexer tokens
	///
	/// peeks one Token not a character
	pub fn peek(&mut self) -> Option<&Result<Token<'de>, miette::Error>> {
		if self.peeked.is_some() {
			return self.peeked.as_ref();
		}

		self.peeked = self.next();

		self.peeked.as_ref()
	}
}

impl<'de> Iterator for Lexer<'de> {
	/// The `Item` type for the `Lexer` iterator is a `Result` containing either a `Token` or an `Error`.
	type Item = Result<Token<'de>, Error>;

	/// If the `Iterator` returns `Err`, it will only return `None`.
	/// Pattern helpful for streaming characters ..
	/// actual lexing happens here
	fn next(&mut self) -> Option<Self::Item> {

		if let Some(next) = self.peeked.take() {
			return Some(next);
		}

		loop {
			// must be inside the loop .. since we use chars with byte_index and self.rest updates based on this
			let mut chars = self.rest.chars(); // get the rest of the chars left

            // get the next char
			let c = chars.next()?;
			// `at` represents the byte-index where this character begins at the string
			let c_at = self.byte;
			// holds the current character as a UTF-8 byte slice from the input string
			let c_str = &self.rest[..c.len_utf8()];

			// holds self.rest
			let c_onwards = self.rest;
			self.rest = chars.as_str();
			self.byte += c.len_utf8(); // incremented by the number of bytes in the current character

			/// type to help us to make sure that we have handled all cases and modularize it
			/// these are multi character tokens
			pub enum Started {
				/// String character tokens set
				String,
				/// set of number characters
				Number,
				/// set of characters forming an identifier
				Ident,
				/// represents <=, <, >=, >, =, ==, !=
				IfEqualElse(TokenType, TokenType),
				/// to handle / and //
				Slash,
			}

			let just = move |kind: TokenType| Some(Ok(Token { kind, offset: c_at, origin: c_str }));

            // TOKEN RECOGNITION
            //
            // If it is a normal token or a standalone token, then you just return it as it is 
            // if not .. start another processing for what's to come

			let started = match c {
				'(' => return just(TokenType::LEFT_PAREN),
				')' => return just(TokenType::RIGHT_PAREN),
				'{' => return just(TokenType::LEFT_BRACE),
				'}' => return just(TokenType::RIGHT_BRACE),
				',' => return just(TokenType::COMMA),
				'.' => return just(TokenType::DOT),
				'+' => return just(TokenType::PLUS),
				';' => return just(TokenType::SEMICOLON),
				'*' => return just(TokenType::STAR),
				'-' => return just(TokenType::MINUS),
				c if c.is_whitespace() => continue,
				'/' => Started::Slash,
				'"' => Started::String,
				'0'..='9' => Started::Number,
				'a'..='z' | 'A'..='Z' | '_' => Started::Ident,
				'=' => Started::IfEqualElse(TokenType::EQUAL_EQUAL, TokenType::EQUAL),
				'<' => Started::IfEqualElse(TokenType::LESS_EQUAL, TokenType::LESS),
				'>' => Started::IfEqualElse(TokenType::GREATER_EQUAL, TokenType::GREATER),
				'!' => Started::IfEqualElse(TokenType::BANG_EQUAL, TokenType::BANG),

				_ => {
					return Some(Err(SingleTokenError {
						err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
						src: self.whole.to_string(),
						token: c,
					}
					.into()));
				},
			};

            // MULTI-CHARACTER TOKENS

			// if started is an Option immediately return, if it's an enum then check for the variants
			// even if you don't handle the Option case .. it does so implicitly if the things are not in the match arms
			break match started {
				Started::IfEqualElse(yes, no) => {
					// deal with any whitespaces in between the token_set
					self.rest = self.rest.trim_start();
					let trimmed = c_onwards.len() - self.rest.len() - 1;
					self.byte += trimmed;

					if self.rest.trim_start().starts_with("=") {
						// get a span of the rest of the characters
						let span = &c_onwards[..trimmed + c.len_utf8() + 1];
						self.rest = &self.rest[1..]; // already trimmed above
						self.byte += 1;

						Some(Ok(Token { kind: yes, offset: c_at, origin: span }))
					} else {
						Some(Ok(Token { kind: no, offset: c_at, origin: c_str }))
					}
				},
				Started::String => {
					if let Some(end) = self.rest.find('"') {
						let literal = &c_onwards[..end + 1 + 1]; // now we include the " " at the start and the end

						self.byte += end + 1;

						self.rest = &self.rest[end + 1..];

						Some(Ok(Token { kind: TokenType::STRING, offset: c_at, origin: literal }))
					} else {
						let e = StringTerminationError {
							err_span: SourceSpan::from(self.byte - c.len_utf8()..self.whole.len()),
							src: self.whole.to_string(),
						};

						self.byte += self.rest.len();

						self.rest = &self.rest[self.rest.len()..];

						return Some(Err(e.into()));
					}
				},
				Started::Ident => {
					let first_non_ident = c_onwards
						.find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
						.unwrap_or(c_onwards.len());

					let literal = &c_onwards[..first_non_ident];

					let extra_bytes = literal.len() - c.len_utf8();

					self.byte += extra_bytes;

					self.rest = &self.rest[extra_bytes..];

					let kind = match literal {
						"and" => TokenType::AND,
						"class" => TokenType::CLASS,
						"else" => TokenType::ELSE,
						"false" => TokenType::FALSE,
						"fun" => TokenType::FUN,
						"for" => TokenType::FOR,
						"if" => TokenType::IF,
						"nil" => TokenType::NIL,
						"or" => TokenType::OR,
						"print" => TokenType::PRINT,
						"return" => TokenType::RETURN,
						"super" => TokenType::SUPER,
						"this" => TokenType::THIS,
						"true" => TokenType::TRUE,
						"var" => TokenType::VAR,
						"while" => TokenType::WHILE,
						_ => TokenType::IDENT,
					};

					return Some(Ok(Token { kind, offset: c_at, origin: literal }));
				},
				Started::Number => {
					// eprintln!("c_onwards: {c_onwards}");

					let first_non_digit = c_onwards
						.find(|c| !matches!(c, '.' | '0'..='9'))
						.unwrap_or(c_onwards.len());

					let mut literal = &c_onwards[..first_non_digit];
					// literal is something like 123.456.789 here

					// eprintln!("literal: {literal}");

					let mut dotted = literal.splitn(3, '.');
					// after 3 the thing repeats

					match (dotted.next(), dotted.next(), dotted.next()) {
						(Some(one), Some(two), Some(_three)) => {
							literal = &literal[..one.len() + 1 + two.len()];
							// it becomes 123.456 here as the literal
							// get the number literal followed by a DOT and smthng
						},

						(Some(one), Some(two), None) if two.is_empty() => {
							if (two.is_empty()) {
								literal = &literal[..one.len()];
							}
						},

						_ => {
							// leave it as is
						},
					}

					let extra_bytes = literal.len() - c.len_utf8();
					// literal operated on c_onwards ... so to get the number of things parsed ..
					// we subtract the initial stage

					self.byte += extra_bytes;
					self.rest = &self.rest[extra_bytes..];

					let n = match literal.parse() {
						Ok(n) => n,
						Err(e) => {
							return Some(Err(miette::miette! {
								labels = vec![
									LabeledSpan::at(self.byte - literal.len()..self.byte, "this numeric literal"),
								],
								"{e}",
							}
							.with_source_code(self.whole.to_string())));
						},
					};

					return Some(Ok(Token {
						kind: TokenType::NUMBER(n),
						offset: c_at,
						origin: literal,
					}));
				},
				Started::Slash => {
					if self.rest.starts_with('/') {
						// this is a comment!
						// keep reading until we hit the end of a line
						let line_end = self.rest.find('\n').unwrap_or(self.rest.len());

						self.byte += line_end;

						self.rest = &self.rest[line_end..]; // we can let the new line go through since we can skip the whitespace anyway

						continue;
					} else {
						Some(Ok(Token { kind: TokenType::SLASH, offset: c_at, origin: c_str }))
					}
				},
			};
		}
	}
}

/// This indicates an error caused by unexpected EOF
#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct Eof;
