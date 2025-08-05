//! in src/evaluator.rs
//!
//! Contains the logic for the evaluator

use crate::{
    parser,
    asth::{Op, Atom, TokenTree},
    lexer::{Eof, Lexer},
};

// for evaluating something, we need some kind of information dump to store the values temporarily 
// we can call this an environment

/// Holds the required environment variables needed for calclations
pub struct EvalEnv {
    pub mid_val: usize,
}

/// Contains the evaluator that walks the AST to form the result
pub struct Evaluator<'de> {
    env: EvalEnv,
    tree: TokenTree<'de>,
}

impl Evaluator {

}
