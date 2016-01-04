//! Definition of the `Token` type and the Tokenizer
//!
//! This module contains functionality for the "lexing" stage, which is
//! responsible for converting the raw source code (a string) into a stream of
//! tokens.

#[cfg(test)]
mod test;

mod tokenizer;
mod token;

pub use self::token::*;
pub use self::tokenizer::*;
