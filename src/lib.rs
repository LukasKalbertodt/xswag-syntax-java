extern crate xswag_base as base;
extern crate lalrpop_util;

pub mod lex;
pub mod ast;
pub mod grammar;

use base::diag::Report;
use lalrpop_util::ParseError;

pub fn parse_compilation_unit(file: &base::code::FileMap)
    -> (Result<ast::CompilationUnit, Report>, Vec<Report>)
{
    // Stop at the first lexing error and remove all non real token for parsing
    let lexer = lex::Tokenizer::new(file)
        .take_while(|res| res.is_ok())
        .map(|res| { let ts = res.unwrap(); (ts.span.lo, ts.tok, ts.span.hi) })
        .filter(|t| t.1.is_real());

    let mut errors = Vec::new();

    let res = grammar::main::parse_CompilationUnit(&mut errors, lexer);
    let res = res.map_err(|e| match e {
        ParseError::User { error: e } => e,
        // TODO: this is stupid
        le @ _ => Report::simple_error(format!("lalrpop: {:?}", le),
            base::code::Span::dummy()),
    });
    (res, errors)
}
