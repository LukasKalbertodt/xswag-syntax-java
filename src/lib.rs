extern crate xswag_base as base;
extern crate lalrpop_util;

pub mod lex;
pub mod ast;
pub mod grammar;

use base::diag::Report;
use base::code::Span;
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
        ParseError::InvalidToken { location: loc } => {
            Report::simple_error("lalrpop InvalidToken", Span::single(loc))
        },
        ParseError::UnrecognizedToken { token: tok, expected: exp } => {
            match tok {
                None => {
                    Report::simple_error(
                        "lalrpop UnrecognizedToken ???",
                        Span::dummy()
                    )
                },
                Some((lo, tok, hi)) => {
                    Report::simple_error(
                        format!("lalrpop UnrecognizedToken `{:?}`", tok),
                        Span::new(lo, hi),
                    )
                }
            }.with_note(format!("Expected one of {:?}", exp))
        },
        ParseError::ExtraToken  { token: (lo, tok, hi) } => {
            Report::simple_error(
                format!("lalrpop ExtraToken {:?}", tok),
                Span::new(lo, hi),
            )
        },
    });
    (res, errors)
}
