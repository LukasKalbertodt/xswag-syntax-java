extern crate xswag_base as base;
extern crate lalrpop_util;

pub mod lex;
pub mod ast;
pub mod grammar;

use base::diag::Report;
use base::code::{BytePos, LineIdx, Span, SrcOffset};
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

    let res = grammar::java8::parse_CompilationUnit(&mut errors, lexer);
    let res = res.map_err(|e| match e {
        ParseError::User { error: e } => e,
        ParseError::InvalidToken { location: loc } => {
            Report::simple_error("lalrpop InvalidToken", Span::single(loc))
        },
        ParseError::UnrecognizedToken { token, expected } => {
            match token {
                None => {
                    Report::simple_error(
                        "lalrpop UnrecognizedToken ???",
                        Span::dummy()
                    )
                },
                Some((lo, tok, hi)) => {
                    // prepare a nice error message
                    let max_preview_len = 15;
                    let mut chs = file.src()[lo.0 as usize .. hi.0 as usize].chars();
                    let mut tok_str: String = chs.by_ref().take(max_preview_len).collect();
                    if chs.next().is_some() {
                        tok_str.push_str("...");
                    }
                    let tok_name = tok.as_java_string();

                    let msg = format!(
                        "unexpected token '{}' (`{}`). Expected one of {:?}",
                        tok_name,
                        tok_str,
                        expected,
                    );

                    // prepare report
                    let rep = Report::simple_error(msg, Span::new(lo, hi));

                    // Check if the user has forgotten a semicolon.
                    // The `line` was returned from the map: unwrap OK
                    let start = file.get_loc(lo);
                    let line = file.get_line(start.line).unwrap();
                    let is_line_start = line
                        .char_indices()
                        .take_while(|&(i, _)| i < start.col.0 as usize)
                        .all(|(_, c)| c.is_whitespace());
                    let last_from_prev_line = file
                        .get_line(start.line - LineIdx(1))
                        .unwrap()
                        .trim_right()
                        .char_indices()
                        .last()
                        .unwrap();  // There is at least '\n' in the line

                    if is_line_start && last_from_prev_line.1 != ';' {
                        rep.with_span_note(
                            "maybe you forgot a semicolon (`;`) here?",
                            Span::single(BytePos(last_from_prev_line.0 as SrcOffset)),
                        )
                    } else {
                        rep.with_note("this is a syntax error. Make sure all \
                            parentheses are balanced and nothing is missing.")
                    }
                }
            }
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
