#[macro_use]
extern crate log;
extern crate xswag_base as base;
extern crate lalrpop_util;

pub mod lex;
pub mod ast;
pub mod grammar;

use base::diag::{Remark, Report, Snippet};
use base::code::{BytePos, FileMap, Span};
use lalrpop_util::ParseError;
use lex::{Token, TokenSpan};
use std::cell::RefCell;
use ast::ItemExt;

const MAX_PREVIEW_LEN: usize = 15;


pub fn parse_compilation_unit(file: &FileMap)
    -> (Option<ast::CompilationUnit>, Vec<Report>)
{
    // Save all tokens that have been read already
    let tokens = RefCell::new(Vec::new());
    let lexing_err = RefCell::new(None);

    // Stop at the first lexing error and remove all non real token for parsing
    let lexer = lex::Tokenizer::new(file)
        .take_while(|res| match res {
            &Ok(_) => true,
            &Err(ref e) => {
                *lexing_err.borrow_mut() = Some(e.clone());
                false
            }
        })
        .map(Result::unwrap)
        .inspect(|t| tokens.borrow_mut().push(t.clone()))
        .map(|ts| { (ts.span.lo, ts.tok, ts.span.hi) })
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
                    Report::simple_spanless_error(
                        "Unexpected end of file (EOF) while parsing. Maybe you \
                            forgot to close a parentheses or brace?",
                    )
                },
                Some((lo, tok, hi)) => {
                    handle_unexpected_token(
                        tok,
                        Span::new(lo, hi),
                        file,
                        &expected,
                        &tokens.borrow(),
                    )
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

    let mut reps = Vec::new();
    let ast = if let Some(ref lexing_err) = *lexing_err.borrow() {
        reps.push(lexing_err.report.clone());
        None
    } else {
        res.map(|ast| Some(ast)).unwrap_or_else(|e| { reps.push(e); None })
    };

    if let Some(ref ast) = ast {
        let fname = file.filename()
            .trim_right_matches(".java")
            .trim_right_matches(".jav");
        let wrong_types = ast.types
            .iter()
            .filter(|ty| ty.vis() == ast::Visibility::Public)
            .filter_map(|ty| ty.ident())
            .filter(|&ident| ident.name != fname);
        for &ast::Ident { ref name, span } in wrong_types {
            let msg = format!(
                "a public type called '{}' should be declared in a file \
                    called '{}.java'. Either rename your file appropriately \
                    or rename the type to '{}'",
                name,
                name,
                fname
            );
            reps.push(Report::simple_error(msg, span));
        }
    }

    reps.extend_from_slice(&errors);
    (ast, reps)
}

fn handle_unexpected_token(
    token: Token,
    span: Span,
    file: &FileMap,
    expected: &[String],
    tokens: &[TokenSpan]
) -> Report {
    // ------ Prepare a nice error message -------
    let tok_name = token.as_java_string();

    // Prepare token preview
    let mut chs = file.src()[span.into_range()].chars();
    let mut tok_str: String = chs.by_ref().take(MAX_PREVIEW_LEN).collect();
    if chs.next().is_some() {
        // if the original span was longer than 15 chars, we append '...'
        tok_str.push_str("...");
    }

    // pack as `Report`
    let mut msg = format!("unexpected '{}'", tok_name);
    if tok_name != tok_str {
        msg.push_str(&format!(" (`{}`)", tok_str));
    }
    if expected.len() == 1 {
        msg.push_str(&format!(". Expected `{}`", expected[0]));
    } else if expected.len() > 1 {
        msg.push_str(&format!(". Expected one of {:?}", expected));
    }

    let rep = Report::simple_error(msg, span);

    // ------ Check if we can give the user some more useful information ------
    // Check if the user has forgotten a semicolon. We trigger this note if
    //   1. the unexpected token is the first token in the line
    //   2. the last line does not end with a semicolon or a opening brace '{'
    //

    // The `loc_lo` was returned from the map: unwrap OK
    let loc_lo = file.get_loc(span.lo);
    let line = file.get_line(loc_lo.line).unwrap();

    // check condition [1]
    let is_line_start = line
        .char_indices()
        .take_while(|&(i, _)| i < loc_lo.col.0 as usize)
        .all(|(_, c)| c.is_whitespace());

    // check condition [2]
    let prev = tokens
        .iter()
        .rev()
        // get the previous token (ignoring whitespace)
        .skip_while(|ts| ts.span.lo >= span.lo)
        .find(|ts| ts.tok.is_real());
    let is_good = prev.map(|ts| ts.tok == Token::Semi || ts.tok == Token::BraceOp);

    if is_line_start && is_good == Some(false) {
        // we can safely unwrap here: `is_good.is_some()` => `prev.is_some()`
        rep.with_remark(Remark::note(
            "maybe you forgot a semicolon (`;`) at the end of this line?",
            Snippet::Replace {
                span: Span::empty_at(prev.unwrap().span.hi),
                with: ";".into(),
            },
        ))
    } else {
        rep.with_note("this is a syntax error. Make sure all \
            parentheses are balanced and nothing is missing.")
    }
}
