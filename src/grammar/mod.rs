pub mod main;

use ast;
use lex;
use base::diag::Report;
use base::code::{Span, BytePos};

// some helper functions

fn get_visibility(mods: &Vec<(BytePos, lex::Token, BytePos)>)
	-> Result<Option<ast::Visibility>, Vec<Report>>
{
	use lex::{Token, Keyword};

    let mut vis = None;
    let mut first_vis = None;
    let mut errors = Vec::new();

	for &(lo, ref tok, hi) in mods {
		// convert into more convinient type
		let span = Span {
			lo: lo,
			hi: hi
		};
        let modifier = if let &Token::KeyW(kw) = tok { kw } else { continue; };


        match modifier {
            Keyword::Public | Keyword::Private | Keyword::Protected => {
                // check if there was a visibility modifier before this one
                if let Some(prev_span) = first_vis {
                    let e = Report::simple_error(
                        "duplicate visibility modifier",
                        span
                    ).with_span_note(
                        "the first visibility modifier is already here",
                        prev_span
                    );
                    errors.push(e);
                } else {
                    first_vis = Some(span);
                    vis = Some(match modifier {
                        Keyword::Public => ast::Visibility::Public,
                        Keyword::Private => ast::Visibility::Private,
                        Keyword::Protected => ast::Visibility::Protected,
                        _ => unreachable!(),
                    });
                }
            },
            _ => {},
        }
    }

    if errors.is_empty() {
        Ok(vis)
    } else {
        Err(errors)
    }
}


// fn get_visibility(mods: &Vec<(lex::Keyword, Span)>)
// 	-> Result<Option<ast::Visibility>, Vec<diag::Report>>
// {
// 	for &(m, span) in mods {
//         match m {
//             Keyword::Public | Keyword::Private | Keyword::Protected => {
//                 // check if there was a visibility modifier before this one
//                 if let Some(prev_span) = first_vis {
//                     let e = Report::simple_error(
//                         "duplicate visibility modifier",
//                         span
//                     ).with_span_note(
//                         "the first visibility modifier is already here",
//                         prev_span
//                     );
//                     errors.push(e);
//                 } else {
//                     first_vis = Some(span);
//                     vis = match m {
//                         Keyword::Public => ast::Visibility::Public,
//                         Keyword::Private => ast::Visibility::Private,
//                         Keyword::Protected => ast::Visibility::Protected,
//                         _ => unreachable!(),
//                     };
//                 }
//             },
//             Keyword::Static => {
//                 if static_ {
//                     let e = Report::simple_error(
//                         "duplicate `static` modifier",
//                         span
//                     ).with_span_note(
//                         "the modifier is already set here",
//                         mods.iter()
//                             .find(|&&(m, span)| m == Keyword::Static)
//                             .map(|&(_, span)| span).unwrap()
//                     );
//                     errors.push(e);
//                 } else {
//                     static_ = true;
//                 }
//             },
//             Keyword::Final => {
//                 if final_ {
//                     let e = Report::simple_error(
//                         "duplicate `final` modifier",
//                         span
//                     ).with_span_note(
//                         "the modifier is already set here",
//                         mods.iter()
//                             .find(|&&(m, span)| m == Keyword::Final)
//                             .map(|&(_, span)| span).unwrap()
//                     );
//                     errors.push(e);
//                 } else {
//                     final_ = true;
//                 }
//             },
//             _ => {},
//         }
//     }
// }
