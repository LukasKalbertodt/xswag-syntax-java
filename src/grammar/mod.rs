pub mod main;

use ast;
use lex;
use base::diag::Report;
use base::code::{Span, BytePos};

// some helper functions

fn get_visibility(mods: &Vec<(BytePos, lex::Keyword, BytePos)>)
	-> (Option<ast::Visibility>, Vec<Report>)
{
	use lex::{Token, Keyword};

    let mut vis = None;
    let mut first_vis = None;
    let mut errors = Vec::new();

	for &(lo, modifier, hi) in mods {
		// convert into more convinient type
		let span = Span {
			lo: lo,
			hi: hi
		};


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

    (vis, errors)
}

macro_rules! gen_finder {
    ($name:ident, $keyword:ident, $msg:expr) => {
        fn $name(mods: &Vec<(BytePos, lex::Keyword, BytePos)>)
            -> (bool, Vec<Report>)
        {
            let mut found = false;
            let mut first_pos = None;
            let mut reports = Vec::new();

            for &(lo, modifier, hi) in mods {
                // convert into more convinient type
                let span = Span {
                    lo: lo,
                    hi: hi
                };

                match modifier {
                    lex::Keyword::$keyword => {
                        // check if there was a visibility modifier before
                        // this one
                        if let Some(prev_span) = first_pos {
                            let e = Report::simple_error(
                                stringify!("duplicate `" $msg "` modifier"),
                                span
                            ).with_span_note(
                                stringify!(
                                    "the first `"
                                    $msg
                                    "` modifier is already here"
                                ),
                                prev_span
                            );
                            reports.push(e);
                        } else {
                            first_pos = Some(span);
                            found = true;
                        }
                    },
                    _ => {},
                }
            }

            (found, reports)
        }
    }
}

gen_finder!(find_static, Static, "static");
gen_finder!(find_strictfp, Strictfp, "strictfp");
gen_finder!(find_abstract, Abstract, "abstract");



fn forward_reports<T, F>(orig: &mut Vec<Report>, fun: F) -> T
    where F: FnOnce() -> (T, Vec<Report>)
{
    let (v, reports) = fun();
    orig.extend_from_slice(&reports);
    v
}
