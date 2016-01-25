macro_rules! check_mods {
    ($errors:ident, $context:expr, $actual:ident, [$($allowed:ident),*] ) => {{
        let allowed_mods = [$(Keyword::$allowed),*];
        $errors.extend_from_slice(
            &check_allowed_modifier(&$actual, &allowed_mods, $context)
        );
    }}
}


pub mod main;

use ast;
use lex;
use base::diag::Report;
use base::code::{Span, BytePos};

type Mods = [(lex::Keyword, Span)];

// some helper functions
fn check_allowed_modifier(actual: &Mods, allowed: &[lex::Keyword], ctx: &str)
    -> Vec<Report>
{
    let mut seen = Vec::new();
    actual.iter().flat_map(|&(m, span)| {
        // check for duplicate modifiers
        let mut reps = Vec::new();
        if let Some(&(prev_span, _)) = seen.iter().find(|&&(_, hay)| hay == m) {
            reps.push(Report::simple_error(
                format!("duplicate `{:?}` modifier", m),
                span,
            ).with_span_note(
                format!("the first `{:?}` modifier is already here", m),
                prev_span,
            ));
        } else {
            seen.push((span, m));
        }

        // check if modifier is valid
        if !allowed.contains(&m) {
            let msg = format!("modifier `{}` is illegal in this context", m);
            let note = format!("valid `{}` modifier are: {:?}", ctx, allowed);
            reps.push(Report::simple_error(msg, span).with_note(note));
        }
        reps.into_iter()
    }).collect()
}

fn get_visibility(mods: &Mods, errors: &mut Vec<Report>)
	-> Option<ast::Visibility>
{
	use lex::{Token, Keyword};

    let mut vis = None;
    let mut first_vis = None;

	for &(modifier, span) in mods {
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

    vis
}

macro_rules! gen_finder {
    ($name:ident, $keyword:ident) => {
        /// Returns if the modifier for the given property is in the list of
        /// modifiers. Does NOT check duplicate modifier.
        fn $name(mods: &Mods) -> Option<Span> {
            mods.iter()
                .find(|&&(m, _)| m == lex::Keyword::$keyword)
                .map(|&(_, span)| span)
        }
    }
}

gen_finder!(get_static, Static);
gen_finder!(get_strictfp, Strictfp);
gen_finder!(get_abstract, Abstract);
