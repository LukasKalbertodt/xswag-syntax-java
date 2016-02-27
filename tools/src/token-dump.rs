extern crate xswag_syntax_java as syntax;
extern crate xswag_base as base;
extern crate term_painter;
extern crate docopt;
extern crate rustc_serialize;

use std::fs::File;
use docopt::Docopt;
use std::io::Read;
use base::diag;
use term_painter::{Color, ToStyle};

pub const USAGE: &'static str = "
Usage: token-dump [options] <file>


Options:
    -g, --grey      Do not use colors.
    -h, --help      Show this message.
";

#[derive(Debug, RustcDecodable)]
pub struct Args {
    pub arg_file: String,
    pub flag_grey: bool,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let mut file = File::open(args.arg_file).unwrap();
    let mut src = String::new();
    file.read_to_string(&mut src).unwrap();

    let file_map = base::code::FileMap::new("PlayExample.java", src);

    let lexer = syntax::lex::Tokenizer::new(&file_map);
        // .take_while(|res| res.is_ok())
        // .map(|res| { let ts = res.unwrap(); (ts.span.lo, ts.tok, ts.span.hi) })
        // .filter(|t| t.1.is_real());
    for tok in lexer {
        match tok {
            Ok(syntax::lex::TokenSpan { tok, span }) => {
                if args.flag_grey {
                    println!("{:?} {:?}", tok, span);
                } else {
                    println!("{:?} {:?}", tok, Color::Green.paint(span));
                }
            },
            Err(e) => println!("{:?}", e),
        }
    }
}
