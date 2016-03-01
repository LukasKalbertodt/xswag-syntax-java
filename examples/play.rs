extern crate xswag_syntax_java as syntax;
extern crate xswag_base as base;
extern crate env_logger;

use std::fs::File;
use std::io::Read;
use base::diag;

fn main() {
    env_logger::init().unwrap();

    let mut file = File::open("examples/PlayExample.java").unwrap();
    let mut src = String::new();
    file.read_to_string(&mut src).unwrap();

    let file_map = base::code::FileMap::new("PlayExample.java", src);
    let (ast, errors) = syntax::parse_compilation_unit(&file_map);

    match ast {
        Some(ast) => println!("{:#?}", ast),
        None => {},
    }
    for e in &errors {
        diag::print(&e, &file_map, diag::PrintOptions::default());
    }
}
