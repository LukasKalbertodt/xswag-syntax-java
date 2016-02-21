extern crate xswag_syntax_java as syntax;
extern crate xswag_base as base;

use base::diag;

// FIXME: import is currently wrong
const SRC: &'static str = "\
import AlgoTools.IO;

public class Cheese {
    public static void main(String[] args) {
        int x;
    }
}

";

fn main() {
    let file = base::code::FileMap::new("<test>", SRC);
    let (res, errors) = syntax::parse_compilation_unit(&file);

    match res {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => {
            diag::print(&e, &file, diag::PrintOptions::default());
        }
    }
    for e in &errors {
        diag::print(&e, &file, diag::PrintOptions::default());
    }
}
