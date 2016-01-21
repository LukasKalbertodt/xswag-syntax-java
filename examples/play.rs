extern crate xswag_syntax_java as syntax;
extern crate xswag_base as base;

use base::diag;

// FIXME: import is currently wrong
const SRC: &'static str = "\
package yolo.swag;

import AlgoTools.IO;
import java.lang-*;

public private class Cheese {
	private public % yolo;
}


";

fn main() {
    let file = base::code::FileMap::new("<test>", SRC);
    let (res, errors) = syntax::parse_compilation_unit(&file);

    println!("{:#?}", res);
    for e in &errors {
    	diag::print(&e, &file, diag::PrintOptions::default());
    }
    // println!("{:#?}", errors);
}
