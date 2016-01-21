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
};

;
class Bread {}


";

fn main() {
    let file = base::code::FileMap::new("<test>", SRC);
    let (res, errors) = syntax::parse_compilation_unit(&file);

	match res {
		Ok(ast) => print!("{:#?}", ast),
		Err(e) => {
			// println!("{:#?}", e);
			diag::print(&e, &file, diag::PrintOptions::default());
		}
	}
    for e in &errors {
    	diag::print(&e, &file, diag::PrintOptions::default());
    }
    // println!("{:#?}", res);
    // println!("{:#?}", errors);
}
