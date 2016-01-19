extern crate xswag_syntax_java as syntax;
extern crate xswag_base as base;

const SRC: &'static str = "\
package yolo.swag;

public private class Cheese { private public % yolo; }


";

fn main() {
    let file = base::code::FileMap::new("<test>", SRC);
    let (res, errors) = syntax::parse_compilation_unit(&file);

    println!("{:#?}", res);
    println!("{:#?}", errors);
}
