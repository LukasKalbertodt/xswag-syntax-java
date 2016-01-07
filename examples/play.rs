extern crate xswag_syntax_java as syntax;
extern crate xswag_base as base;

fn main() {
    let file = base::code::FileMap::new("<test>",
        "public private class Cheese { private public % yolo; }"
    );
    let (res, errors) = syntax::parse_compilation_unit(&file);

    println!("{:#?}", res);
    println!("{:#?}", errors);
}
