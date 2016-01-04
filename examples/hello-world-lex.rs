extern crate xswag_syntax_java as syntax;
extern crate xswag_base as base;

fn main() {
    let f = base::code::FileMap::new("HelloWorld.java",
        include_str!("HelloWorld.java"));
    let lexer = syntax::lex::Tokenizer::new(&f);

    for res in lexer {
        println!("{:?}", res);
    }
}
