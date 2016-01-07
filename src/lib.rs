extern crate xswag_base as base;
extern crate lalrpop_util;

pub mod lex;
pub mod ast;
pub mod grammar;


pub fn play() {
    // println!("{:#?}",
    //     grammar::import::parse_SingleTypeImportDecl("import peter"));
    // println!("{:#?}",
    //     grammar::import::parse_SingleTypeImportDecl("import Peter"));
    // let toks = vec![grammar::Tokis::Outer, grammar::Tokis::Outer,
    //     grammar::Tokis::Inner, grammar::Tokis::Outer, grammar::Tokis::Outer];
    let toks = vec![lex::Token::BraceOp, lex::Token::KeyW(lex::Keyword::Public), lex::Token::BraceCl];

    let file = base::code::FileMap::new("<test>",
        "public private class Cheese { private public public % yolo; }"
    );
    let lexer = lex::Tokenizer::new(&file)
        .take_while(|res| res.is_ok())
        .map(|res| { let ts = res.unwrap(); (ts.span.lo, ts.tok, ts.span.hi) })
        .filter(|t| t.1.is_real());

    let mut errors = Vec::new();

    println!("{:#?}", grammar::main::parse_CompilationUnit(&mut errors, lexer));
    println!("{:#?}", errors);
    // println!("{:?}", grammar::main::parse_CompilationUnit(toks.into_iter()));
}
