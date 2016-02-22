/// AST nodes that are part of a `Block`
///

pub use super::{
    Visibility,
    Ident,
    Path,
    Import,
    Type,
    Dims,
};


#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<BlockStatement>,
}

#[derive(Clone, Debug)]
pub enum BlockStatement {
    LocalVariableDecl {
        final_: bool,
        ty: Type,
        vars: Vec<(Ident, Dims)>,
    },
    // ClassDecl,
    // Statement,
}
