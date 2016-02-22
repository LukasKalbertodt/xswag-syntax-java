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
use base::code::Span;


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
    Statement(Statement),
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub label: Option<Ident>,
    pub stmt: StatementType,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StatementType {
    Empty,
    Block(Block),
}
