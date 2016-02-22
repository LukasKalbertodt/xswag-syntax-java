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

#[derive(Clone, Debug)]
pub struct Expr {
    pub expr: ExprType,
    pub span: Span,
}

impl Expr {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Clone, Debug)]
pub enum ExprType {
    Conditional {
        cond: Box<Expr>,
        if_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    BinOp {
        op: BinOpType,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum BinOpType {
    // Assignment [ =  *=  /=  %=  +=  -=  <<=  >>=  >>>=  &=  ^=  |= ]
    Assign,
    MulAssign,
    DivAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    ShlAssign,
    ShrAssign,
    ShrUnAssign,
    AndAssign,
    XorAssign,
    OrAssign,

    // Logical operators
    LogicalOr,
    LogicalAnd,

    // Bitwise operators
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
}
