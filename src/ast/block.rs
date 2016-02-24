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
use lex;


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
    Expr(Expr),
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
    UnaryOp {
        op: UnaryOpType,
        expr: Box<Expr>,
    },

    // PrimaryNoNewArray
    Literal(lex::Lit),
    ClassLiteral(Type),
    Name(Path),
    This,
    // TypeNameThis,
    FieldAccess {
        root: Option<Box<Expr>>,
        path: Path,
    },
    ArrayAccess {
        obj: Box<Expr>,
        idx: Box<Expr>,
    },
    MethodInvocation {
        name: MethodInvocationType,
        args: Vec<Expr>,
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

#[derive(Clone, Copy, Debug)]
pub enum UnaryOpType {
    Plus,
    Neg,
    PreIncr,
    PreDecr,
    PostIncr,
    PostDecr,
    Not,
    BitwiseNot,
}

impl BinOpType {
    pub fn from_token(tok: &lex::Token) -> Option<Self> {
        use lex::Token::*;

        match *tok {
            // =   >   <   !   ~   ?   :   ->
            Eq => Some(BinOpType::Assign),
            Gt => None,
            Lt => None,
            Bang => None,
            Tilde => None,
            Question => None,
            Colon => None,
            Arrow => None,

            // ==  >=  <=  !=  &&  ||  ++  --
            EqEq => None,
            Ge => None,
            Le => None,
            Ne => None,
            AndAnd => Some(BinOpType::LogicalAnd),
            OrOr => Some(BinOpType::LogicalOr),
            PlusPlus => None,
            MinusMinus => None,

            // +   -   *   /   &   |   ^   %   <<   >>   >>>
            Plus => None,
            Minus => None,
            Star => None,
            Slash => None,
            And => Some(BinOpType::BitwiseAnd),
            Or => Some(BinOpType::BitwiseOr),
            Caret => Some(BinOpType::BitwiseXor),
            Percent => None,
            Shl => None,
            Shr => None,
            ShrUn => None,

            // +=  -=  *=  /=  &=  |=  ^=  %=  <<=  >>=  >>>=
            PlusEq => Some(BinOpType::AddAssign),
            MinusEq => Some(BinOpType::SubAssign),
            StarEq => Some(BinOpType::MulAssign),
            SlashEq => Some(BinOpType::DivAssign),
            AndEq => Some(BinOpType::AndAssign),
            OrEq => Some(BinOpType::OrAssign),
            CaretEq => Some(BinOpType::XorAssign),
            PercentEq => Some(BinOpType::ModAssign),
            ShlEq => Some(BinOpType::ShlAssign),
            ShrEq => Some(BinOpType::ShrAssign),
            ShrUnEq => Some(BinOpType::ShrUnAssign),

            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum MethodInvocationType {
    SimpleName(Ident),
    SimplePath(Path),
    Expr(Box<Expr>, Ident),
}
