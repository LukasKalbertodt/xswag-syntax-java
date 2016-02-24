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
    IfThenElse {
        cond: Expr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    While {
        cond: Expr,
        body: Box<Statement>,
    },
    For {
        init: ForInit,
        cond: Option<Expr>,
        update: Vec<Statement>,
        body: Box<Statement>,
    },
    Break(Option<Ident>),
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

    // Logical operators ||  &&
    LogicalOr,
    LogicalAnd,

    // Bitwise operators |  &  ^
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,

    // Comparison
    Equals,
    Gt,
    Lt,
    Ge,
    Le,
    Ne,

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    ShrUn,
}

impl BinOpType {
    pub fn from_token(tok: &lex::Token) -> Option<Self> {
        use lex::Token::*;

        match *tok {
            // =   >   <   !   ~   ?   :   ->
            Eq => Some(BinOpType::Assign),
            Gt => Some(BinOpType::Gt),
            Lt => Some(BinOpType::Lt),

            // ==  >=  <=  !=  &&  ||  ++  --
            EqEq => Some(BinOpType::Equals),
            Ge => Some(BinOpType::Ge),
            Le => Some(BinOpType::Le),
            Ne => Some(BinOpType::Ne),
            AndAnd => Some(BinOpType::LogicalAnd),
            OrOr => Some(BinOpType::LogicalOr),

            // +   -   *   /   &   |   ^   %   <<   >>   >>>
            Plus => Some(BinOpType::Add),
            Minus => Some(BinOpType::Sub),
            Star => Some(BinOpType::Mul),
            Slash => Some(BinOpType::Div),
            And => Some(BinOpType::BitwiseAnd),
            Or => Some(BinOpType::BitwiseOr),
            Caret => Some(BinOpType::BitwiseXor),
            Percent => Some(BinOpType::Mod),
            Shl => Some(BinOpType::Shl),
            Shr => Some(BinOpType::Shr),
            ShrUn => Some(BinOpType::ShrUn),

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

#[derive(Clone, Debug)]
pub enum MethodInvocationType {
    SimpleName(Ident),
    SimplePath(Path),
    Expr(Box<Expr>, Ident),
}

#[derive(Clone, Debug)]
pub enum ForInit {
    VarDecl(Box<BlockStatement>),
    Stmts(Vec<Statement>),
}
