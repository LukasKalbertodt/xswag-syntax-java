// TODO: Remove
#![allow(dead_code)]

// TODO: make this private again, once #18241 is fixed
// https://github.com/rust-lang/rust/issues/18241
pub mod item;
pub mod block;

pub use self::item::{
    ItemExt,
    TypeDef,
    Interface,
    Class,
    ClassMember,
    Field,
    Method,
    TypeItem,
    FormalParameter,
};

pub use self::block::{
    Block,
    BlockStatement,
    Statement,
    StatementType,
    Expr,
    ExprType,
    BinOpType,
    UnaryOpType,
    MethodInvocationType,
    ForInit,
    VariableDeclarator,
    SwitchArm,
    SwitchLabel,
};

use std::fmt::{Display, Formatter, Error};
use base::code::{BytePos, Span};
use std::vec::Vec;
use std::default::Default;
use std::fmt;
use std::marker;
use std::ops;

macro_rules! java_enum { (
    $name:ident { $( $variant:ident => $java_word:expr, )* }
) => {
    #[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
    pub enum $name {
        $( $variant, )*
    }

    impl $name {
        pub fn as_java_string(&self) -> &str {
            match *self {
                $( $name::$variant => $java_word , )*
            }
        }
    }

    impl Display for $name {
        fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
            self.as_java_string().fmt(f)
        }
    }
}}

pub type Dims = u16;

// ============================================================================
// Definition of types that are common in AST nodes
// ============================================================================
#[derive(Clone, Debug)]
pub struct Spanned<T: Clone + fmt::Debug> {
    pub inner: T,
    pub span: Span,
}

impl<T: Clone + fmt::Debug> Spanned<T> {
    pub fn map<F, U>(self, f: F) -> Spanned<U>
        where F: FnOnce(T) -> U,
              U: Clone + fmt::Debug
    {
        Spanned {
            inner: f(self.inner),
            span: self.span,
        }
    }
}

impl Into<Expr> for Spanned<ExprType> {
    fn into(self) -> Expr {
        Expr {
            expr: self.inner,
            span: self.span,
        }
    }
}

impl Into<Expr> for Spanned<Expr> {
    fn into(self) -> Expr {
        self.inner
    }
}

impl Into<Box<Expr>> for Spanned<ExprType> {
    fn into(self) -> Box<Expr> {
        Box::new(Expr {
            expr: self.inner,
            span: self.span,
        })
    }
}

impl<T> Into<Ident> for Spanned<T> where T: Into<String> + Clone + fmt::Debug {
    fn into(self) -> Ident {
        Ident {
            name: self.inner.into(),
            span: self.span,
        }
    }
}

impl<T> marker::Copy for Spanned<T> where T: Copy + fmt::Debug {}

#[derive(Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

// custom `Debug` impl to shorten debug output and improve readability
impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, r#"Ident("{}" @ ({}, {}))"#,
            self.name, self.span.lo.0, self.span.hi.0)
    }
}

#[derive(Clone)]
pub struct Path {
    pub segments: Vec<Ident>,
}

impl Path {
    pub fn single(name: Ident) -> Path {
        Path {
            segments: vec![name],
        }
    }

    pub fn span(&self) -> Option<Span> {
        match (self.segments.first(), self.segments.last()) {
            (Some(first), Some(last)) => Some(Span {
                lo: first.span.lo,
                hi: last.span.hi
            }),
            _ => None,
        }
    }
}

impl ops::Add for Path {
    type Output = Path;
    fn add(mut self, mut rhs: Path) -> Self::Output {
        self.segments.append(&mut rhs.segments);
        self
    }
}

impl ops::Add<Ident> for Path {
    type Output = Path;
    fn add(mut self, rhs: Ident) -> Self::Output {
        self.segments.push(rhs);
        self
    }
}

impl ops::Add<Path> for Ident {
    type Output = Path;
    fn add(self, mut rhs: Path) -> Self::Output {
        rhs.segments.insert(0, self);
        rhs
    }
}

impl ops::Add for Ident {
    type Output = Path;
    fn add(self, rhs: Ident) -> Self::Output {
        Path {
            segments: vec![self, rhs],
        }
    }
}

// custom `Debug` impl to shorten debug output and improve readability
impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(span) = self.span() {
            let mut p = self.segments
                .first()
                .map(|f| f.name.clone())
                .unwrap_or_default();
            for seg in &self.segments[1..] {
                p.push('.');
                p.push_str(&seg.name);
            }

            write!(f, r#"Path("{}" @ ({}, {}))"#, p, span.lo.0, span.hi.0)
        } else {
            write!(f, "Path(EMPTY)")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub name: Path,
    pub dims: Dims,
}

impl Type {
    pub fn without_dims(name: Path) -> Type {
        Type {
            name: name,
            dims: 0,
        }
    }

    pub fn map_dims<F>(self, f: F) -> Type
        where F: FnOnce(Dims) -> Dims
    {
        Type {
            name: self.name,
            dims: f(self.dims),
        }
    }
}


// ============================================================================
// Top-Down AST definition starting with the goal symbol
// ============================================================================
/// A Java compilation unit. This is the goal symbol for the syntactic grammar.
#[derive(Debug, Clone)]
pub struct CompilationUnit {
    pub package: Option<Path>,
    pub imports: Vec<Import>,
    pub types: Vec<TypeDef>,
}

impl Default for Ident {
    fn default() -> Self {
        Ident {
            name: "".into(),
            span: Span::dummy(),
        }
    }
}

/// A import declaration
#[derive(Debug, Clone)]
pub enum Import {
    /// e.g. `import IO.AlgoTools;`
    SingleType(Path),
    /// called "type-import-on-demand" in specs -- e.g. `import IO.*;`
    TypeOnDemand(Path),
    SingleStatic(Path),
    StaticOnDemand(Path),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Protected,
    Package,
    Private,
}

#[derive(Debug, Clone)]
pub struct Name {
    // for qualified names
    pub path: Vec<Ident>,
    pub last: Option<Ident>,
}

java_enum! (Modifier {
    Public => "public",
    Protected => "protected",
    Private => "private",
    Abstract => "abstract",
    Static => "static",
    Final => "final",
    Synchronized => "synchronized",
    Native => "native",
    Strictfp => "strictfp",
    Transient => "transient",
    Volatile => "volatile",
});
