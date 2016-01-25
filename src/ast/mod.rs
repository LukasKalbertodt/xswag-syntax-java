// TODO: Remove
#![allow(dead_code)]

// TODO: make this private again, once #18241 is fixed
// https://github.com/rust-lang/rust/issues/18241
pub mod item;

pub use self::item::{
    ItemExt,
    Type,
    Interface,
    Class,
    Field,
    Method,
    TypeItem,
};

use std::fmt::{Display, Formatter, Error};
use base::code::{BytePos, Span};
use std::vec::Vec;
use std::default::Default;
use std::fmt;

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

// ============================================================================
// Definition of types that are common in AST nodes
// ============================================================================
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

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<Ident>,
}

// pub struct PathSegment {

// }



// ============================================================================
// Top-Down AST definition starting with the goal symbol
// ============================================================================
/// A Java compilation unit. This is the goal symbol for the syntactic grammar.
#[derive(Debug, Clone)]
pub struct CompilationUnit {
    pub package: Option<Path>,
    pub imports: Vec<Import>,
    pub types: Vec<Type>,
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
