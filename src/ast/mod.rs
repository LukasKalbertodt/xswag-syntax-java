// TODO: Remove
#![allow(dead_code)]

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

#[derive(Debug, Clone)]
pub enum Type {
    NormalClass(Class),
    Enum(()),
    NormalInterface(Interface),
}

#[derive(Debug, Clone)]
pub enum Item {
    Import(Import),
    Class(Box<Class>),
    Method(Box<Method>),
}


impl Default for Ident {
    fn default() -> Self {
        Ident {
            name: "".into(),
            span: Span { lo: BytePos(0), hi: BytePos(0) },
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
pub struct Interface {
    pub name: Ident,
    pub vis: Visibility,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Ident,
    pub vis: Visibility,
    pub methods: Vec<Method>,
    pub fields: Vec<Field>,
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

#[derive(Debug, Clone)]
pub struct Field {
    pub vis: Visibility,
    pub static_: bool,
    pub final_: bool,
    pub ty: String,
    pub name: Ident,
    // pub init ...
}

#[derive(Debug, Clone)]
pub struct Method {
    pub vis: Visibility,
    pub name: Ident,
    pub ret_ty: Ident,
    pub static_: bool,
    pub final_: bool,
    pub params: Vec<FormalParameter>,
}

#[derive(Debug, Clone)]
pub struct FormalParameter {
    pub ty: Ident,
    pub name: Ident,
    pub dims: usize,
    pub final_: bool,
}
