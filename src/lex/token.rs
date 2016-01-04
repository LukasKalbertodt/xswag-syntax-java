//! This module defines basic token types
//!
//! The definition of Java tokens is mostly in section 3 (lexical structure) of
//! the Java language specification.
//!

use std::fmt::{Display, Formatter, Error};
use std::str::FromStr;
use base::code::Span;

// Macro to generate enums with helper methods
macro_rules! gen_helper {
    (
        $name:ident; ;
        $($variant:ident = $val:expr),+
    ) => { };
    (
        $name:ident;
        $helper:ident $(, $tail:ident)*;
        $($variant:ident = $val:expr),+
    ) => {
        $helper!($name; $($variant = $val),+ );
        gen_helper!($name; $($tail),*; $($variant = $val),+);
    };
}

macro_rules! gen_enum {
    (
        $(#[$attr:meta])*
        pub enum $name:ident;
        with $($helper:ident),* for:
        $($variant:ident = $val:expr),+
    ) => {
        $(
            #[$attr]
        )*
        pub enum $name {
            $($variant,)*
        }
        gen_helper!($name; $($helper),*; $( $variant = $val ),+);
    }
}

macro_rules! to_java_string {
    ($name:ident; $($variant:ident = $val:expr),+) => {
        impl $name {
            pub fn as_java_string(&self) -> &'static str {
                match self {
                    $( &$name::$variant => $val ,)*
                }
            }
        }
    }
}

macro_rules! display {
    ($name:ident; $($variant:ident = $val:expr),+) => {
        impl Display for $name {
            fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
                self.as_java_string().fmt(f)
            }
        }
    }
}

macro_rules! from_str {
    ($name:ident; $($variant:ident = $val:expr),+) => {
        impl FromStr for $name {
            type Err = ();
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($val => Ok($name::$variant), )*
                    _ => Err(()),
                }
            }
        }
    }
}


/// A token with it's span in the source text
#[derive(Debug, Clone, PartialEq)]
pub struct TokenSpan {
    /// The token
    pub tok: Token,
    /// Byte position of token in Filemap
    pub span: Span,
}

/// A Java token
///
/// This enum differs a bit from the original definition in the Java spec, in
/// which this `Token` is called *InputElement* and is defined as:
/// ```
/// WhiteSpace  |  Comment  |  Token
/// ```
/// The Java-*Token* is defined as:
/// ```
/// Identifier  |  Keyword  |  Literal  |  Seperator  |  Operator
/// ```
///
/// This `Token` type differs from the formal and correct definition to make
/// the parser and lexer module less verbose. The differences are:
/// - all 5 variants of the Java-*Token* are direct variants of this `Token`
/// - therefore the name Java-*Token* is not necessary and Java's
///   *InputElement* is called `Token` instead
/// - *Seperator*s and *Operator*s are also direct variants of this `Token`
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // Variants of the Java-*InputElement* (not "real" tokens)
    Whitespace,
    Comment,

    // Variants of the Java-*Token*
    Ident(String),
    KeyW(Keyword),
    Literal(Lit),

    // Variants of Java-*Seperator*
    // (   )   {   }   [   ]   ;   ,   .   ...   @   ::
    ParenOp,
    ParenCl,
    BraceOp,
    BraceCl,
    BracketOp,
    BracketCl,
    Semi,
    Comma,
    Dot,
    DotDotDot,
    At,
    ColonSep,

    // Variants of Java-*Operator*
    // =   >   <   !   ~   ?   :   ->
    Eq,
    Gt,
    Lt,
    Bang,
    Tilde,
    Question,
    Colon,
    Arrow,

    // ==  >=  <=  !=  &&  ||  ++  --
    EqEq,
    Ge,
    Le,
    Ne,
    AndAnd,
    OrOr,
    PlusPlus,
    MinusMinus,

    // +   -   *   /   &   |   ^   %   <<   >>   >>>
    Plus,
    Minus,
    Star,
    Slash,
    And,
    Or,
    Caret,
    Percent,
    Shl,
    Shr,
    ShrUn,

    // +=  -=  *=  /=  &=  |=  ^=  %=  <<=  >>=  >>>=
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    AndEq,
    OrEq,
    CaretEq,
    PercentEq,
    ShlEq,
    ShrEq,
    ShrUnEq,
}

impl Token {
    /// Returns true if the token is a "real" token (aka. a Java-*Token*)
    pub fn is_real(&self) -> bool {
        match *self {
            Token::Whitespace | Token::Comment => false,
            _ => true,
        }
    }

    /// String for error reporting. Example:
    /// ```
    /// Excpected one of `,` `;` `)`
    /// ```
    pub fn as_java_string(&self) -> &'static str {
        use self::Token::*;
        match self.clone() {
            Whitespace => "whitespace",
            Comment => "comment",

            Ident(_) => "identifier",
            KeyW(keyword) => keyword.as_java_string(),
            Literal(..) => "Lit(???)",

            ParenOp => "(",
            ParenCl => ")",
            BraceOp => "{",
            BraceCl => "}",
            BracketOp => "[",
            BracketCl => "]",
            Semi => ";",
            Comma => ",",
            Dot => ".",
            DotDotDot => "...",
            At => "@",
            ColonSep => "::",

            Eq => "=",
            Gt => ">",
            Lt => "<",
            Bang => "!",
            Tilde => "~",
            Question => "?",
            Colon => ":",
            Arrow => "->",

            EqEq => "==",
            Ge => ">=",
            Le => "<=",
            Ne => "!=",
            AndAnd => "&&",
            OrOr => "||",
            PlusPlus => "++",
            MinusMinus => "--",

            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            And => "&",
            Or => "|",
            Caret => "^",
            Percent => "%",
            Shl => "<<",
            Shr => ">>",
            ShrUn => ">>>",

            PlusEq => "+=",
            MinusEq => "-=",
            StarEq => "*=",
            SlashEq => "/=",
            AndEq => "&=",
            OrEq => "|=",
            CaretEq => "^=",
            PercentEq => "%=",
            ShlEq => "<<=",
            ShrEq => ">>=",
            ShrUnEq => ">>>=",
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.as_java_string())
    }
}

gen_enum! {
    /// Represents one of the Java keywords
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub enum Keyword;
    with to_java_string, display, from_str for:

    Abstract = "abstract",
    Assert = "assert",
    Boolean = "boolean",
    Break = "break",
    Byte = "byte",
    Case = "case",
    Catch = "catch",
    Char = "char",
    Class = "class",
    Const = "const",
    Continue = "continue",
    Default = "default",
    Do = "do",
    Double = "double",
    Else = "else",
    Enum = "enum",
    Extends = "extends",
    Final = "final",
    Finally = "finally",
    Float = "float",
    For = "for",
    If = "if",
    Goto = "goto",
    Implements = "implements",
    Import = "import",
    Instanceof = "instanceof",
    Int = "int",
    Interface = "interface",
    Long = "long",
    Native = "native",
    New = "new",
    Package = "package",
    Private = "private",
    Protected = "protected",
    Public = "public",
    Return = "return",
    Short = "short",
    Static = "static",
    Strictfp = "strictfp",
    Super = "super",
    Switch = "switch",
    Synchronized = "synchronized",
    This = "this",
    Throw = "throw",
    Throws = "throws",
    Transient = "transient",
    Try = "try",
    Void = "void",
    Volatile = "volatile",
    While = "while"
}

/// A Java literal
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    /// String literal, e.g. `"hi"`
    Str(String),
    /// Char literal, e.g. `'x'`
    Char(char),
    /// Integer literal, e.g. `0x27l`
    Integer {
        /// Literal as occured in source code (without type suffix and radix
        /// indicators)
        raw: String,
        /// If `l` type suffix was used.
        is_long: bool,
        /// Detected radix
        radix: u8
    },
    /// Floating point literal, e.g. `3.14e3f`
    Float {
        /// Float number without radix indicators
        raw: String,
        /// If the `f` was NOT used
        is_double: bool,
        /// Detected radix
        radix: u8,
        /// Exponent part without type suffix
        exp: String
    },
    /// Null literal `null`
    Null,
    /// Boolean literal `true` or `false`
    Bool(bool),
}
