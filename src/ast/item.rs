/// AST nodes that represent an "item". An item can be owned by other things.
/// Items include: interfaces, classes, methods, ...
///

pub use super::{
    Visibility,
    Ident,
    Path,
    Import,
    Type,
    Block,
};

/// This trait provides generic access to several properties of items
pub trait ItemExt {
    /// identifier of the item, if any
    fn ident(&self) -> Option<&Ident> { None }
    // visibility of the item, if any
    fn vis(&self) -> Visibility;
    // if the item is static or `None` if that property is not set
    fn static_(&self) -> Option<bool> { None }
}

// // ----- macros to help implement `ItemExt`  -----
// macro_rules! forward_func {
//     ($func_name:ident -> $ret:ty; $($variant:ident),*) => {
//         fn $func_name(&self) -> $ret {
//             match *self {
//                 $($variant(ref inner) => inner.func_name()),*
//             }
//         }
//     }
// }

// macro_rules! impl_ext_for_item_enum {
//     ($name:ident; $($variant:ident),*) => {
//         impl TypeExt for $name {
//             forward_func!(ident -> Option<&Ident>; $($name::$ident),*)
//             forward_func!(vis -> Visibility; $($name::$ident),*)
//             forward_func!(static_ -> Option<bool>; $($name::$ident),*)
//         }
//     }
// }



/// Represents a Java type
#[derive(Debug, Clone)]
pub enum TypeDef {
    /// A Java class, defined with `class`
    NormalClass(Class),
    // Enum(()),
    /// A Java interface, defined with `interface`
    NormalInterface(Interface),
}


impl ItemExt for TypeDef {
    fn ident(&self) -> Option<&Ident> {
        match *self {
            TypeDef::NormalClass(ref c) => c.ident(),
            TypeDef::NormalInterface(ref i) => i.ident(),
        }
    }
    fn vis(&self) -> Visibility {
        match *self {
            TypeDef::NormalClass(ref c) => c.vis(),
            TypeDef::NormalInterface(ref i) => i.vis(),
        }
    }
    fn static_(&self) -> Option<bool> {
        match *self {
            TypeDef::NormalClass(ref c) => c.static_(),
            TypeDef::NormalInterface(ref i) => i.static_(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Import(Import),
    Class(Box<Class>),
    Method(Box<Method>),
    Constant(()),
}



pub enum TypeItem {
    Type(TypeDef),
    Constant(Field),
    Method(Method),
}


#[derive(Debug, Clone)]
pub struct Interface {
    pub name: Ident,
    pub vis: Visibility,
    pub static_: bool,
    pub strictfp: bool,
    pub extends: Vec<Type>,
    pub types: Vec<TypeDef>,
    pub constants: Vec<Field>,
    pub methods: Vec<Method>,
}

impl ItemExt for Interface {
    fn ident(&self) -> Option<&Ident> { Some(&self.name) }
    fn vis(&self) -> Visibility { self.vis }
    fn static_(&self) -> Option<bool> { Some(self.static_) }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Ident,
    pub vis: Visibility,
    pub members: Vec<ClassMember>,
}

impl ItemExt for Class {
    fn ident(&self) -> Option<&Ident> { Some(&self.name) }
    fn vis(&self) -> Visibility { self.vis }
    fn static_(&self) -> Option<bool> { Some(unimplemented!()) }
}

#[derive(Clone, Debug)]
pub enum ClassMember {
    Method(Method),
    Field(Field),
}

#[derive(Debug, Clone)]
pub struct Field {
    pub vis: Visibility,
    pub static_: bool,
    pub final_: bool,
    pub ty: Type,
    pub name: Ident,
    // pub init ...
}

#[derive(Debug, Clone)]
pub struct Method {
    pub vis: Visibility,
    pub name: Ident,
    pub ret_ty: Type,
    pub static_: bool,
    pub final_: bool,
    pub strictfp: bool,
    pub abstract_: bool,
    pub native: bool,
    pub synchronized: bool,
    pub default: bool,
    pub params: Vec<FormalParameter>,
    pub block: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct FormalParameter {
    pub ty: Type,
    pub name: Ident,
    pub final_: bool,
}
