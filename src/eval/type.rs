use std::collections::HashSet;

use Type::*;

#[derive(Clone)]
pub enum Type {
    IntegerTy,
    FloatTy,
    StringTy,
    CharTy,
    BoolTy,
    ListTy,
    MapTy,
    RangeTy,
    FunTy,
    NothingTy,
    CustomTy {
        name: String,
        mems: Vec<String>
    },
    TyTy,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerTy => write!(f, "Integer"),
            FloatTy   => write!(f, "Float"),
            StringTy  => write!(f, "String"),
            CharTy    => write!(f, "Character"),
            BoolTy    => write!(f, "Bool"),
            ListTy    => write!(f, "List"),
            MapTy     => write!(f, "Map"),
            RangeTy   => write!(f, "Range"),
            FunTy     => write!(f, "Function"),
            NothingTy => write!(f, "Nothing"),
            TyTy      => write!(f, "Type"),
            
            CustomTy { name, mems: _ } => write!(f, "{name}"),
        }
    }
}

impl std::cmp::PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::CustomTy { name: l_name, mems: l_mems }, 
             Self::CustomTy { name: r_name, mems: r_mems }) => {
                l_name == r_name && HashSet::<String>::from_iter(l_mems.iter().cloned()) == HashSet::from_iter(r_mems.iter().cloned())
            },
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}