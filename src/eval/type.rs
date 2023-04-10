use std::collections::HashMap;

use crate::ast::stmt::Function;

use super::value::BuiltInFunction;

pub const INTEGER_TYPE_ID       : usize = 0;
pub const FLOAT_TYPE_ID         : usize = 1;
pub const STRING_TYPE_ID        : usize = 2;
pub const BOOL_TYPE_ID          : usize = 3;
pub const LIST_TYPE_ID          : usize = 4;
pub const MAP_TYPE_ID           : usize = 5;
pub const RANGE_TYPE_ID         : usize = 6;
pub const METHOD_TYPE_ID        : usize = 7;
pub const FUNCTION_TYPE_ID      : usize = 8;
pub const NOTHING_TYPE_ID       : usize = 9;
pub const TYPE_TYPE_ID          : usize = 10;
pub const CHARACTER_TYPE_ID     : usize = 11;


pub enum Type {
    Def {
        members: Vec<String>,
        methods: HashMap<String, Function>,
    },
    BuiltInDef {
        members: Vec<String>,
        methods: HashMap<String, BuiltInFunction>,
    }
}