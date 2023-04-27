use std::collections::HashMap;

use super::value::BuiltInFunction;

pub enum BuiltInType {
    Integer,
    Float,
    String,
    Bool,
    List,
    Map,
    Range,
    Function,
    Nothing,
    Type,
    Module,
    Character,

    CustomStartId
}

#[derive(Clone)]
pub struct Type {
    pub members: Vec<String>,
    pub methods: HashMap<String, usize>,
    pub builtin_methods: Option<HashMap<String, BuiltInFunction>>,
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}