use std::collections::HashMap;

use crate::{
    ast::stmt::{ Stmt, self }, 
    span::Spanned, error::error::{ Error, Res }
};

use super::{
    evaluator::{ Engine, State }, 
    r#type::*
};

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum KeyValue {
    IntegerKey(i32),
    StringKey(String),
    BoolKey(bool),
    NothingKey,
}

impl std::fmt::Display for KeyValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeyValue::IntegerKey(int) => write!(f, "{int}"),
            KeyValue::StringKey(s)    => write!(f, "\"{s}\""),
            KeyValue::BoolKey(b)      => write!(f, "{b}"),
            KeyValue::NothingKey      => write!(f, "Nothing"),
        }
    }
}

impl TryInto<KeyValue> for Value {
    type Error = String;

    fn try_into(self) -> Result<KeyValue, Self::Error> {
        match self {
            Value::Integer(int) => Ok(KeyValue::IntegerKey(int)),
            Value::String(s)    => Ok(KeyValue::StringKey(s)),
            Value::Bool(b)      => Ok(KeyValue::BoolKey(b)),
            Value::Nothing      => Ok(KeyValue::NothingKey),
            _ => Err(format!("`{ty}` is not hashable.", ty = self.ty()))
        }
    }
}

impl Into<Value> for KeyValue {
    fn into(self) -> Value {
        match self {
            KeyValue::IntegerKey(int) => Value::Integer(int),
            KeyValue::StringKey(s)    => Value::String(s),
            KeyValue::BoolKey(b)      => Value::Bool(b),
            KeyValue::NothingKey      => Value::Nothing,
        }
    }
}

#[derive(Clone, Copy)]
pub struct BuiltInFunction {
    pub arity: usize,
    pub fun: fn(vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)>
}

#[derive(Clone)]
pub struct Function {
    pub args: Vec<String>,
    pub body: Vec<Spanned<Stmt>>,
}

impl Into<(String, Function)> for &Spanned<stmt::Function> {
    fn into(self) -> (String, Function) {
        let data = self.clone().data;
        (data.name, Function {
            args: data.args,
            body: data.body,
        })
    }
} 

#[derive(Clone)]
pub enum Value {
    Integer(i32),
    Float(f32),
    String(String),
    Char(char),
    Bool(bool),
    List(Vec<Value>),
    Map(HashMap<KeyValue, Value>),
    Range(i32, i32),
    Instance {
        type_id: usize, 
        members: HashMap<String, Value>,
    },
    Function {
        fun: Function,
        value: Option<Box<Value>>, // If Method
        closure: Option<HashMap<String, Value>> // If Closure
    },
    BuiltInFunction {
        fun: BuiltInFunction,
        value: Option<Box<Value>>, // If Method
    },
    Nothing,
    Type(usize),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(int)    => write!(f, "{int}"),
            Value::Float(float)    => write!(f, "{float}"),
            Value::String(s)       => write!(f, "{s}"),
            Value::Char(ch)        => write!(f, "{ch}"),
            Value::Bool(b)         => write!(f, "{b}"),
            Value::List(list)      => {
                if list.len() == 0 {
                    return write!(f, "[]")
                }
                write!(f, "[")?;
                write!(f, "{f}", f = list.first().unwrap())?;
                for value in &list[1..] {
                    write!(f, ", {value}")?;
                }
                write!(f, "]")?;
                Ok(())
            },
            Value::Map(map) => {
                if map.len() == 0 {
                    return write!(f, "#[]")
                }
                let mut first = true;
                write!(f, "#[")?;
                for (key, value) in map {
                    if first {
                        write!(f, "{key}: {value}")?;
                        first = false;
                    } else {
                        write!(f, ", {key}: {value}")?;
                    }
                }
                write!(f, "]")?;
                Ok(())
            },
            Value::Instance { type_id, members } => {
                write!(f, "{type_id} ")?;
                let mut first = true;
                for (key, value) in members {
                    if first {
                        write!(f, "{key}: {value}")?;
                        first = false;
                    } else {
                        write!(f, ", {key}: {value}")?;
                    }
                }
                Ok(())
            },
            Value::Range(bot, up)     => write!(f, "{bot}..{up}"),
            Value::Function {..}        => write!(f, "<function>"),
            Value::BuiltInFunction {..} => write!(f, "<built-in function>"),
            Value::Nothing            => write!(f, "nothing"),
            Value::Type(ty)           => write!(f, "{ty}"),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Value {
    pub fn ty(&self) -> usize {
        match self {
            Value::Integer(_)         => INTEGER_TYPE_ID,
            Value::Float(_)           => FLOAT_TYPE_ID,
            Value::String(_)          => STRING_TYPE_ID,
            Value::Bool(_)            => BOOL_TYPE_ID,
            Value::List(_)            => LIST_TYPE_ID,
            Value::Map(_)             => MAP_TYPE_ID,
            Value::Range(_, _)        => RANGE_TYPE_ID,
            Value::Function {..}        |
            Value::BuiltInFunction {..} => FUNCTION_TYPE_ID,
            Value::Nothing            => NOTHING_TYPE_ID,
            Value::Type(_)            => TYPE_TYPE_ID,
            
            Value::Instance { type_id, .. } => *type_id,
            
            Value::Char(_)            => CHARACTER_TYPE_ID,
            
        }
    }

    pub fn as_integer(&self) -> Result<i32, String> {
        match self {
            Value::Integer(int) => Ok(*int),
            _ => Err(format!("Expected Integer, got {}.", self.ty()))
        }
    }

    pub fn as_string(&self) -> Result<String, String> {
        match self {
            Value::String(s) => Ok(s.clone()),
            _ => Err(format!("Expected String, got {}.", self.ty()))
        }
    }

    pub fn as_char(&self) -> Result<char, String> {
        match self {
            Value::Char(ch) => Ok(*ch),
            _ => Err(format!("Expected Character, got {}.", self.ty()))
        }
    }

    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(format!("Expected Bool, got {}.", self.ty()))
        }
    }

    pub fn as_type(&self) -> Result<usize, String> {
        match self {
            Value::Type(id) => Ok(*id),
            _ => Err(format!("Expected Type, got {}.", self.ty()))
        }
    }

    pub fn as_instance(&self) -> Result<(usize, HashMap<String, Value>), String> {
        match self {
            Value::Instance { type_id, members } => Ok((*type_id, members.clone())),
            _ => Err(format!("Expected a `Definition`, got {}.", self.ty()))
        }
    }
}

pub type ExprResult = Result<Value, String>;

impl Value {
    pub fn indexable(self) -> ExprResult {
        match self {
            Value::String(_) |
            Value::List(_)   |
            Value::Map(_)    => Ok(self),
            _ => Err(format!("`{ty}` is not indexable.", ty = self.ty()))
        }
    }

    pub fn iter(self) -> Result<Box<dyn Iterator<Item = Value>>, String> {
        match self {
            Value::String(s)  => {
                let iter = s.chars().map(|ch| {
                    Value::Char(ch)
                }).collect::<Vec<_>>().into_iter();
                Ok(Box::new(iter))
            },
            Value::List(list) => Ok(Box::new(list.into_iter())),
            Value::Map(map)   => {
                let iter = map.into_iter().map(|(key, value)| {
                    Value::List(vec![key.into(), value])
                }).collect::<Vec<_>>().into_iter();
                Ok(Box::new(iter))
            },
            Value::Range(bot, up) => {
                let iter = (bot..up).into_iter().map(|n| {
                    Value::Integer(n)
                });
                Ok(Box::new(iter))
            },
            _ => Err(format!("`{ty}` is not iterable.", ty = self.ty()))
        }
    }

    pub fn replace(&self, index: Value, val: Value) -> ExprResult {
        match self {
            Value::String(s)  => {
                let index = index.as_integer()?;
                let index = if index < 0 {
                    s.len() as i32 + index
                } else {
                    index
                };
                let v = val.as_char()?; 
                if s.len() < index as usize {
                    return Err("Index out of bounds".to_string())
                }
                let s = s.chars().enumerate().map(|(i, ch)| {
                    if i == index as usize { v } else { ch }
                }).collect();
                Ok(Value::String(s))
            },
            Value::List(list) => {
                let index = index.as_integer()?;
                let index = if index < 0 {
                    list.len() as i32 + index
                } else {
                    index
                };
                let mut list = list.clone();
                match list.get_mut(index as usize) {
                    Some(v) => {
                        *v = val;
                        Ok(Value::List(list))
                    },
                    None => Err("Index out of bounds.".to_string())
                }
            },
            Value::Map(map) => {
                let key_value: KeyValue = match TryInto::try_into(index) {
                    Ok(kv) => kv,
                    Err(_) => return Err(format!("Invalid Key.").to_string()),
                };
                let mut map = map.clone();                
                match map.insert(key_value.clone(), val) {
                    Some(_) => Ok(Value::Map(map)),
                    None => return Err(format!("Absent Key {key_value}.")),
                }
            }
            _ => unreachable!()
        }
    }

    pub fn get_method(&self, method: &str, engine: &mut Engine) -> Result<Value, String> {
        match engine.get_type(self.ty()) {
            Type::Def { methods, .. } => {
                if let Some(method) = methods.get(method) {
                    return Ok(Value::Function { 
                        fun: method.clone(), 
                        value: Some(Box::new(self.clone())),
                        closure: None
                    })
                }
            }
            Type::BuiltInDef { builtin_methods, methods, .. } => {
                if let Some(method) = builtin_methods.get(method) {
                    return Ok(Value::BuiltInFunction { 
                        fun: method.clone(), 
                        value: Some(Box::new(self.clone())),
                    })
                }

                if let Some(method) = methods.get(method) {
                    return Ok(Value::Function { 
                        fun: method.clone(),
                        value: Some(Box::new(self.clone())), 
                        closure: None
                    })
                }
            }
        }

        let Some(method) = engine.get_impl(method) else {
            return Err(format!("`{ty}` does not implement `{method}`.", ty = self.ty()))
        };

        Ok(Value::Function { 
            fun: method, 
            value: Some(Box::new(self.clone())), 
            closure: None 
        })
    } 

    // When fails, returns a tuple containing
    //      (String, Option<Error>)
    // Error string of which caused by expression itself  
    // An optionally error, if the execution of application didn't succeed 
    pub fn apply(&self, mut vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)> {
        fn handle<T>(res: Res<T>) -> Result<T, (String, Option<Error>)> {
            res.map_err(|err| ("Error while evaluating expression".to_string(), Some(err)))
        }

        let check_arity = |expected| -> Result<(), (String, Option<Error>)> {
            if vals.len() !=  expected {
                Err((format!("Expected {expected} arguments but given {given}", given = vals.len()), None))
            } else {
                Ok(())
            }
        }; 

        match self {
            Value::Function { fun: Function { args, body }, value: self_value, closure } => {
                check_arity(args.len())?;                
                if body.is_empty() {
                    return Ok(Value::Nothing)
                }

                engine.enter_scope();

                if let Some(value) = self_value {
                    engine.define(String::from("self"), *value.clone());
                }

                if let Some(closure) = closure {
                    for (arg, value) in closure {
                        engine.define(arg.to_string(), value.clone());
                    }
                }
                
                handle(engine.collect_definitions(body))?;
                for (arg, v) in std::iter::zip(args, vals) {
                    engine.define(arg.to_string(), v);
                }
                
                for stmt in &body[..body.len() - 1] {
                    if let State::Return(val) = handle(engine.run(stmt))? {
                        engine.exit_scope();
                        return Ok(val)
                    }
                }
                
                // If the last statement is an expression return the value
                let stmt = body.last().unwrap();                
                let rv = match &stmt.data {
                    Stmt::Expr(e) => handle(engine.eval(e))?,
                    _ => match handle(engine.run(stmt))? {
                        State::Return(val) => val,
                        _ => Value::Nothing,
                    }
                };
                engine.exit_scope();
                
                Ok(rv)
            },
            
            Value::BuiltInFunction { fun, value: self_value } => {
                check_arity(fun.arity)?;
                if let Some(value) = self_value {
                    vals.push(*value.clone());
                }
                (fun.fun)(vals, engine)
            }
                
            Value::Type(id) => {
                let (Type::Def       { members, .. }| 
                    Type::BuiltInDef { members, .. }) = engine.get_type(*id);
                check_arity(members.len())?;
                let mut map = HashMap::new(); 
                for (member, v) in std::iter::zip(members, vals) {
                    map.insert(member.to_string(), v);
                }
                Ok(Value::Instance { type_id: *id, members: map })
            },
            _ => Err((format!("`{ty}` is not applicable.", ty = self.ty()), None))
        }
    }
}
