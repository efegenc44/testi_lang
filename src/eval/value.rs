use std::collections::HashMap;

use Value::*;

use crate::{
    ast::stmt::{Stmt, Function}, 
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
            IntegerVal(int) => Ok(KeyValue::IntegerKey(int)),
            StringVal(s)    => Ok(KeyValue::StringKey(s)),
            BoolVal(b)      => Ok(KeyValue::BoolKey(b)),
            NothingVal      => Ok(KeyValue::NothingKey),
            _ => Err(format!("`{ty}` is not hashable.", ty = self.ty()))
        }
    }
}

impl Into<Value> for KeyValue {
    fn into(self) -> Value {
        match self {
            KeyValue::IntegerKey(int) => IntegerVal(int),
            KeyValue::StringKey(s)    => StringVal(s),
            KeyValue::BoolKey(b)      => BoolVal(b),
            KeyValue::NothingKey      => NothingVal,
        }
    }
}

#[derive(Clone, Copy)]
pub struct BuiltInFunction {
    pub arity: usize,
    pub fun: fn(vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)>
}

#[derive(Clone)]
pub enum Value {
    IntegerVal(i32),
    FloatVal(f32),
    StringVal(String),
    CharVal(char),
    BoolVal(bool),
    ListVal(Vec<Value>),
    MapVal(HashMap<KeyValue, Value>),
    RangeVal(i32, i32),
    InstanceVal {
        type_id: usize, 
        members: HashMap<String, Value>,
    },
    MethodVal {
        value: Box<Value>,
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>,
    },
    FunVal {
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>,
    },
    ClosureVal {
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>,
        closure: HashMap<String, Value>
    },
    BuiltInMethodVal {
        value: Box<Value>,
        arity: usize,
        fun: fn(vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)>
    },
    BuiltInFunVal {
        arity: usize,
        fun: fn(vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)>
    },
    NothingVal,
    TypeVal(usize),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerVal(int)    => write!(f, "{int}"),
            FloatVal(float)    => write!(f, "{float}"),
            StringVal(s)       => write!(f, "{s}"),
            CharVal(ch)        => write!(f, "{ch}"),
            BoolVal(b)         => write!(f, "{b}"),
            ListVal(list)      => {
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
            MapVal(map) => {
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
            InstanceVal { type_id, members } => {
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
            RangeVal(bot, up)     => write!(f, "{bot}..{up}"),
            MethodVal {..}        => write!(f, "<method>"),
            FunVal {..}           => write!(f, "<function>"),
            ClosureVal {..}       => write!(f, "<function>"),
            BuiltInFunVal {..}    => write!(f, "<built-in function>"),
            BuiltInMethodVal {..} => write!(f, "<built-in method>"),
            NothingVal            => write!(f, "nothing"),
            TypeVal(ty)           => write!(f, "{ty}"),
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
            IntegerVal(_)         => INTEGER_TYPE_ID,
            FloatVal(_)           => FLOAT_TYPE_ID,
            StringVal(_)          => STRING_TYPE_ID,
            BoolVal(_)            => BOOL_TYPE_ID,
            ListVal(_)            => LIST_TYPE_ID,
            MapVal(_)             => MAP_TYPE_ID,
            RangeVal(_, _)        => RANGE_TYPE_ID,
            MethodVal {..}        |
            BuiltInMethodVal {..} => METHOD_TYPE_ID,
            FunVal {..}           |
            ClosureVal {..}       |
            BuiltInFunVal {..}    => FUNCTION_TYPE_ID,
            NothingVal            => NOTHING_TYPE_ID,
            TypeVal(_)            => TYPE_TYPE_ID,
            
            InstanceVal { type_id, .. } => *type_id,
            
            CharVal(_)            => CHARACTER_TYPE_ID,
            
        }
    }

    pub fn as_integer(&self) -> Result<i32, String> {
        match self {
            IntegerVal(int) => Ok(*int),
            _ => Err(format!("Expected Integer, got {}.", self.ty()))
        }
    }

    pub fn as_string(&self) -> Result<String, String> {
        match self {
            StringVal(s) => Ok(s.clone()),
            _ => Err(format!("Expected String, got {}.", self.ty()))
        }
    }

    pub fn as_char(&self) -> Result<char, String> {
        match self {
            CharVal(ch) => Ok(*ch),
            _ => Err(format!("Expected Character, got {}.", self.ty()))
        }
    }

    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            BoolVal(b) => Ok(*b),
            _ => Err(format!("Expected Bool, got {}.", self.ty()))
        }
    }

    pub fn as_type(&self) -> Result<usize, String> {
        match self {
            TypeVal(id) => Ok(*id),
            _ => Err(format!("Expected Type, got {}.", self.ty()))
        }
    }

    pub fn as_instance(&self) -> Result<(usize, HashMap<String, Value>), String> {
        match self {
            InstanceVal { type_id, members } => Ok((*type_id, members.clone())),
            _ => Err(format!("Expected a `Definition`, got {}.", self.ty()))
        }
    }
}

pub type ExprResult = Result<Value, String>;

impl Value {
    pub fn len(&self) -> Result<usize, String> {
        match self {
            StringVal(s)  => Ok(s.chars().count()),
            ListVal(list) => Ok(list.len()),
            _ => Err(format!("`{}` has no len attribute.", self.ty()))
        }
    }

    pub fn indexable(self) -> ExprResult {
        match self {
            StringVal(_) |
            ListVal(_)   |
            MapVal(_)    => Ok(self),
            _ => Err(format!("`{ty}` is not indexable.", ty = self.ty()))
        }
    }

    pub fn iter(self) -> Result<Box<dyn Iterator<Item = Value>>, String> {
        match self {
            StringVal(s)  => {
                let iter = s.chars().map(|ch| {
                    CharVal(ch)
                }).collect::<Vec<_>>().into_iter();
                Ok(Box::new(iter))
            },
            ListVal(list) => Ok(Box::new(list.into_iter())),
            MapVal(map)   => {
                let iter = map.into_iter().map(|(key, value)| {
                    ListVal(vec![key.into(), value])
                }).collect::<Vec<_>>().into_iter();
                Ok(Box::new(iter))
            },
            RangeVal(bot, up) => {
                let iter = (bot..up).into_iter().map(|n| {
                    IntegerVal(n)
                });
                Ok(Box::new(iter))
            },
            _ => Err(format!("`{ty}` is not iterable.", ty = self.ty()))
        }
    }

    pub fn replace(&self, index: Value, val: Value) -> ExprResult {
        match self {
            StringVal(s)  => {
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
                Ok(StringVal(s))
            },
            ListVal(list) => {
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
                        Ok(ListVal(list))
                    },
                    None => Err("Index out of bounds.".to_string())
                }
            },
            MapVal(map) => {
                let key_value: KeyValue = match TryInto::try_into(index) {
                    Ok(kv) => kv,
                    Err(_) => return Err(format!("Invalid Key.").to_string()),
                };
                let mut map = map.clone();                
                match map.insert(key_value.clone(), val) {
                    Some(_) => Ok(MapVal(map)),
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
                    return Ok(MethodVal { 
                        value: Box::new(self.clone()), 
                        args: method.args.clone(),
                        body: method.body.clone()
                    })
                }
            }
            Type::BuiltInDef { methods, .. } => {
                if let Some(method) = methods.get(method) {
                    return Ok(BuiltInMethodVal { 
                        value: Box::new(self.clone()), 
                        arity: method.arity, 
                        fun: method.fun 
                    })
                }
            }
        }

        let Some(Function { args, body, .. }) = engine.get_impl(method) else {
            return Err(format!("`{ty}` does not implement `{method}`.", ty = self.ty()))
        };

        Ok(MethodVal { 
            value: Box::new(self.clone()), 
            args, 
            body 
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
            FunVal     { args, body }     |
            MethodVal  { args, body, .. } |
            ClosureVal { args, body, .. } => {
                check_arity(args.len())?;                
                if body.is_empty() {
                    return Ok(NothingVal)
                }

                engine.enter_scope();
                match self {
                    ClosureVal { closure, .. } => 
                        for (arg, v) in closure {
                            engine.define(arg.to_string(), v.to_owned());
                        },
                    
                    MethodVal { value, .. } => engine.define(String::from("self"), *value.to_owned()),
                    _ => (),
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
                    Stmt::ExprStmt(e) => handle(engine.eval(e))?,
                    _ => match handle(engine.run(stmt))? {
                        State::Return(val) => val,
                        _ => NothingVal,
                    }
                };
                engine.exit_scope();
                
                Ok(rv)
            },
            
            BuiltInFunVal    { arity, fun }     |
            BuiltInMethodVal { arity, fun, .. } => {
                check_arity(*arity)?;
                if let BuiltInMethodVal { value, .. } = self {
                    vals.push(*value.clone());
                }
                fun(vals, engine)
            }
                
            TypeVal(id) => {
                let (Type::Def       { members, .. }| 
                    Type::BuiltInDef { members, .. }) = engine.get_type(*id);
                check_arity(members.len())?;
                let mut map = HashMap::new(); 
                for (mem, v) in std::iter::zip(members, vals) {
                    map.insert(mem.to_string(), v);
                }
                Ok(InstanceVal { type_id: *id, members: map })
            },
            _ => Err((format!("`{ty}` is not applicable.", ty = self.ty()), None))
        }
    }
}
