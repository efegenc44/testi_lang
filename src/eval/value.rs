use std::collections::HashMap;

use crate::{
    ast::stmt::{ Stmt, self }, 
    span::Spanned, error::error::{ Error, Res }
};

use super::{
    evaluator::{ Engine, State }, 
    r#type::*
};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
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

#[derive(Debug, Clone)]
pub struct Function {
    pub args: Vec<String>,
    pub body: Vec<Spanned<Stmt>>,
}

impl Into<Function> for &Spanned<stmt::Function> {
    fn into(self) -> Function {
        let data = self.clone().data;
        Function {
            args: data.args,
            body: data.body,
        }
    }
} 

#[derive(Clone)]
pub enum Value {
    Integer(i32),
    Float(f32),
    String(String),
    Char(char),
    Bool(bool),
    List(usize),
    Map(usize),
    Instance {
        type_id: usize, 
        id: usize,
    },
    Function {
        id: usize,
        value: Option<Box<Value>>, // If Method
        closure: Option<HashMap<String, Value>> // If Closure
    },
    BuiltInFunction {
        fun: BuiltInFunction,
        value: Option<Box<Value>>, // If Method
    },
    Nothing,
    Type(usize),
    Module(usize)
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(int) => write!(f, "{int}"),
            Value::Float(float) => write!(f, "{float}"),
            Value::String(s)    => write!(f, "{s}"),
            Value::Char(ch)     => write!(f, "{ch}"),
            Value::Bool(b)      => write!(f, "{b}"),
            Value::List(list)   => write!(f, "{list}"),
            Value::Map(map)     => write!(f, "{map}"),
            Value::Instance { type_id, id } => write!(f, "{type_id};{id}"),
            Value::Function {..}        => write!(f, "<function>"),
            Value::BuiltInFunction {..} => write!(f, "<built-in function>"),
            Value::Nothing            => write!(f, "nothing"),
            Value::Type(ty)           => write!(f, "{ty}"),
            Value::Module(_)          => write!(f, "<module>"),
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
            Value::Integer(_)         => BuiltInType::Integer as usize,
            Value::Float(_)           => BuiltInType::Float as usize ,
            Value::String(_)          => BuiltInType::String as usize,
            Value::Bool(_)            => BuiltInType::Bool as usize,
            Value::List(_)            => BuiltInType::List as usize,
            Value::Map(_)             => BuiltInType::Map as usize,
            Value::Function {..}        |
            Value::BuiltInFunction {..} => BuiltInType::Function as usize,
            Value::Nothing            => BuiltInType::Nothing as usize,
            Value::Type(_)            => BuiltInType::Type as usize,
            Value::Module(_)          => BuiltInType::Module as usize,
            
            Value::Instance { type_id, .. } => *type_id,
            
            Value::Char(_)            => BuiltInType::Character as usize,
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

    pub fn as_instance(&self) -> Result<(usize, usize), String> {
        match self {
            Value::Instance { type_id, id } => Ok((*type_id, *id)),
            _ => Err(format!("Expected Type, got {}.", self.ty()))
        }
    }
}

impl Value {
    // Contrary to what it says as "unsafe" this function is quite safe.
    // Because that we don't mutate the things we iterate over,
    // there is ackchyually (🤓) no danger to mutably reference the Engine
    // again in the iteration body (i guess that is what happens), but because 
    // Rust Borrow Checker doesn't allow us to do that, we use UNSAFE 😨.
    // I couldn't come up with a better solution, sorry guys 😞 
    pub unsafe /* 😨 */ fn mark(&self, engine: *mut Engine) {
        match self {
            Value::List(id) => {
                (*engine).lists.marked.insert(*id);
                for value in (*engine).lists.get(id) {
                    value.mark(engine)
                }
            },
            
            Value::Map(id) => {
                (*engine).maps.marked.insert(*id);
                for value in (*engine).maps.get(id).values() {
                    value.mark(engine)
                }
            },
            
            Value::Instance { type_id, id } => {
                (*engine).types.marked.insert(*type_id);
                
                let Type { methods, .. } = (*engine).types.get(id);
                for method in methods.values() {
                    (*engine).functions.marked.insert(*method);
                }
                
                (*engine).instances.marked.insert(*id);
                for value in (*engine).instances.get(id).values() {
                    value.mark(engine)
                }
            },
            
            Value::Function { id, value, closure } => {
                (*engine).functions.marked.insert(*id);
                if let Some(value) = value {
                    value.mark(engine);
                }
                if let Some(closure) = closure {
                    for value in closure.values() {
                        value.mark(engine);
                    }
                }
            },
            
            Value::BuiltInFunction { fun: _, value } => 
                if let Some(value) = value {
                    value.mark(engine);
                },
            
            Value::Type(id) => {
                (*engine).types.marked.insert(*id);
                
                let Type { methods, .. } = (*engine).types.get(id);
                for method in methods.values() {
                    (*engine).functions.marked.insert(*method);
                }
            },
            
            Value::Module(id) => {
                (*engine).modules.marked.insert(*id);
            }
            _ => ()
        }
    }
    
    pub fn get_method(&self, method: &str, engine: &mut Engine) -> Result<Value, String> {
        let Type { methods, builtin_methods, .. } = engine.types.get(&self.ty());

        if let Some(method) = methods.get(method) {
            return Ok(Value::Function { 
                id: *method, 
                value: Some(Box::new(self.clone())),
                closure: None
            })
        }

        if let Some(builtin_methods) = builtin_methods {
            if let Some(method) = builtin_methods.get(method) {
                return Ok(Value::BuiltInFunction { 
                    fun: method.clone(), 
                    value: Some(Box::new(self.clone())),
                })
            }
        }

        let Some(method) = engine.get_impl(method) else {
            return Err(format!("`{ty}` does not implement `{method}`.", ty = self.ty()))
        };

        Ok(Value::Function { 
            id: *method, 
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
            Value::Function { id, value: self_value, closure } => {
                let Function { args, body } = engine.functions.get(id).clone();
                
                check_arity(args.len())?;                
                if body.is_empty() {
                    return Ok(Value::Nothing)
                }

                engine.enter_scope();

                if let Some(value) = self_value {
                    engine.define(String::from("self"), *value.clone());
                } else {
                    engine.define(String::from("recur"), self.clone());
                }

                if let Some(closure) = closure {
                    for (arg, value) in closure {
                        engine.define(arg.to_string(), value.clone());
                    }
                }
                
                handle(engine.collect_definitions(&body))?;
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
            
            Value::BuiltInFunction { fun, value } => {
                check_arity(fun.arity)?;
                if let Some(value) = value {
                    vals.push(*value.clone());
                }
                (fun.fun)(vals, engine)
            }
                
            Value::Type(type_id) => {
                let Type { members, .. } = engine.types.get(type_id);
                check_arity(members.len())?;
                let mut map = HashMap::new(); 
                for (member, v) in std::iter::zip(members, vals) {
                    map.insert(member.to_string(), v);
                }
                Ok(Value::Instance { type_id: *type_id, id: engine.instances.make(map) })
            },
            _ => Err((format!("`{ty}` is not applicable.", ty = self.ty()), None))
        }
    }
}
