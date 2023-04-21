use std::collections::HashMap;

use super::{value::{ Value, BuiltInFunction }, r#type::*};

pub fn get_global() -> HashMap<String, Value> {
    let global = HashMap::from([        
        ("Integer".to_string(), Value::Type(INTEGER_TYPE_ID)),
        ("Bool".to_string(), Value::Type(BOOL_TYPE_ID)),
        ("String".to_string(), Value::Type(STRING_TYPE_ID)),
        //("Float".to_string(), Value::Type(FLOAT_TYPE_ID)),
        ("Range".to_string(), Value::Type(RANGE_TYPE_ID)),
        //("Character".to_string(), Value::Type(CHARACTER_TYPE_ID)),
        ("Function".to_string() , Value::Type(FUNCTION_TYPE_ID)),
        ("List".to_string(), Value::Type(LIST_TYPE_ID)),
        //("Map".to_string(), Value::Type(MAP_TYPE_ID)),
        ("Nothing".to_string(), Value::Type(NOTHING_TYPE_ID)),
        //("Type".to_string(), Value::Type(TYPE_TYPE_ID)),
        
        ("print".to_string(), Value::BuiltInFunction {
            fun: BuiltInFunction { arity: 1, fun: |vals, _| {
                println!("{}", vals[0]);
                Ok(Value::Nothing)
            }},
            value: None
        }),
        ("type".to_string(), Value::BuiltInFunction { 
            fun: BuiltInFunction { arity: 1, fun: |vals, _| {
                Ok(Value::Type(vals[0].ty()))
            }},
            value: None
        }),
        ("map".to_string(), Value::BuiltInFunction { 
            fun: BuiltInFunction { arity: 2, fun: |vals, engine| {
                let fv = vals.get(0).unwrap();
                let sv = vals.get(1).unwrap();
                let Value::List(list) = sv else {
                    return Err((format!("Expected `List` as second argument, not `{ty}`", ty = sv.ty()), None));
                };
                let list = engine.lists.get(list).clone();
                let mut new_list = vec![];
                for e in list {
                    new_list.push(fv.apply(vec![e.to_owned()], engine)?);
                }
                Ok(Value::List(engine.lists.make(new_list)))
            }},
            value: None
        }),
        ("input".to_string(), Value::BuiltInFunction { 
            fun: BuiltInFunction { arity: 0, fun: |_, _| {
                let mut line = String::new();
                std::io::stdin().read_line(&mut line).unwrap();
                let line = line.trim_end().to_string();
                Ok(Value::String(line))
            }},
            value: None
        }),
        ("markNsweep".to_string(), Value::BuiltInFunction { 
            fun: BuiltInFunction { arity: 0, fun: |_, engine| {
                engine.mark_and_sweep();
                Ok(Value::Nothing)
            }},
            value: None
        }),
        ("showLists".to_string(), Value::BuiltInFunction { 
            fun: BuiltInFunction { arity: 0, fun: |_, engine| {
                dbg!(&engine.lists.values);
                Ok(Value::Nothing)
            }},
            value: None
        }),
        ("showTypes".to_string(), Value::BuiltInFunction { 
            fun: BuiltInFunction { arity: 0, fun: |_, engine| {
                dbg!(&engine.types.values);
                Ok(Value::Nothing)
            }},
            value: None
        }),
        ("showModules".to_string(), Value::BuiltInFunction { 
            fun: BuiltInFunction { arity: 0, fun: |_, engine| {
                dbg!(&engine.modules.values);
                Ok(Value::Nothing)
            }},
            value: None
        }),
    ]);

    global
}

pub fn integer_type() -> Type {
    Type::BuiltInDef { 
        members: vec![], 
        builtin_methods: HashMap::from([
            (String::from("times"), BuiltInFunction { arity: 2, fun: |values, engine| {
                let v = values.last().unwrap().as_integer().unwrap();
                let Value::List(list) = &values[1] else {
                    return Err((format!("Expected `List` as second argument, not `{ty}`", ty = values[1].ty()), None));
                };
                let list = engine.lists.get(list).clone();
                for _ in 0..v {
                    values[0].apply(list.clone(), engine)?;
                }
                Ok(Value::Nothing)
            }}),
            (String::from("add"), BuiltInFunction { arity: 1, fun: |values, _| {
                let left  = values.last().unwrap().as_integer().unwrap();                    
                let right = values[0].as_integer().map_err(|err| (err, None))?;
                Ok(Value::Integer(left + right))
            }}),
            (String::from("le"), BuiltInFunction { arity: 1, fun: |values, _| {
                let left  = values.last().unwrap().as_integer().unwrap();                    
                let right = values[0].as_integer().map_err(|err| (err, None))?;
                Ok(Value::Bool(left <= right))
            }}),
            (String::from("sub"), BuiltInFunction { arity: 1, fun: |values, _| {
                let left  = values.last().unwrap().as_integer().unwrap();                    
                let right = values[0].as_integer().map_err(|err| (err, None))?;
                Ok(Value::Integer(left - right))
            }}),
            (String::from("neg"), BuiltInFunction { arity: 0, fun: |values, _| {
                let v = values.last().unwrap().as_integer().unwrap();                    
                Ok(Value::Integer(-v))
            }}),
            (String::from("eq"), BuiltInFunction { arity: 1, fun: |values, _| {
                let left  = values.last().unwrap().as_integer().unwrap();                    
                let right = values[0].as_integer().map_err(|err| (err, None))?;
                Ok(Value::Bool(left == right))
            }})
        ]),
        methods: HashMap::new(), 
    }
}

pub fn string_type() -> Type {
    Type::BuiltInDef { 
        members: vec![], 
        builtin_methods: HashMap::from([
            (String::from("parse_integer"), BuiltInFunction { arity: 0, fun: |values, _| {
                let v = values.last().unwrap().as_string().unwrap();
                match v.parse() {
                    Ok(int) => Ok(Value::Integer(int)),
                    Err(_)  => Err((format!("Couldn't convert to `Integer`"), None))
                }
            }})
        ]),
        methods: HashMap::new(), 
    }
}

pub fn bool_type() -> Type {
    Type::BuiltInDef { 
        members: vec![], 
        builtin_methods: HashMap::from([
            (String::from("and"), BuiltInFunction { arity: 1, fun: |values, _| {
                let left  = values.last().unwrap().as_bool().unwrap();                    
                let right = values[0].as_bool().map_err(|err| (err, None))?;
                Ok(Value::Bool(left && right))
            }}),
            (String::from("not"), BuiltInFunction { arity: 0, fun: |values, _| {
                let v = values.last().unwrap().as_bool().unwrap();                    
                Ok(Value::Bool(!v))
            }}),
            (String::from("eq"), BuiltInFunction { arity: 1, fun: |values, _| {
                let left  = values.last().unwrap().as_bool().unwrap();                    
                let right = values[0].as_bool().map_err(|err| (err, None))?;
                Ok(Value::Bool(left == right))
            }})
        ]),
        methods: HashMap::new(),
    }
}

pub fn function_type() -> Type {
    Type::BuiltInDef { 
        members: vec![], 
        builtin_methods: HashMap::new(), 
        methods: HashMap::new(), 
    }
}

pub fn range_type() -> Type {
    Type::BuiltInDef { 
        members: vec![String::from("bot"), String::from("up")], 
        builtin_methods: HashMap::new(), 
        methods: HashMap::new(), 
    }
}

pub fn nothing_type() -> Type {
    Type::BuiltInDef { 
        members: vec![String::from("bot"), String::from("up")], 
        builtin_methods: HashMap::new(), 
        methods: HashMap::new(), 
    }
}

pub fn list_type() -> Type {
    Type::BuiltInDef { 
        members: vec![], 
        builtin_methods: HashMap::from([
            (String::from("index"), BuiltInFunction { arity: 1, fun: |values, engine| {
                let Value::List(list) = values.last().unwrap() else {unreachable!()};
                let list = engine.lists.get(list);
                let index_value = values[0].as_integer().map_err(|err| (err, None))? as usize;
                if index_value >= list.len() {
                    return Err((format!("Index out of bounds."), None))
                }
                Ok(list[index_value].clone())
            }}),
            (String::from("len"), BuiltInFunction { arity: 0, fun: |values, engine| {
                let Value::List(list) = values.last().unwrap() else {unreachable!()};
                let list = engine.lists.get(list);
                Ok(Value::Integer(list.len() as i32))
            }}),
        ]), 
        methods: HashMap::new(), 
    }
}