use std::collections::HashMap;

use super::{value::{ Value::{ self, * }, BuiltInFunction }, r#type::*};

pub fn get_global() -> HashMap<String, Value> {
    let global = HashMap::from([        
        ("Integer".to_string(), TypeVal(INTEGER_TYPE_ID)),
        ("Bool".to_string(), TypeVal(BOOL_TYPE_ID)),
        ("String".to_string(), TypeVal(STRING_TYPE_ID)),
        ("Float".to_string(), TypeVal(FLOAT_TYPE_ID)),
        ("Range".to_string(), TypeVal(RANGE_TYPE_ID)),
        ("Character".to_string(), TypeVal(CHARACTER_TYPE_ID)),
        ("Method".to_string(), TypeVal(METHOD_TYPE_ID)),
        ("Function".to_string() , TypeVal(FUNCTION_TYPE_ID)),
        ("List".to_string(), TypeVal(LIST_TYPE_ID)),
        ("Map".to_string(), TypeVal(MAP_TYPE_ID)),
        ("Nothing".to_string(), TypeVal(NOTHING_TYPE_ID)),
        ("Type".to_string(), TypeVal(TYPE_TYPE_ID)),
        ("print".to_string(), BuiltInFunVal {
            arity: 1, fun: |vals, _| {
                println!("{}", vals[0]);
                Ok(NothingVal)
            }
        }),
        ("type".to_string(), BuiltInFunVal { 
            arity: 1, fun: |vals, _| {
                Ok(TypeVal(vals[0].ty()))
            }
        }),
        ("contains_key".to_string(), BuiltInFunVal { 
            arity: 2, fun: |vals, _| {
                let fv = vals.get(0).unwrap();
                let MapVal(map) = fv else {
                    return Err((format!("Expected `Map` as first argument, not `{ty}`", ty = fv.ty()), None));
                };
                let key = match vals[1].clone().try_into() {
                    Ok(key)  => key,
                    Err(err) => return Err((err, None)),
                };
                Ok(BoolVal(map.contains_key(&key)))     
            }
        }),
        ("len".to_string(), BuiltInFunVal { 
            arity: 1, fun: |vals, _| {
                let res = match vals[0].len() {
                    Ok(len)  => len,
                    Err(err) => return Err((err, None)),
                };
                Ok(IntegerVal(res as i32))     
            }
        }),
        ("map".to_string(), BuiltInFunVal { 
            arity: 2, fun: |vals, engine| {
                let fv = vals.get(0).unwrap();
                let sv = vals.get(1).unwrap();
                let ListVal(list) = sv else {
                    return Err((format!("Expected `List` as second argument, not `{ty}`", ty = sv.ty()), None));
                };
                let mut new_list = vec![];
                for e in list {
                    new_list.push(fv.apply(vec![e.to_owned()], engine)?);
                }
                Ok(ListVal(new_list))
            } 
        }),
        ("input".to_string(), BuiltInFunVal { 
            arity: 0, fun: |_, _| {
                let mut line = String::new();
                std::io::stdin().read_line(&mut line).unwrap();
                let line = line.trim_end().to_string();
                Ok(StringVal(line))
            } 
        })
    ]);

    global
}

pub fn integer_type() -> Type {
    Type::BuiltInDef { 
        members: vec![], 
        builtin_methods: HashMap::from([
            (String::from("times"), BuiltInFunction { arity: 2, fun: |values, engine| {
                let v = values.last().unwrap().as_integer().unwrap();
                let ListVal(list) = &values[1] else {
                    return Err((format!("Expected `List` as second argument, not `{ty}`", ty = values[1].ty()), None));
                };
                for _ in 0..v {
                    values[0].apply(list.clone(), engine)?;
                }
                Ok(NothingVal)
            }}),
            (String::from("add"), BuiltInFunction { arity: 1, fun: |values, _| {
                let left  = values.last().unwrap().as_integer().unwrap();                    
                let right = values[0].as_integer().map_err(|err| (err, None))?;
                Ok(IntegerVal(left + right))
            }}),
            (String::from("neg"), BuiltInFunction { arity: 0, fun: |values, _| {
                let v = values.last().unwrap().as_integer().unwrap();                    
                Ok(IntegerVal(-v))
            }}),
            (String::from("eq"), BuiltInFunction { arity: 1, fun: |values, _| {
                let left  = values.last().unwrap().as_integer().unwrap();                    
                let right = values[0].as_integer().map_err(|err| (err, None))?;
                Ok(BoolVal(left == right))
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
                    Ok(int) => Ok(IntegerVal(int)),
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
                Ok(BoolVal(left && right))
            }}),
            (String::from("not"), BuiltInFunction { arity: 0, fun: |values, _| {
                let v = values.last().unwrap().as_bool().unwrap();                    
                Ok(BoolVal(!v))
            }}),
            (String::from("eq"), BuiltInFunction { arity: 1, fun: |values, _| {
                let left  = values.last().unwrap().as_bool().unwrap();                    
                let right = values[0].as_bool().map_err(|err| (err, None))?;
                Ok(BoolVal(left == right))
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