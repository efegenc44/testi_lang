use std::collections::HashMap;

use super::{
    value::Value::{ self, * },
    r#type::Type::*
};

pub fn get_global() -> HashMap<String, Value> {
    let global = HashMap::from([
        ("Integer".to_string()  , TypeVal(IntegerTy)),
        ("Float".to_string()    , TypeVal(FloatTy)),
        ("String".to_string()   , TypeVal(StringTy)),
        ("Character".to_string(), TypeVal(CharTy)),
        ("Bool".to_string()     , TypeVal(BoolTy)),
        ("Function".to_string() , TypeVal(FunTy)),
        ("List".to_string()     , TypeVal(ListTy)),
        ("Map".to_string()      , TypeVal(MapTy)),
        ("Nothing".to_string()  , TypeVal(NothingTy)),
        ("Type".to_string()     , TypeVal(TyTy)),
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
                let FunTy = fv.ty() else {
                    return Err((format!("Expected `Function` as first argument, not `{ty}`", ty = fv.ty()), None));
                };
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