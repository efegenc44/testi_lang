use crate::{
    eval::value::Value::*, 
    make_type, map, builtinfun
};

make_type! { string , (), Some(map! {
    add => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_string().unwrap();
        match &vals[0] {
            String(other) => Ok(String(selff + other)),
            other => return Err((format!("Can't add {} to `String`", other.ty()), None))
        }
    }},
    
    mul => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_string().unwrap();
        match &vals[0] {
            Integer(other) => {
                let mut s = std::string::String::new();
                for _ in 0..*other {
                    s += &selff;
                }
                Ok(String(s))
            },
            other => return Err((format!("Can't multiply {} with `String`", other.ty()), None))
        }
    }},
    
    eq => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_string().unwrap();
        match &vals[0] {
            String(other) => Ok(Bool(&selff == other)),
            other => return Err((format!("Can't check equality of {} against `String`", other.ty()), None))
        }
    }},
    
    parse_integer => builtinfun! { 0, |vals, _| {
        match vals.last().unwrap().as_string().unwrap().parse::<i32>() {
            Ok(int) => Ok(Integer(int)),
            Err(_) => Err((format!("Can't parse into `Integer`"), None)),
        }
    }},

    to_string => builtinfun! { 0, |vals, _| {
        Ok(vals.last().unwrap().clone())
    }},
})}