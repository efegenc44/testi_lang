use crate::{
    eval::value::Value::*, 
    make_type, map, builtinfun
};

make_type! { float , (), Some(map! {
    add => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_float().unwrap();
        match &vals[0] {
            Float(other) => Ok(Float(selff + other)),
            other => return Err((format!("Can't add {} to `Float`", other.ty()), None))
        }
    }},
    
    sub => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_float().unwrap();
        match &vals[0] {
            Float(other) => Ok(Float(selff - other)),
            other => return Err((format!("Can't subtract {} from `Float`", other.ty()), None))
        }
    }},
    
    mul => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_float().unwrap();
        match &vals[0] {
            Float(other) => Ok(Float(selff * other)),
            other => return Err((format!("Can't multiply {} with `Float`", other.ty()), None))
        }
    }},
    
    div => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_float().unwrap();
        match &vals[0] {
            Float(other) => {
                if other == &0. {
                    return Err((format!("Division by zero"), None))
                }
                Ok(Float(selff + other))
            },
            other => return Err((format!("Can't divide {} with `Float`", other.ty()), None))
        }
    }},
    
    eq => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_float().unwrap();
        match &vals[0] {
            Float(other) => Ok(Bool(&selff == other)),
            other => return Err((format!("Can't check equality of {} against `Float`", other.ty()), None))
        }
    }},
    
    gt => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_float().unwrap();
        match &vals[0] {
            Float(other) => Ok(Bool(&selff > other)),
            other => return Err((format!("Can't check greatness of {} against `Float`", other.ty()), None))
        }
    }},
    
    to_string => builtinfun! { 0, |vals, _| {
        let selff = vals.last().unwrap().as_float().unwrap();
        Ok(String(selff.to_string()))
    }},
})}