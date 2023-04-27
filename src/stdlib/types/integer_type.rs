use crate::{
    eval::value::Value::*, 
    make_type, map, builtinfun
};

make_type! { integer , (), Some(map! {
    add => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_integer().unwrap();
        match &vals[0] {
            Integer(other) => Ok(Integer(selff + other)),
            other => return Err((format!("Can't add {} to `Integer`", other.ty()), None))
        }
    }},
    
    sub => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_integer().unwrap();
        match &vals[0] {
            Integer(other) => Ok(Integer(selff - other)),
            other => return Err((format!("Can't subtract {} from `Integer`", other.ty()), None))
        }
    }},
    
    mul => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_integer().unwrap();
        match &vals[0] {
            Integer(other) => Ok(Integer(selff * other)),
            other => return Err((format!("Can't multiply {} with `Integer`", other.ty()), None))
        }
    }},

    mod => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_integer().unwrap();
        match &vals[0] {
            Integer(other) => Ok(Integer(selff % other)),
            other => return Err((format!("Can't mod {} with `Integer`", other.ty()), None))
        }
    }},

    div => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_integer().unwrap();
        match &vals[0] {
            Integer(other) => {
                if other == &0 {
                    return Err((format!("Division by zero"), None))
                }
                Ok(Integer(selff + other))
            },
            other => return Err((format!("Can't divide {} with `Integer`", other.ty()), None))
        }
    }},
    
    neg => builtinfun! { 0, |vals, _| {
        let selff = vals.last().unwrap().as_integer().unwrap();
        Ok(Integer(-selff))
    }},

    times => builtinfun! { 2, |vals, engine| {
        let v = vals.last().unwrap().as_integer().unwrap();
        let list = vals[1].as_list().map_err(|err| (err, None))?;
        let list = engine.lists.get(&list).clone();
        for _ in 0..v {
            vals[0].apply(list.clone(), engine)?;
        }
        Ok(Nothing)
    }},

    eq => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_integer().unwrap();
        match &vals[0] {
            Integer(other) => Ok(Bool(&selff == other)),
            other => return Err((format!("Can't check equality of {} against `Integer`", other.ty()), None))
        }
    }},
    
    gt => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_integer().unwrap();
        match &vals[0] {
            Integer(other) => Ok(Bool(&selff > other)),
            other => return Err((format!("Can't check greatness of {} against `Integer`", other.ty()), None))
        }
    }},
    
    to_string => builtinfun! { 0, |vals, _| {
        let selff = vals.last().unwrap().as_integer().unwrap();
        Ok(String(selff.to_string()))
    }},
})}