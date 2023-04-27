use crate::{
    eval::value::Value::*, 
    make_type, map, builtinfun
};

make_type! { bool , (), Some(map! {
    and => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_bool().unwrap();
        match &vals[0] {
            Bool(other) => Ok(Bool(selff && *other)),
            other => return Err((format!("Can't and {} to `Bool`", other.ty()), None))
        }
    }},
    
    or => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_bool().unwrap();
        match &vals[0] {
            Bool(other) => Ok(Bool(selff || *other)),
            other => return Err((format!("Can't or {} from `Bool`", other.ty()), None))
        }
    }},

    not => builtinfun! { 0, |vals, _| {
        let selff = vals.last().unwrap().as_bool().unwrap();
        Ok(Bool(!selff))
    }},

    eq => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_bool().unwrap();
        match &vals[0] {
            Bool(other) => Ok(Bool(&selff == other)),
            other => return Err((format!("Can't check equality of {} against `Bool`", other.ty()), None))
        }
    }},
    
    gt => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_bool().unwrap();
        match &vals[0] {
            Bool(other) => Ok(Bool(&selff > other)),
            other => return Err((format!("Can't check greatness of {} against `Bool`", other.ty()), None))
        }
    }},
    
    to_string => builtinfun! { 0, |vals, _| {
        let selff = vals.last().unwrap().as_bool().unwrap();
        Ok(String(selff.to_string()))
    }},
})}