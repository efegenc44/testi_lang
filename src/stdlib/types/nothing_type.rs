use crate::{
    eval::value::Value::*, 
    make_type, map, builtinfun
};

make_type! { nothing , (), Some(map! {
    eq => builtinfun! { 1, |vals, _| {
        match &vals[0] {
            Nothing => Ok(Bool(true)),
            other => return Err((format!("Can't check equality of {} against `Nothing`", other.ty()), None))
        }
    }},

    to_string => builtinfun! { 0, |_, _| {
        Ok(String(std::string::String::from("Nothing")))
    }},
})}