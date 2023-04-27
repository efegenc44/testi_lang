use crate::{
    eval::value::Value::*, 
    make_type, map, builtinfun
};

make_type! { typee , (), Some(map! {
    eq => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_type().unwrap();
        match &vals[0] {
            Type(id) => Ok(Bool(&selff == id)),
            other => return Err((format!("Can't check equality of {} against `Type`", other.ty()), None))
        }
    }},
})}