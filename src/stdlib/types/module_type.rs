use crate::{
    eval::value::Value::*, 
    make_type, map, builtinfun
};

make_type! { module , (), Some(map! {
    eq => builtinfun! { 1, |vals, _| {
        let selff = vals.last().unwrap().as_module().unwrap();
        match &vals[0] {
            Module(id) => Ok(Bool(&selff == id)),
            other => return Err((format!("Can't check equality of {} against `Module`", other.ty()), None))
        }
    }},
})}