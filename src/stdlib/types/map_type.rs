use crate::{
    eval::value::Value::*, 
    make_type, map, builtinfun
};

make_type! { map , (), Some(map! {
    remove => builtinfun! { 1, |vals, engine| {
        let selff = vals.last().unwrap().as_map().unwrap();
        let kvalue = vals[0].clone().try_into().map_err(|err| (err, None))?;
        let map = engine.maps.get_mut(&selff);
        if map.len() < 1 {
            return Err((format!("Map is empty cannot pop!"), None));
        }
        map.remove(&kvalue);
        Ok(Nothing)
    }},

    contains_key => builtinfun! { 1, |vals, engine| {
        let selff = vals.last().unwrap().as_map().unwrap();
        let kvalue = vals[0].clone().try_into().map_err(|err| (err, None))?;
        let map = engine.maps.get(&selff);
        Ok(Bool(map.contains_key(&kvalue)))
    }},
    
    index => builtinfun! { 1, |vals, engine| {
        let selff = vals.last().unwrap().as_map().unwrap();
        let map = engine.maps.get(&selff);
        let kvalue = vals[0].clone().try_into().map_err(|err| (err, None))?;
        if !map.contains_key(&kvalue) {
            return Err((format!("Absent key."), None))
        }
        Ok(map[&kvalue].clone())
    }},

    eq => builtinfun! { 1, |vals, engine| {
        let selff = vals.last().unwrap().as_map().unwrap();
        match &vals[0] {
            List(other) => {
                // TODO: Discard clone
                let selff = engine.maps.get(&selff).clone(); 
                let other = engine.maps.get(&other).clone(); 
                for ((key1, value1), (key2, value2)) in std::iter::zip(selff, other) {
                    let b1 = key1 == key2;

                    let Bool(b2) = value1.get_method("eq", engine)
                        .map_err(|err| (err, None))
                        .and_then(|method| method.apply(vec![value2.clone()], engine))? else {
                        return Err((format!("Equality should return `Bool`"), None))
                    };
                    if !(b1 && b2) {
                        return Ok(Bool(false))
                    }
                }
                Ok(Bool(true))
            },
            other => return Err((format!("Can't check equality of {} against `Integer`", other.ty()), None))
        }
    }},

    to_string => builtinfun! { 0, |vals, engine| {
        let selff = vals.last().unwrap().as_map().unwrap();
        // TODO: discard cloning 
        let map = engine.maps.get(&selff).clone();
        let mut string = std::string::String::from("#[ ");
        for (key, value) in map {
            let v = value.get_method("to_string", engine)
                .map_err(|err| (err, None))
                .and_then(|method| method.apply(vec![], engine))?;
            string += format!("{key}: {v} ").as_str();
        }
        string += "]\n";
        Ok(String(string))
    }},
})}