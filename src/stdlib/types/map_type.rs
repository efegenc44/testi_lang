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

    len => builtinfun! { 0, |vals, engine| {
        let selff = vals.last().unwrap().as_map().unwrap();
        let map = engine.maps.get(&selff);
        Ok(Integer(map.len() as i32))
    }},

    keys => builtinfun! { 0, |vals, engine| {
        let selff = vals.last().unwrap().as_map().unwrap();
        let map = engine.maps.get(&selff);
        Ok(List(engine.lists.make(map.keys().map(|key| key.clone().into()).collect())))
    }},
})}