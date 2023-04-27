use crate::{
    eval::value::Value::*, 
    make_type, map, builtinfun
};

make_type! { list , (), Some(map! {
    push => builtinfun! { 1, |vals, engine| {
        let selff = vals.last().unwrap().as_list().unwrap();
        let list = engine.lists.get_mut(&selff);
        list.push(vals[0].clone());
        Ok(Nothing)
    }},
    
    pop => builtinfun! { 0, |vals, engine| {
        let selff = vals.last().unwrap().as_list().unwrap();
        let list = engine.lists.get_mut(&selff);
        if list.len() < 1 {
            return Err((format!("List is empty cannot pop!"), None));
        }
        Ok(list.pop().unwrap())
    }},
    
    index => builtinfun! { 1, |vals, engine| {
        let selff = vals.last().unwrap().as_list().unwrap();
        let list = engine.lists.get(&selff);
        let index = vals[0].as_integer().map_err(|err| (err, None))? as usize;
        if index >= list.len() {
            return Err((format!("Index out of bounds"),None))
        }
        Ok(list[index].clone())
    }},

    len => builtinfun! { 0, |vals, engine| {
        let selff = vals.last().unwrap().as_list().unwrap();
        let list = engine.lists.get(&selff);
        Ok(Integer(list.len() as i32))
    }},

    eq => builtinfun! { 1, |vals, engine| {
        let selff = vals.last().unwrap().as_list().unwrap();
        match &vals[0] {
            List(other) => {
                // TODO: Discard clone
                let selff = engine.lists.get(&selff).clone(); 
                let other = engine.lists.get(&other).clone(); 
                for (value1, value2) in std::iter::zip(selff, other) {
                    let Bool(b) = value1.get_method("eq", engine)
                        .map_err(|err| (err, None))
                        .and_then(|method| method.apply(vec![value2.clone()], engine))? else {
                        return Err((format!("Equality should return `Bool`"), None))
                    };
                    if !b {
                        return Ok(Bool(false))
                    }
                }
                Ok(Bool(true))
            },
            other => return Err((format!("Can't check equality of {} against `Integer`", other.ty()), None))
        }
    }},

    to_string => builtinfun! { 0, |vals, engine| {
        let selff = vals.last().unwrap().as_list().unwrap();
        // TODO: discard cloning 
        let list = engine.lists.get(&selff).clone();
        let mut string = std::string::String::from("[ ");
        for value in list {
            let s = value.get_method("to_string", engine)
                        .map_err(|err| (err, None))
                        .and_then(|method| method.apply(vec![], engine))?;
            string += format!("{s} ").as_str();
        }
        string += "]\n";
        Ok(String(string))
    }},
})}