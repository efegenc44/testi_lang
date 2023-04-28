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
})}