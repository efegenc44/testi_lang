
use std::collections::HashMap;

use crate::{ 
    map, vbuiltinfun,
    eval::{
        r#type::BuiltInType,
        value::{ self, Value }
    }
};

use Value::*;

pub fn get_global() -> HashMap<std::string::String, Value> {
    map! {
        // Global BuiltIn Type Variables
        Integer  => Type(BuiltInType::Integer  as usize),
        Float    => Type(BuiltInType::Float    as usize),
        String   => Type(BuiltInType::String   as usize),
        Bool     => Type(BuiltInType::Bool     as usize),
        List     => Type(BuiltInType::List     as usize),
        Map      => Type(BuiltInType::Map      as usize),
        Range    => Type(BuiltInType::Range    as usize),
        Function => Type(BuiltInType::Function as usize),
        Nothing  => Type(BuiltInType::Nothing  as usize),
        Type     => Type(BuiltInType::Type     as usize),
        Module   => Type(BuiltInType::Module   as usize),
 
        print => vbuiltinfun! { 1, |vals, engine| {
            let string = vals[0].get_method("to_string", engine)
                            .map_err(|err| (err, None))
                            .and_then(|method| method.apply(vec![], engine))?;
            println!("{string}");
            Ok(Nothing)
        }},

        type => vbuiltinfun! { 1, |vals, _| {
            println!("{}", vals[0].ty());
            Ok(Nothing)
        }},
        
        input => vbuiltinfun! { 0, |_, _| {
            let mut line = std::string::String::new();
            std::io::stdin().read_line(&mut line).unwrap();
            let line = line.trim_end().to_string();
            Ok(String(line))
        }},

        mark_n_sweep => vbuiltinfun! { 0, |_, engine| {
            engine.mark_and_sweep();
            Ok(Nothing)
        }}

    }
}
