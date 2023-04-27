#[macro_export]
macro_rules! map {
    {$($k: ident => $v: expr),* $(,)?} => {
        std::collections::HashMap::from([ 
            $( (std::string::String::from(stringify!($k)), $v) ),*
        ])
    };
}

#[macro_export]
macro_rules! vbuiltinfun {
    ($arity: literal, $fun: expr) => {
        Value::BuiltInFunction { 
            fun: value::BuiltInFunction {
                arity: $arity,
                fun: $fun
            }, 
            value: None 
        }        
    };
}

#[macro_export]
macro_rules! builtinfun {
    ($arity: literal, $fun: expr) => {
        crate::eval::value::BuiltInFunction {
            arity: $arity,
            fun: $fun
        } 
    };
}

#[macro_export]
macro_rules! make_type {
    ($name: ident, ($($member: ident)*), $methods: expr) => {
        pub fn $name() -> crate::eval::r#type::Type {
            crate::eval::r#type::Type {
                members: vec![ $(std::string::String::from(stringify!($member))),* ],
                builtin_methods: $methods,
                methods: std::collections::HashMap::new()
            }
        } 
    };
}