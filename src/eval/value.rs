use std::collections::HashMap;

use Value::*;

use crate::{
    ast::stmt::{Stmt, Function}, 
    span::Spanned, error::error::{ Error, Res }
};

use super::{
    r#type::Type::{ self, * }, 
    evaluator::{ Engine, State }
};

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum KeyValue {
    IntegerKey(i32),
    StringKey(String),
    BoolKey(bool),
    NothingKey,
}

impl std::fmt::Display for KeyValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeyValue::IntegerKey(int) => write!(f, "{int}"),
            KeyValue::StringKey(s)    => write!(f, "\"{s}\""),
            KeyValue::BoolKey(b)      => write!(f, "{b}"),
            KeyValue::NothingKey      => write!(f, "Nothing"),
        }
    }
}

impl TryInto<KeyValue> for Value {
    type Error = String;

    fn try_into(self) -> Result<KeyValue, Self::Error> {
        match self {
            IntegerVal(int) => Ok(KeyValue::IntegerKey(int)),
            StringVal(s)    => Ok(KeyValue::StringKey(s)),
            BoolVal(b)      => Ok(KeyValue::BoolKey(b)),
            NothingVal      => Ok(KeyValue::NothingKey),
            _ => Err(format!("`{ty}` is not hashable.", ty = self.ty()))
        }
    }
}


impl Into<Value> for KeyValue {
    fn into(self) -> Value {
        match self {
            KeyValue::IntegerKey(int) => IntegerVal(int),
            KeyValue::StringKey(s)    => StringVal(s),
            KeyValue::BoolKey(b)      => BoolVal(b),
            KeyValue::NothingKey      => NothingVal,
        }
    }
}

#[derive(Clone, Copy)]
pub struct BuiltInFunction {
    pub arity: usize,
    pub fun: fn(vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)>
}

#[derive(Clone)]
pub enum Value {
    IntegerVal(i32),
    FloatVal(f32),
    StringVal(String),
    CharVal(char),
    BoolVal(bool),
    ListVal(Vec<Value>),
    MapVal(HashMap<KeyValue, Value>),
    RangeVal(i32, i32),
    InstanceVal {
        type_name: String, 
        members: HashMap<String, Value>,
    },
    MethodVal {
        value: Box<Value>,
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>,
    },
    FunVal {
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>,
    },
    ClosureVal {
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>,
        closure: HashMap<String, Value>
    },
    BuiltInMethodVal {
        value: Box<Value>,
        arity: usize,
        fun: fn(vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)>
    },
    BuiltInFunVal {
        arity: usize,
        fun: fn(vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)>
    },
    NothingVal,
    TypeVal(Type),
    DefVal {
        name: String,
        members: Vec<String>,
        methods: HashMap<String, Function>,
    },
    BuiltInDefVal {
        name: String,
        members: Vec<String>,
        methods: HashMap<String, BuiltInFunction>,
    },
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerVal(int)    => write!(f, "{int}"),
            FloatVal(float)    => write!(f, "{float}"),
            StringVal(s)       => write!(f, "\"{s}\""),
            CharVal(ch)        => write!(f, "{ch}"),
            BoolVal(b)         => write!(f, "{b}"),
            ListVal(list)      => {
                if list.len() == 0 {
                    return write!(f, "[]")
                }
                write!(f, "[")?;
                write!(f, "{f}", f = list.first().unwrap())?;
                for value in &list[1..] {
                    write!(f, ", {value}")?;
                }
                write!(f, "]")?;
                Ok(())
            },
            MapVal(map) => {
                if map.len() == 0 {
                    return write!(f, "#[]")
                }
                let mut first = true;
                write!(f, "#[")?;
                for (key, value) in map {
                    if first {
                        write!(f, "{key}: {value}")?;
                        first = false;
                    } else {
                        write!(f, ", {key}: {value}")?;
                    }
                }
                write!(f, "]")?;
                Ok(())
            },
            InstanceVal { type_name, members } => {
                write!(f, "{type_name} ")?;
                let mut first = true;
                for (key, value) in members {
                    if first {
                        write!(f, "{key}: {value}")?;
                        first = false;
                    } else {
                        write!(f, ", {key}: {value}")?;
                    }
                }
                Ok(())
            },
            RangeVal(bot, up)          => write!(f, "{bot}..{up}"),
            MethodVal {..}             => write!(f, "<method>"),
            FunVal {..}                => write!(f, "<function>"),
            ClosureVal {..}            => write!(f, "<function>"),
            BuiltInFunVal {..}         => write!(f, "<built-in function>"),
            BuiltInMethodVal {..}      => write!(f, "<built-in method>"),
            NothingVal                 => write!(f, "nothing"),
            TypeVal(ty)                => write!(f, "{ty}"),
            DefVal { name, .. }        => write!(f, "{name}"),
            BuiltInDefVal { name, .. } => write!(f, "{name}"),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            IntegerVal(_)         => IntegerTy,
            FloatVal(_)           => FloatTy,
            StringVal(_)          => StringTy,
            CharVal(_)            => CharTy,
            BoolVal(_)            => BoolTy,
            ListVal(_)            => ListTy,
            MapVal(_)             => MapTy,
            RangeVal(_, _)        => RangeTy,
            MethodVal {..}        => MethodTy,
            BuiltInMethodVal {..} => MethodTy,
            FunVal {..}           => FunTy,
            ClosureVal {..}       => FunTy,
            BuiltInFunVal {..}    => FunTy,
            NothingVal            => NothingTy,
            TypeVal(_)            => TyTy,
            DefVal {..}           => TyTy,
            BuiltInDefVal {..}    => TyTy,
            
            InstanceVal { type_name, .. } => CustomTy(type_name.to_owned()),
        }
    }

    pub fn as_integer(&self) -> Result<i32, String> {
        match self {
            IntegerVal(int) => Ok(*int),
            _ => Err(format!("Expected {}, got {}.", IntegerTy, self.ty()))
        }
    }

    pub fn as_string(&self) -> Result<String, String> {
        match self {
            StringVal(s) => Ok(s.clone()),
            _ => Err(format!("Expected {}, got {}.", IntegerTy, self.ty()))
        }
    }

    pub fn as_char(&self) -> Result<char, String> {
        match self {
            CharVal(ch) => Ok(*ch),
            _ => Err(format!("Expected {}, got {}.", CharTy, self.ty()))
        }
    }

    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            BoolVal(b) => Ok(*b),
            _ => Err(format!("Expected {}, got {}.", BoolTy, self.ty()))
        }
    }

    pub fn as_type(&self) -> Result<Type, String> {
        match self {
            TypeVal(ty)                => Ok(ty.clone()),
            DefVal { name, .. }        => Ok(CustomTy(name.clone())),
            BuiltInDefVal { name, .. } => Ok(CustomTy(name.clone())),
            _ => Err(format!("Expected {}, got {}.", TyTy, self.ty()))
        }
    }

    pub fn as_instance(&self) -> Result<(String, HashMap<String, Value>), String> {
        match self {
            InstanceVal { type_name, members } => Ok((type_name.clone(), members.clone())),
            _ => Err(format!("Expected a `Definition`, got {}.", self.ty()))
        }
    }
}

pub type ExprResult = Result<Value, String>;

impl std::ops::Add for &Value {
    type Output = ExprResult;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (IntegerVal(int1), IntegerVal(int2)) => Ok(IntegerVal(int1 + int2)),
            (FloatVal(float1), FloatVal(float2)) => Ok(FloatVal(float1 + float2)),
            (StringVal(s1)   , StringVal(s2)   ) => Ok(StringVal(format!("{s1}{s2}"))),
            (CharVal(ch1)    , CharVal(ch2)    ) => Ok(StringVal(format!("{ch1}{ch2}"))),
            (CharVal(ch)     , StringVal(s)    ) => Ok(StringVal(format!("{ch}{s}"))),
            (StringVal(s)    , CharVal(ch)     ) => Ok(StringVal(format!("{s}{ch}"))),
            (ListVal(list), _) => {
                let mut list = list.to_owned();
                list.push(rhs.to_owned());
                Ok(ListVal(list))
            },
            (MapVal(map), ListVal(list)) => {
                if list.len() != 2 {
                    return Err(format!("Can't make a pair out of {len}-length `List`, it needs to be 2-lenght.", len = list.len()))
                }
                let mut map = map.to_owned();
                let key = TryInto::try_into(list[0].to_owned()).unwrap();
                map.insert(key, list[1].to_owned());
                Ok(MapVal(map))
            },
            _ => Err(format!("Can't add `{right}` to `{left}`", right = rhs.ty(), left = self.ty()))

        }
    }
}

impl std::ops::Mul for &Value {
    type Output = ExprResult;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (IntegerVal(int1), IntegerVal(int2)) => Ok(IntegerVal(int1 * int2)),
            (FloatVal(float1), FloatVal(float2)) => Ok(FloatVal(float1 * float2)),
            _ => Err(format!("Can't multiply `{left}` with `{right}`", right = rhs.ty(), left = self.ty()))
        }
    }
}

impl std::ops::Sub for &Value {
    type Output = ExprResult;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (IntegerVal(int1), IntegerVal(int2)) => Ok(IntegerVal(int1 - int2)),
            (FloatVal(float1), FloatVal(float2)) => Ok(FloatVal(float1 - float2)),
            _ => Err(format!("Can't subtract `{right}` from `{left}`", right = rhs.ty(), left = self.ty()))
        }
    }
}

impl std::ops::Div for &Value {
    type Output = ExprResult;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (IntegerVal(int1), IntegerVal(int2)) => {
                if int2 == &0 {
                    return Err("Attempt to divide by zero.".to_string())
                }
                Ok(FloatVal(*int1 as f32 / *int2 as f32))
            },
            (FloatVal(float1), FloatVal(float2)) => {
                if float2 == &0. {
                    return Err("Attempt to divide by zero.".to_string())
                }
                Ok(FloatVal(float1 / float2))
            },
            _ => Err(format!("Can't divide `{left}` with `{right}`", right = rhs.ty(), left = self.ty()))
        }
    }
}

impl std::ops::Rem for &Value {
    type Output = ExprResult;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (IntegerVal(int1), IntegerVal(int2)) => Ok(IntegerVal(int1 % int2)),
            _ => Err(format!("Can't mod `{left}` with `{right}`", right = rhs.ty(), left = self.ty()))
        }
    }
}


impl std::ops::BitOr for &Value {
    type Output = ExprResult;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (BoolVal(b1), BoolVal(b2)) => Ok(BoolVal(b1 | b2)),
            _ => Err(format!("Can't Or `{right}` with `{left}`", right = rhs.ty(), left = self.ty()))
        }
    }
}

impl std::ops::BitAnd for &Value {
    type Output = ExprResult;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (BoolVal(b1), BoolVal(b2)) => Ok(BoolVal(b1 & b2)),
            _ => Err(format!("Can't And `{right}` with `{left}`", right = rhs.ty(), left = self.ty()))
        }
    }
}

impl std::ops::Neg for &Value {
    type Output = ExprResult;

    fn neg(self) -> Self::Output {
        match self {
            IntegerVal(int) => Ok(IntegerVal(-int)),
            FloatVal(float) => Ok(FloatVal(-float)),
            _ => Err(format!("Can't negate `{operand}`.", operand = self.ty()))
        }
    }
}

impl std::ops::Not for &Value {
    type Output = ExprResult;

    fn not(self) -> Self::Output {
        match self {
            BoolVal(b) => Ok(BoolVal(!b)),
            _ => Err(format!("Can't not `{operand}`.", operand = self.ty())),
        }
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (IntegerVal(l0), IntegerVal(r0)) => l0 == r0,
            (FloatVal(l0)  , FloatVal(r0))   => l0 == r0,
            (StringVal(l0) , StringVal(r0))  => l0 == r0,
            (BoolVal(l0)   , BoolVal(r0))    => l0 == r0,
            (ListVal(l0)   , ListVal(r0))    => l0 == r0,
            (MapVal(l0)    , MapVal(r0))     => l0 == r0,
            (TypeVal(l0)   , TypeVal(r0))    => l0 == r0,
            (InstanceVal { type_name: ty1, members: mems1 }, 
             InstanceVal { type_name: ty2, members: mems2 }) => ty1 == ty2 && mems1 == mems2,
            (NothingVal, NothingVal) => true,
            (FunVal {..}, FunVal {..}) => false,
            (BuiltInFunVal {..}, BuiltInFunVal {..}) => false,
            _ => false,
        }
    }
}

impl Value {
    pub fn greater(&self, rhs: &Self) -> Result<bool, String> {
        match (self, rhs) {
            (IntegerVal(int1), IntegerVal(int2)) => Ok(int1   >   int2),
            (FloatVal(float1), FloatVal(float2)) => Ok(float1 > float2),
            _ => Err(format!("Can't check greaterness between `{left}` and `{right}`.", right = rhs.ty(), left = self.ty()))
        }
    }

    pub fn greater_or_equal(&self, rhs: &Self) -> Result<bool, String> {
        Ok(self.greater(rhs)? || self == rhs)
    }

    pub fn less(&self, rhs: &Self) -> Result<bool, String> {
        match (self, rhs) {
            (IntegerVal(int1), IntegerVal(int2)) => Ok(int1   <   int2),
            (FloatVal(float1), FloatVal(float2)) => Ok(float1 < float2),
            _ => Err(format!("Can't check lessness between `{left}` and `{right}`.", right = rhs.ty(), left = self.ty()))
        }
    }

    pub fn less_or_equal(&self, rhs: &Self) -> Result<bool, String> {
        Ok(self.less(rhs)? || self == rhs)
    }

    pub fn len(&self) -> Result<usize, String> {
        match self {
            StringVal(s)  => Ok(s.chars().count()),
            ListVal(list) => Ok(list.len()),
            _ => Err(format!("`{}` has no len attribute.", self.ty()))
        }
    }

    pub fn indexable(self) -> ExprResult {
        match self {
            StringVal(_) |
            ListVal(_)   |
            MapVal(_)    => Ok(self),
            _ => Err(format!("`{ty}` is not indexable.", ty = self.ty()))
        }
    }

    pub fn iter(self) -> Result<Box<dyn Iterator<Item = Value>>, String> {
        match self {
            StringVal(s)  => {
                let iter = s.chars().map(|ch| {
                    CharVal(ch)
                }).collect::<Vec<_>>().into_iter();
                Ok(Box::new(iter))
            },
            ListVal(list) => Ok(Box::new(list.into_iter())),
            MapVal(map)   => {
                let iter = map.into_iter().map(|(key, value)| {
                    ListVal(vec![key.into(), value])
                }).collect::<Vec<_>>().into_iter();
                Ok(Box::new(iter))
            },
            RangeVal(bot, up) => {
                let iter = (bot..up).into_iter().map(|n| {
                    IntegerVal(n)
                });
                Ok(Box::new(iter))
            },
            _ => Err(format!("`{ty}` is not iterable.", ty = self.ty()))
        }
    }

    pub fn index(&self, index: Value) -> ExprResult {
        match self {
            StringVal(s)  => {
                let index = index.as_integer()?;
                let index = if index < 0 {
                    s.len() as i32 + index
                } else {
                    index
                } as usize;
                
                match s.chars().nth(index) {
                    Some(res) => Ok(CharVal(res)),
                    None => Err("Index out of bounds".to_string())
                }
            },
            ListVal(list) => {
                let index = index.as_integer()?;
                let index = if index < 0 {
                    list.len() as i32 + index
                } else {
                    index
                } as usize;
                
                match list.get(index) {
                    Some(res) => Ok(res.clone()),
                    None => Err("Index out of bounds.".to_string())
                }
            },
            MapVal(map) => {
                let key_value = match TryInto::try_into(index) {
                    Ok(kv) => kv,
                    Err(_) => return Err(format!("Invalid Key.").to_string()),
                };
                match map.get(&key_value) {
                    Some(val) => Ok(val.clone()),
                    None => return Err(format!("Absent Key {key_value}.")),
                }
            }
            _ => unreachable!()
        }
    }

    pub fn replace(&self, index: Value, val: Value) -> ExprResult {
        match self {
            StringVal(s)  => {
                let index = index.as_integer()?;
                let index = if index < 0 {
                    s.len() as i32 + index
                } else {
                    index
                };
                let v = val.as_char()?; 
                if s.len() < index as usize {
                    return Err("Index out of bounds".to_string())
                }
                let s = s.chars().enumerate().map(|(i, ch)| {
                    if i == index as usize { v } else { ch }
                }).collect();
                Ok(StringVal(s))
            },
            ListVal(list) => {
                let index = index.as_integer()?;
                let index = if index < 0 {
                    list.len() as i32 + index
                } else {
                    index
                };
                let mut list = list.clone();
                match list.get_mut(index as usize) {
                    Some(v) => {
                        *v = val;
                        Ok(ListVal(list))
                    },
                    None => Err("Index out of bounds.".to_string())
                }
            },
            MapVal(map) => {
                let key_value: KeyValue = match TryInto::try_into(index) {
                    Ok(kv) => kv,
                    Err(_) => return Err(format!("Invalid Key.").to_string()),
                };
                let mut map = map.clone();                
                match map.insert(key_value.clone(), val) {
                    Some(_) => Ok(MapVal(map)),
                    None => return Err(format!("Absent Key {key_value}.")),
                }
            }
            _ => unreachable!()
        }
    }

    // When fails, returns a tuple containing
    //      (String, Option<Error>)
    // Error string of which caused by expression itself  
    // An optionally error, if the execution of application didn't succeed 
    pub fn apply(&self, mut vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)> {
        fn handle<T>(res: Res<T>) -> Result<T, (String, Option<Error>)> {
            res.map_err(|err| { 
                ("Error while evaluating expression".to_string(), Some(err)) 
            })
        }

        match self {
            FunVal     { args, body }     |
            MethodVal  { args, body, .. } |
            ClosureVal { args, body, .. } => {
                if vals.len() != args.len() {
                    return Err((format!("Expected {} arguments but {} given", args.len(), vals.len()), None))
                }
                
                if body.is_empty() {
                    return Ok(NothingVal)
                }

                engine.enter_scope();
                match self {
                    ClosureVal { closure, .. } => {
                        for (arg, v) in closure {
                            engine.define(arg.to_string(), v.to_owned());
                        }
                    },
                    MethodVal { value, .. } => {
                        engine.define(String::from("self"), *value.to_owned());
                    }
                    _ => (),
                }
                handle(engine.collect_definition(body))?;
                for (arg, v) in std::iter::zip(args, vals) {
                    engine.define(arg.to_string(), v);
                }
                
                for stmt in &body[..body.len() - 1] {
                    let state = handle(engine.run(stmt))?;
                    if let State::Return(val) = state {
                        engine.exit_scope();
                        return Ok(val)
                    }
                }
                
                // If the last statement is an expression return the value
                let stmt = body.last().unwrap();
                let rv = match &stmt.data {
                    Stmt::ExprStmt(e) => handle(engine.eval(e))?,
                    _ => match handle(engine.run(stmt))? {
                        State::Return(val) => val,
                        _ => NothingVal,
                    }
                };
                engine.exit_scope();
                
                Ok(rv)
            },
            BuiltInFunVal { arity, fun } => {
                if vals.len() != *arity {
                    return Err((format!("Expected {} arguments but {} given.", arity, vals.len()), None))
                }
                fun(vals, engine)
            }
            BuiltInMethodVal { value, arity, fun } => {
                if vals.len() != *arity {
                    return Err((format!("Expected {} arguments but {} given.", arity, vals.len()), None))
                }
                vals.push(*value.clone());
                fun(vals, engine)
            }
            DefVal        { name, members, .. } |
            BuiltInDefVal { name, members, .. } => {
                if vals.len() != members.len() {
                    return Err((format!("Expected {} arguments but {} given.", members.len(), vals.len()), None))
                }
                let mut map = HashMap::new(); 
                for (mem, v) in std::iter::zip(members, vals) {
                    map.insert(mem.to_string(), v);
                }
                Ok(InstanceVal { type_name: name.to_owned(), members: map })
            }
            TypeVal(ty) => {
                match ty {
                    CustomTy(name) => {
                        let (DefVal       { members, .. } | 
                            BuiltInDefVal { members, .. }) = engine.resolve(name).unwrap() else {
                            unreachable!()
                        };
                        if vals.len() != members.len() {
                            return Err((format!("Expected {} arguments but {} given.", members.len(), vals.len()), None))
                        }
                        let mut map = HashMap::new(); 
                        for (mem, v) in std::iter::zip(members, vals) {
                            map.insert(mem.to_string(), v);
                        }
                        Ok(InstanceVal { type_name: name.to_owned(), members: map })
                    },
                    _ => Err((format!("`{ty}` is not applicable.", ty = self.ty()), None)) 
                }
            },
            _ => Err((format!("`{ty}` is not applicable.", ty = self.ty()), None))
        }
    }
}
