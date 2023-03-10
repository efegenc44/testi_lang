use std::collections::HashMap;

use Value::*;

use crate::{
    ast::stmt::Stmt, 
    span::Spanned, error::error::{Error, Res}
};

use super::{
    r#type::Type::{ self, * }, 
    evaluator::{Engine, State}
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

#[derive(Clone)]
pub enum Value {
    IntegerVal(i32),
    FloatVal(f32),
    StringVal(String),
    BoolVal(bool),
    ListVal(Vec<Value>),
    MapVal(HashMap<KeyValue, Value>),
    RangeVal(i32, i32),
    InstanceVal {
        type_name: String, 
        members: HashMap<String, Value>,
    },
    FunVal {
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>,
    },
    BuiltInFunVal {
        arity: usize,
        fun: fn(vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)>
    },
    NothingVal,
    TypeVal(Type),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerVal(int)    => write!(f, "{int}"),
            FloatVal(float)    => write!(f, "{float}"),
            StringVal(s)       => write!(f, "\"{s}\""),
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
                if members.len() == 0 {
                    return write!(f, "def {type_name} end")
                }
                let mut first = true;
                write!(f, "def {type_name} ")?;
                for (key, value) in members {
                    if first {
                        write!(f, "{key}: {value}")?;
                        first = false;
                    } else {
                        write!(f, ", {key}: {value}")?;
                    }
                }
                write!(f, " end")?;
                Ok(())
            },
            RangeVal(bot, up)  => write!(f, "{bot}..{up}"),
            FunVal {..}        => write!(f, "<function>"),
            BuiltInFunVal {..} => write!(f, "<built-in function>"),
            NothingVal         => write!(f, "nothing"),
            TypeVal(ty)        => write!(f, "{ty}"),
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
            IntegerVal(_)      => IntegerTy,
            FloatVal(_)        => FloatTy,
            StringVal(_)       => StringTy,
            BoolVal(_)         => BoolTy,
            ListVal(_)         => ListTy,
            MapVal(_)          => MapTy,
            RangeVal(_, _)     => RangeTy,
            FunVal {..}        => FunTy,
            BuiltInFunVal {..} => FunTy,
            NothingVal         => NothingTy,
            TypeVal(_)         => TyTy,
            
            InstanceVal { type_name, members } => CustomTy { 
                name: type_name.to_owned(),
                mems: members.keys().map(|key| {
                    key.to_owned()
                }).collect()
            },
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
            (StringVal(s1)   , StringVal(s2)   ) => Ok(StringVal(s1.to_owned() + s2)),
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
                    StringVal(String::from(ch))
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
                let IntegerVal(index) = index else {
                    return Err(format!("Index must be an `Integer` not `{ty}`.", ty = index.ty()))
                };
                match s.chars().nth(index as usize) {
                    Some(res) => Ok(StringVal(String::from(res))),
                    None => Err("Index out of bounds".to_string())
                }
            },
            ListVal(list) => {
                let IntegerVal(index) = index else {
                    return Err(format!("Index must be an `Integer` not `{ty}`.", ty = index.ty()))
                };
                match list.get(index as usize) {
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

    // When fails, returns a tuple containing
    //      (String, Option<Error>)
    // Error string of which caused by expression itself  
    // An optionally error, if the execution of application didn't succeed 
    pub fn apply(&self, vals: Vec<Value>, engine: &mut Engine) -> Result<Value, (String, Option<Error>)> {
        fn handle<T>(res: Res<T>) -> Result<T, (String, Option<Error>)> {
            res.map_err(|err| { 
                ("Error while evaluating expression".to_string(), Some(err)) 
            })
        }

        match self {
            FunVal { args, body } => {
                if vals.len() != args.len() {
                    return Err((format!("Expected {} arguments but {} given", args.len(), vals.len()), None))
                }
                
                if body.is_empty() {
                    return Ok(NothingVal)
                }

                engine.enter_scope();
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
            TypeVal(ty) => {
                match ty {
                    CustomTy { name, mems } => {
                        if vals.len() != mems.len() {
                            return Err((format!("Expected {} arguments but {} given.", mems.len(), vals.len()), None))
                        }
                        let mut map = HashMap::new(); 
                        for (mem, v) in std::iter::zip(mems, vals) {
                            map.insert(mem.to_string(), v);
                        }
                        Ok(InstanceVal { type_name: name.to_owned(), members: map })
                    },

                    // Type Conversions from here
                    IntegerTy => {
                        if vals.len() != 1 {
                            return Err((format!("Expected {} arguments but {} given.", 1, vals.len()), None))
                        }
                        match &vals[0] {
                            IntegerVal(int) => Ok(IntegerVal(*int)),
                            FloatVal(float) => Ok(IntegerVal(*float as i32)),
                            StringVal(s)    => match s.parse() {
                                Ok(int) => Ok(IntegerVal(int)),
                                Err(_)  => Err((format!("Couldn't convert `{}` to `Integer`", vals[0]), None))
                            }
                            BoolVal(b) => Ok(IntegerVal(*b as i32)),
                            NothingVal => Ok(IntegerVal(0)),
                            _ => Err((format!("Can't convert `{}` to `Integer`", vals[0].ty()), None))
                        }
                    },
                    FloatTy => {
                        if vals.len() != 1 {
                            return Err((format!("Expected {} arguments but {} given.", 1, vals.len()), None))
                        }
                        match &vals[0] {
                            IntegerVal(int) => Ok(FloatVal(*int as f32)),
                            FloatVal(float) => Ok(FloatVal(*float)),
                            StringVal(s)    => match s.parse() {
                                Ok(int) => Ok(FloatVal(int)),
                                Err(_)  => Err((format!("Couldn't convert `{}` to `Float`", vals[0]), None)),
                            }
                            BoolVal(b) => Ok(FloatVal(*b as i32 as f32)),
                            NothingVal => Ok(FloatVal(0.)),
                            _ => Err((format!("Can't convert `{}` to `Float`", vals[0].ty()), None))
                        }
                    },
                    StringTy => {
                        if vals.len() != 1 {
                            return Err((format!("Expected {} arguments but {} given.", 1, vals.len()), None))
                        }
                        Ok(StringVal(vals[0].to_string()))
                    },
                    BoolTy => {
                        if vals.len() != 1 {
                            return Err((format!("Expected {} arguments but {} given.", 1, vals.len()), None))
                        }
                        match &vals[0] {
                            IntegerVal(int)   => Ok(BoolVal(int != &0)),
                            FloatVal(float)   => Ok(BoolVal(float != &0.)),
                            StringVal(s)      => Ok(BoolVal(!s.is_empty())),
                            BoolVal(b)        => Ok(BoolVal(*b)),
                            ListVal(list)     => Ok(BoolVal(!list.is_empty())),
                            MapVal(map)       => Ok(BoolVal(!map.is_empty())),
                            RangeVal(bot, up) => Ok(BoolVal(bot < up)),
                            NothingVal        => Ok(BoolVal(false)),
                            _ => Err((format!("Can't convert `{}` to `Bool`", vals[0].ty()), None))
                        }
                    },
                    ListTy => {
                        if vals.len() != 1 {
                            return Err((format!("Expected {} arguments but {} given.", 1, vals.len()), None))
                        }
                        match &vals[0] {
                            ListVal(_) => Ok(vals[0].clone()),
                            RangeVal(bot, up) => {
                                let list = (*bot..*up).map(|x| {
                                    IntegerVal(x)
                                }).collect();
                                Ok(ListVal(list))
                            },
                            NothingVal => Ok(ListVal(vec![])),
                            _ => Err((format!("Can't convert `{}` to `List`", vals[0].ty()), None)),
                        }
                    },
                    MapTy => {
                        let mut map = HashMap::new();
                        for val in &vals {
                            if let ListVal(list) = val {
                                if list.len() == 2 {
                                    let key = match list[0].to_owned().try_into() {
                                        Ok(key) => key,
                                        Err(err) => return Err((err, None)),
                                    };
                                    let value = list[1].to_owned();
                                    map.insert(key, value);
                                    
                                    continue;
                                }
                            }
                            return Err((format!("Couldn't convert `{}` to `Map` pair", val), None))
                        }
                        Ok(MapVal(map))
                    },
                    RangeTy => {
                        if vals.len() == 0 || vals.len() > 2 {
                            return Err((format!("Expected 1 or 2 arguments but {} given.", vals.len()), None))
                        }
                        match vals.len() {
                            1 => {
                                let IntegerVal(int) = vals[0] else {
                                    return Err((format!("Expected `Integer` not `{ty}`", ty = vals[0].ty()), None));
                                };
                                Ok(RangeVal(0, int))
                            },
                            2 => {
                                let IntegerVal(int1) = vals[0] else {
                                    return Err((format!("Expected `Integer` not `{ty}`", ty = vals[0].ty()), None));
                                };
                                let IntegerVal(int2) = vals[1] else {
                                    return Err((format!("Expected `Integer` not `{ty}`", ty = vals[0].ty()), None));
                                };
                                Ok(RangeVal(int1, int2))
                            },
                            _ => unreachable!()
                        }
                    },
                    NothingTy => Ok(NothingVal),
                    FunTy     |
                    TyTy      => Err((format!("Can't convert to `{ty}`"), None)),
                }
            },
            _ => Err((format!("`{ty}` is not applicable.", ty = self.ty()), None))
        }
    }
}
