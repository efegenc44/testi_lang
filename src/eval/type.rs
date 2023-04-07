use Type::*;

#[derive(Clone, PartialEq)]
pub enum Type {
    IntegerTy,
    FloatTy,
    StringTy,
    CharTy,
    BoolTy,
    ListTy,
    MapTy,
    RangeTy,
    MethodTy,
    FunTy,
    NothingTy,
    CustomTy(String),
    TyTy,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerTy      => write!(f, "Integer"),
            FloatTy        => write!(f, "Float"),
            StringTy       => write!(f, "String"),
            CharTy         => write!(f, "Character"),
            BoolTy         => write!(f, "Bool"),
            ListTy         => write!(f, "List"),
            MapTy          => write!(f, "Map"),
            RangeTy        => write!(f, "Range"),
            MethodTy       => write!(f, "Method"),
            FunTy          => write!(f, "Function"),
            NothingTy      => write!(f, "Nothing"),
            CustomTy(name) => write!(f, "{name}"),
            TyTy           => write!(f, "Type"),
            
        }
    }
}