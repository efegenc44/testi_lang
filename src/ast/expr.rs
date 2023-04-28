use crate::{
    lex::token::Token,
    span::Spanned
};

use super::stmt::Stmt;

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    And,
    Or,
    Xrn,
    Irn
}

impl BinaryOp {
    pub fn get_method_name(&self) -> String {
        match self {
            BinaryOp::Add => String::from("add"),
            BinaryOp::Sub => String::from("sub"),
            BinaryOp::Mul => String::from("mul"),
            BinaryOp::Div => String::from("div"),
            BinaryOp::Mod => String::from("mod"),
            BinaryOp::Eq  => String::from("eq"),
            BinaryOp::Ne  => String::from("ne"),
            BinaryOp::Gt  => String::from("gt"),
            BinaryOp::Ge  => String::from("ge"),
            BinaryOp::Lt  => String::from("lt"),
            BinaryOp::Le  => String::from("le"),
            BinaryOp::And => String::from("and"),
            BinaryOp::Or  => String::from("or"),
            BinaryOp::Xrn => String::from("xrn"),
            BinaryOp::Irn => String::from("irn"),
        }
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "mod"),
            BinaryOp::Eq  => write!(f, "=="),
            BinaryOp::Ne  => write!(f, "!="),
            BinaryOp::Gt  => write!(f, ">"),
            BinaryOp::Ge  => write!(f, ">="),
            BinaryOp::Lt  => write!(f, "<"),
            BinaryOp::Le  => write!(f, "<="),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or  => write!(f, "or"),
            BinaryOp::Xrn => write!(f, ".."),
            BinaryOp::Irn => write!(f, "..="),
        }
    }
}

impl From<Token> for BinaryOp {
    fn from(value: Token) -> Self {
        match value {
            Token::PLUS         => Self::Add,
            Token::MINUS        => Self::Sub,
            Token::STAR         => Self::Mul,
            Token::SLASH        => Self::Div,
            Token::KMOD         => Self::Mod,
            Token::DEQUAL       => Self::Eq,
            Token::BANGEQUAL    => Self::Ne,
            Token::GREATER      => Self::Gt,
            Token::GREATEREQUAL => Self::Ge,
            Token::LESS         => Self::Lt,
            Token::LESSEQUAL    => Self::Le,
            Token::KAND         => Self::And,
            Token::KOR          => Self::Or,
            Token::TWODOT       => Self::Xrn,
            Token::TWODOTEQUAL  => Self::Irn,
            _ => unreachable!()
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    pub fn get_method_name(&self) -> String {
        match self {
            UnaryOp::Neg  => String::from("neg"),
            UnaryOp::Not  => String::from("not"),
        }
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Neg  => write!(f, "-"),
            UnaryOp::Not  => write!(f, "!"),
        }
    }
}

impl From<Token> for UnaryOp {
    fn from(value: Token) -> Self {
        match value {
            Token::MINUS => Self::Neg,
            Token::BANG  => Self::Not,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AssignOp {
    Normal,
    Composite(BinaryOp)
}

impl std::fmt::Display for AssignOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignOp::Normal         => write!(f, "="),
            AssignOp::Composite(bop) => write!(f, "{bop}")
        }
    }
}

impl From<Token> for AssignOp {
    fn from(value: Token) -> Self {
        match value {
            Token::EQUAL      => Self::Normal,
            Token::PLUSEQUAL  => Self::Composite(BinaryOp::Add),
            Token::MINUSEQUAL => Self::Composite(BinaryOp::Sub),
            Token::STAREQUAL  => Self::Composite(BinaryOp::Mul),
            Token::SLASHEQUAL => Self::Composite(BinaryOp::Div),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Assignment {
        op: AssignOp,
        assignee: Box<Spanned<Expr>>,
        expr: Box<Spanned<Expr>>
    },
    Binary {
        op   : BinaryOp,
        left : Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    Unary {
        op     : UnaryOp,
        operand: Box<Spanned<Expr>>,
    },
    Application {
        applied: Box<Spanned<Expr>>,
        exprs  : Vec<Spanned<Expr>>
    },
    Index {
        from: Box<Spanned<Expr>>,
        index_expr: Box<Spanned<Expr>>,
    },
    Access {
        from: Box<Spanned<Expr>>,
        member: String,
    },
    TypeTest {
        expr: Box<Spanned<Expr>>,
        ty: Box<Spanned<Expr>>
    },
    Function {
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>,
        closure: Option<Vec<String>>
    },
    List(Vec<Spanned<Expr>>),
    Map(Vec<(Spanned<Expr>, Spanned<Expr>)>),
    Natural(usize),
    Float(f32),
    String(String),
    Bool(bool),
    Variable(String),
    Copy(Box<Spanned<Expr>>),
    Nothing,
}