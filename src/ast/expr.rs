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
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
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
    AssignmentExpr {
        op: AssignOp,
        assignee: Box<Spanned<Expr>>,
        expr: Box<Spanned<Expr>>
    },
    BinaryExpr{
        op   : BinaryOp,
        left : Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    UnaryExpr{
        op     : UnaryOp,
        operand: Box<Spanned<Expr>>,
    },
    ApplicationExpr {
        applied: Box<Spanned<Expr>>,
        exprs  : Vec<Spanned<Expr>>
    },
    IndexExpr {
        from: Box<Spanned<Expr>>,
        index_expr: Box<Spanned<Expr>>,
    },
    AccessExpr {
        from: Box<Spanned<Expr>>,
        member: String,
    },
    TypeTestExpr {
        expr: Box<Spanned<Expr>>,
        ty: Box<Spanned<Expr>>
    },
    FunctionExpr {
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>,
        closure: Option<Vec<String>>
    },
    ListExpr(Vec<Spanned<Expr>>),
    MapExpr(Vec<(Spanned<Expr>, Spanned<Expr>)>),
    NaturalExpr(usize),
    FloatExpr(f32),
    StringExpr(String),
    CharExpr(char),
    BoolExpr(bool),
    Variable(String),
    NothingExpr,
}