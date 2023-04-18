use crate::span::Spanned;

use super::expr::Expr;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<Spanned<Stmt>>
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Import(Vec<String>),
    Def {
        name: String,
        members: Vec<String>,
        methods: Vec<Spanned<Function>> 
    },
    Impl(Function),
    ImplFor {
        ty: String,
        mets: Vec<Spanned<Function>> 
    },
    Let {
        var: String,
        expr: Spanned<Expr>,
    },
    Function(Function),
    Closure {
        fun: Function,
        closure: Vec<String>
    },
    If {
        branches: Vec<(
            Spanned<Expr>,     // Condition 
            Vec<Spanned<Stmt>> // Branch body
        )>,
        elss: Option<Vec<Spanned<Stmt>>>
    },
    While {
        cond: Spanned<Expr>,
        body: Vec<Spanned<Stmt>>
    },
    For {
        var : String,
        iter: Spanned<Expr>,
        body: Vec<Spanned<Stmt>>
    },
    Return(Spanned<Expr>),
    Continue,
    Break,
    Expr(Spanned<Expr>),
}