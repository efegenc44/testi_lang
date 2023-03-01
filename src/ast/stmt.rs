use crate::span::Spanned;

use super::expr::Expr;

#[derive(Clone, Debug)]
pub enum Stmt {
    DefStmt {
        name: String,
        mems: Vec<String>
    },
    LetStmt {
        var: String,
        expr: Spanned<Expr>,
    },
    FunStmt {
        name: String,
        args: Vec<String>,
        body: Vec<Spanned<Stmt>>
    },
    IfStmt {
        branches: Vec<(
            Spanned<Expr>,     // Condition 
            Vec<Spanned<Stmt>> // Branch body
        )>,
        elss: Option<Vec<Spanned<Stmt>>>
    },
    WhileStmt {
        cond: Spanned<Expr>,
        body: Vec<Spanned<Stmt>>
    },
    ForStmt {
        var : String,
        iter: Spanned<Expr>,
        body: Vec<Spanned<Stmt>>
    },
    ReturnStmt(Spanned<Expr>),
    ContinueStmt,
    BreakStmt,
    ExprStmt(Spanned<Expr>),
}