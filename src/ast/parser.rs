use crate::{
    span::{ Span, Spanned }, 
    lex::token::Token::{ self, * },
    error::error::{ Res, simple_error }
};

use super::{
    expr::Expr,
    stmt::{ Stmt, self },
};

pub struct Parser {
    cursor: usize,
    tokens: Vec<Spanned<Token>>,
    
    in_loop: usize,
    in_fun: usize,
}

impl Iterator for Parser {
    type Item = Res<Spanned<Stmt>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse()
    }
}

macro_rules! binary_expr_precedence {
    ($name: ident $upper: ident $($op: ident)|+ LEFT_ASSOC) => {
        fn $name(&mut self) -> Res<Spanned<Expr>> {
            let mut left = self.$upper()?;
            while let $( $op )|+ = self.peek() {
                let op = self.peek().into();
                self.advance();
                let right = self.$upper()?;
                let span = left.span.clone().extend(&right.span);
                left = Spanned::new(Expr::Binary { 
                    op, 
                    left: Box::new(left),
                    right: Box::new(right) 
                }, span)
            }
            Ok(left)
        }
    };

    ($name: ident $upper: ident $($op: ident)|+ NO_ASSOC) => {
        fn $name(&mut self) -> Res<Spanned<Expr>> {
            let mut left = self.$upper()?;
            if let $( $op )|+ = self.peek() {
                let op = self.peek().into();
                self.advance();
                let right = self.$upper()?;
                let span = left.span.clone().extend(&right.span);
                left = Spanned::new(Expr::Binary {
                    op,
                    left: Box::new(left), 
                    right: Box::new(right)
                }, span)
            }
            Ok(left)
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Self { 
            tokens,
            cursor: 0, 
            in_loop: 0,
            in_fun: 0
        }
    }

    fn peek(&self) -> Token {
        self.tokens[self.cursor].data.clone()
    }

    fn span(&self) -> Span {
        self.tokens[self.cursor].span.clone()
    }

    #[inline] // I am a low level dev 😎
    fn advance(&mut self) {
        self.cursor += 1;
    }

    fn next(&mut self) -> Span {
        let span = self.span();
        self.advance();
        span
    }

    fn consume(&mut self, token: Token) -> Res<Span> {
        if self.peek() == token {
            let span = self.span();
            self.advance();
            Ok(span)
        } else {
            simple_error(
                format!("Expected a `{token}`, not `{token2}`", token2 = self.peek()),
                self.span()
            )
        }
    }

    fn consume_symbol(&mut self) -> Res<String> {
        if let SYMBOL(sym) = self.peek() {
            let sym = sym.to_string();
            self.advance();
            Ok(sym)
        } else {
            simple_error(
                format!("Expected a `Symbol`, not `{token}`", token = self.peek()),
                self.span()
            )
        }
    }

    fn optional(&mut self, token: Token) -> bool {
        if self.peek() == token {
            self.advance();
            return true
        }
        false
    }

    fn comma_seperated_paren<F, T>(&mut self, f: F) -> Res<Spanned<Vec<T>>>
    where 
        F: Fn(&mut Parser) -> Res<T>
    {
        let lparen_span = self.consume(LPAREN)?;
        let mut things = vec![];
        if self.peek() != RPAREN {
            things.push(f(self)?);
            while self.optional(COMMA) {
                things.push(f(self)?);
            }
        }
        let rparen_span = self.consume(RPAREN)?;
        Ok(Spanned::new(things, lparen_span.extend(&rparen_span)))
    }

    // ye
    fn squared_expr_inner<F, T>(&mut self, f: F) -> Res<Vec<T>>
    where 
        F: Fn(&mut Parser) -> Res<T>
    {
        let mut things = vec![];
        if self.peek() != RSQUARE {
            things.push(f(self)?);
            while self.optional(COMMA) {
                things.push(f(self)?);
            }
        }
        Ok(things)
    }

    fn parse_until<F, T>(&mut self, f: F, token: Token) -> Res<Vec<T>> 
    where
        F: Fn(&mut Parser) -> Res<T>
    {
        let mut seq = vec![];
        while self.peek() != token {
            seq.push(f(self)?)
        }
        Ok(seq)
    }
    
    fn parse_until_one_of<F, T, const N: usize>(&mut self, f: F, tokens: [Token; N]) -> Res<Vec<T>> 
    where
        F: Fn(&mut Parser) -> Res<T>
    {
        let mut seq = vec![];
        while !tokens.contains(&self.tokens.get(self.cursor).unwrap().data) {
            seq.push(f(self)?)
        }
        Ok(seq)
    }

    fn product(&mut self) -> Res<Spanned<Expr>> {
        let ct = self.peek();
        let cs = self.span();
        
        // In the next step, it is guaranteed that we consume the token
        // so we can basically advance here to remove repeating `advance`s
        self.advance();

        let mut expr = match ct {
            SYMBOL(sym)  => Spanned::new(Expr::Variable(sym), cs),
            STRING(s)    => Spanned::new(Expr::String(s), cs),
            CHAR(ch)     => Spanned::new(Expr::Char(ch), cs),
            NATURAL(int) => Spanned::new(Expr::Natural(int), cs),
            FLOAT(float) => Spanned::new(Expr::Float(float), cs),
            KTRUE        => Spanned::new(Expr::Bool(true), cs),
            KFALSE       => Spanned::new(Expr::Bool(false), cs),
            KNOTHING     => Spanned::new(Expr::Nothing, cs),
            STAR => {
                let product = self.product()?;
                let product_span = product.span.clone();
                Spanned::new(Expr::Copy(Box::new(product)), cs.extend(&product_span)) 
            } 
            BANG |
            MINUS => {
                let op = ct.into();
                let product = self.product()?;
                let product_span = product.span.clone();
                let operand = Box::new(product);
                Spanned::new(Expr::Unary { op, operand }, cs.extend(&product_span))
            },
            LPAREN => {
                let expr = self.expr()?.data;
                let rparen_span = self.span();
                self.consume(RPAREN)?;
                Spanned::new(expr, cs.extend(&rparen_span))
            },
            LSQUARE => {
                let list = self.squared_expr_inner(Self::expr)?;
                let rsquare_span = self.consume(RSQUARE)?;
                Spanned::new(Expr::List(list), cs.extend(&rsquare_span))
            },
            HASH => {
                self.consume(LSQUARE)?;
                let pairs = self.squared_expr_inner(|parser| {
                    let key = parser.expr()?;
                    parser.consume(COLON)?;
                    let value = parser.expr()?;
                    Ok((key, value))
                })?; 
                let rcurly_span = self.consume(RSQUARE)?;
                Spanned::new(Expr::Map(pairs), cs.extend(&rcurly_span))
            },
            BACKSLASH => {
                let closure = if self.peek() == LPAREN {
                    Some(self.comma_seperated_paren(Self::consume_symbol)?.data)
                } else { None };
                let mut args = vec![];
                if self.peek() != DOT {
                    args.push(self.consume_symbol()?);
                    while self.optional(COMMA) {
                        args.push(self.consume_symbol()?);
                    }
                }
                self.consume(DOT)?;
                let body = self.parse_until(Self::statement, KEND)?;
                let end_span = self.next();
                Spanned::new(Expr::Function { args, body, closure }, cs.extend(&end_span))
            }
            _ => return simple_error(format!("Unexpected token `{ct}`."), cs)
        };

        loop {
            let start_span = expr.span.clone();
            match self.peek() {
                LPAREN => {
                    let exprs = self.comma_seperated_paren(Self::expr)?;
                    expr = Spanned::new(Expr::Application {
                        applied: Box::new(expr), 
                        exprs: exprs.data
                    }, start_span.extend(&exprs.span)) 
                },
                LSQUARE => {
                    self.advance();
                    let index_expr = Box::new(self.expr()?);
                    let rsquare_span = self.consume(RSQUARE)?;
                    expr = Spanned::new(Expr::Index {
                        from: Box::new(expr), 
                        index_expr
                    }, start_span.extend(&rsquare_span))
                },
                DOT => {
                    self.advance();
                    let member = self.consume_symbol()?;
                    let end_span = self.span();
                    expr = Spanned::new(Expr::Access {
                        from: Box::new(expr), 
                        member
                    }, start_span.extend(&end_span))
                }
                _ => break
            }
        }
        Ok(expr)
    }

    //                      Name       Upper      Operators               Associavity
    binary_expr_precedence!(term       product    STAR    | SLASH | KMOD  LEFT_ASSOC);
    binary_expr_precedence!(arith      term       PLUS    | MINUS         LEFT_ASSOC);
    binary_expr_precedence!(range      arith      TWODOT  | TWODOTEQUAL   NO_ASSOC);
    binary_expr_precedence!(comparison range      LESS    | LESSEQUAL |
                                                  GREATER | GREATEREQUAL  NO_ASSOC);
    binary_expr_precedence!(type_test  comparison KIS                     NO_ASSOC);
    binary_expr_precedence!(equality   type_test  DEQUAL  | BANGEQUAL     NO_ASSOC);
    binary_expr_precedence!(logic_and  equality   KAND                    LEFT_ASSOC);
    binary_expr_precedence!(logic_or   logic_and  KOR                     LEFT_ASSOC);

    fn assignment(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.logic_or()?;
        if let EQUAL      | PLUSEQUAL | 
               MINUSEQUAL | STAREQUAL | 
               SLASHEQUAL = self.peek() {
            let op = self.peek().into();
            self.advance();
            let right = self.assignment()?;
            let span = left.span.clone().extend(&right.span);
            left = Spanned::new(Expr::Assignment { 
                op,
                assignee: Box::new(left), 
                expr    : Box::new(right) 
            }, span)
        }
        Ok(left)
    }

    #[inline]
    fn expr(&mut self) -> Res<Spanned<Expr>> {
        self.assignment()
    }

    fn let_statement(&mut self) -> Res<Spanned<Stmt>> {
        let let_span = self.next();
        let var = self.consume_symbol()?;
        self.consume(EQUAL)?;
        let expr = self.expr()?;
        let span = let_span.extend(&expr.span);
        Ok(Spanned::new(Stmt::Let { var, expr }, span))
    }

    fn fun_statement(&mut self) -> Res<Spanned<Stmt>> {
        let fun_span = self.next();
        let closure = if self.peek() == LPAREN {
            Some(self.comma_seperated_paren(Self::consume_symbol)?.data)
        } else { None };
        self.in_fun += 1;
        let name = self.consume_symbol()?;
        let args = self.comma_seperated_paren(Self::consume_symbol)?.data;
        let body = self.parse_until(Self::statement, KEND)?;
        let end_span = self.next();
        self.in_fun -= 1;
        let fun = stmt::Function { name, args, body };
        Ok(Spanned::new(match closure {
            Some(closure) => Stmt::Closure { fun, closure },
            None => Stmt::Function(fun),
        }, fun_span.extend(&end_span)))
    }

    fn return_statement(&mut self) -> Res<Spanned<Stmt>> {
        let return_span = self.next();
        if self.in_fun == 0 {
            return simple_error("Return statement outside of a function definition.", return_span)
        }
        let expr = self.expr()?;
        let expr_span = expr.span.clone();
        Ok(Spanned::new(Stmt::Return(expr), return_span.extend(&expr_span)))
    }

    fn branch(&mut self) -> Res<(Spanned<Expr>, Vec<Spanned<Stmt>>)> {
        self.advance();
        let cond = self.expr()?;
        self.consume(KTHEN)?;
        let body = self.parse_until_one_of(Self::statement, [KEF, KELSE, KEND])?;
        Ok((cond, body))
    }

    fn if_statement(&mut self) -> Res<Spanned<Stmt>> {
        let if_span = self.span();
        let branches = self.parse_until_one_of(Self::branch, [KELSE, KEND])?;
        let elss = if self.optional(KELSE) {
            Some(self.parse_until(Self::statement, KEND)?)
        } else {
            None
        };
        let end_span = self.next();
        Ok(Spanned::new(Stmt::If { branches, elss }, if_span.extend(&end_span)))
    }

    fn while_statement(&mut self) -> Res<Spanned<Stmt>> {
        self.in_loop += 1;
        let while_span = self.next();
        let cond = self.expr()?;
        self.consume(KTHEN)?;
        let body = self.parse_until(Self::statement, KEND)?;
        let end_span = self.next();
        self.in_loop -= 1;
        Ok(Spanned::new(Stmt::While { cond, body }, while_span.extend(&end_span)))
    }

    fn continue_statement(&mut self) -> Res<Spanned<Stmt>> {
        let cnt_span = self.next();
        if self.in_loop == 0 {
            return simple_error("Continue statement outside of a loop.", cnt_span)
        }
        Ok(Spanned::new(Stmt::Continue, cnt_span))
    }
    
    fn break_statement(&mut self) -> Res<Spanned<Stmt>> {
        let break_span = self.next();
        if self.in_loop == 0 {
            return simple_error("Break statement outside of a loop.", break_span)
        }
        Ok(Spanned::new(Stmt::Break, break_span))
    }

    fn for_statement(&mut self) -> Res<Spanned<Stmt>> {
        self.in_loop += 1;
        let for_span = self.next();
        let var = self.consume_symbol()?;
        self.consume(KIN)?;
        let iter = self.expr()?;
        self.consume(KTHEN)?;
        let body = self.parse_until(Self::statement, KEND)?;
        let end_span = self.next();
        self.in_loop -= 1;
        Ok(Spanned::new(Stmt::For { var, iter, body }, for_span.extend(&end_span)))
    }

    fn def_statement(&mut self) -> Res<Spanned<Stmt>> {
        let def_span = self.next();
        let name = self.consume_symbol()?;
        let members = self.parse_until_one_of(Self::consume_symbol, [KEND, KIMPL])?;
        let methods = if self.optional(KIMPL) {
            self.parse_until(Self::def_method, KEND)?
        } else { vec![] };
        let end_span = self.next();
        Ok(Spanned::new(Stmt::Definition { name, members, methods }, def_span.extend(&end_span)))
    }

    fn def_method(&mut self) -> Res<Spanned<stmt::Function>> {
        self.in_fun += 1;
        let fun_span = self.next();
        let name = self.consume_symbol()?;
        let args = self.comma_seperated_paren(Self::consume_symbol)?.data;
        let body = self.parse_until(Self::statement, KEND)?;
        let end_span = self.next();
        self.in_fun -= 1;
        Ok(Spanned::new(stmt::Function { name, args, body }, fun_span.extend(&end_span)))
    }

    fn impl_statement(&mut self) -> Res<Spanned<Stmt>> {
        let impl_span = self.next();
        if self.optional(KFOR) {
            let ty = self.consume_symbol()?;
            let mets = self.parse_until(Self::def_method, KEND)?;            
            let end_span = self.next();
            return Ok(Spanned::new(Stmt::ImplFor { ty, methods: mets }, impl_span.extend(&end_span)))
        }
        self.in_fun += 1;
        let name = self.consume_symbol()?;
        let args = self.comma_seperated_paren(Self::consume_symbol)?.data;
        let body = self.parse_until(Self::statement, KEND)?;
        let end_span = self.next();
        self.in_fun -= 1;
        Ok(Spanned::new(Stmt::Impl(stmt::Function { name, args, body }), impl_span.extend(&end_span)))
    }

    fn import_statement(&mut self) -> Res<Spanned<Stmt>> {
        let import_span = self.next();
        let mut strings = vec![self.consume_symbol()?];
        while self.optional(DOT) {
            strings.push(self.consume_symbol()?)
        }
        Ok(Spanned::new(Stmt::Import(strings), import_span))
    }

    fn statement(&mut self) -> Res<Spanned<Stmt>> {
        let stmt = match self.peek() {
            KLET      => self.let_statement()?,
            KFUN      => self.fun_statement()?,
            KRETURN   => self.return_statement()?,
            KIF       => self.if_statement()?,
            KWHILE    => self.while_statement()?,
            KCONTINUE => self.continue_statement()?,
            KBREAK    => self.break_statement()?,
            KFOR      => self.for_statement()?,
            KDEF      => self.def_statement()?,
            KIMPL     => self.impl_statement()?,
            KIMPORT   => self.import_statement()?,
            _ => {
                let expr = self.expr()?;
                let span = expr.span.clone();
                Spanned::new(Stmt::Expr(expr), span)
            }
        };
        let _ = self.optional(SEMICOLON);
        Ok(stmt)
    }
    
    fn parse(&mut self) -> Option<Res<Spanned<Stmt>>> {
        if self.peek() == END {
            return None
        }
        Some(self.statement())
    }
}
