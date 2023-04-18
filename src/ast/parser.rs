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
        self.tokens[self.cursor].span
    }

    fn advance(&mut self) {
        self.cursor += 1;
    }

    fn consume(&mut self, token: &Token) -> Res<()> {
        if self.peek() == *token {
            self.advance();
            Ok(())
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

    fn optional(&mut self, token: &Token) -> bool {
        if self.peek() == *token {
            self.advance();
            return true
        }
        false
    }

    fn paren_exprs(&mut self) -> Res<Spanned<Vec<Spanned<Expr>>>> {
        let lparen_span = self.span();
        // consume LPAREN and advance
        self.advance();
        let mut exprs = vec![];
        if self.peek() != RPAREN {
            exprs.push(self.expr()?);
            while self.optional(&COMMA) {
                exprs.push(self.expr()?);
            }
        }
        let rparen_span = self.span();
        self.consume(&RPAREN)?;
        Ok(Spanned::new(exprs, lparen_span.extend(rparen_span)))
    }

    fn args(&mut self) -> Res<Vec<String>> {
        self.consume(&LPAREN)?;
        let mut args = vec![];
        if self.peek() != RPAREN {
            args.push(self.consume_symbol()?);
            while self.optional(&COMMA) {
                args.push(self.consume_symbol()?);
            }
        }
        self.consume(&RPAREN)?;
        Ok(args)
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
            PLUS |
            BANG |
            MINUS => {
                let op = ct.into();
                let product = self.product()?;
                let product_span = product.span;
                let operand = Box::new(product);
                Spanned::new(Expr::Unary { op, operand }, cs.extend(product_span))
            },
            LPAREN => {
                let expr = self.expr()?.data;
                let rparen_span = self.span();
                self.consume(&RPAREN)?;
                Spanned::new(expr, cs.extend(rparen_span))
            },
            LSQUARE => {
                let mut list = vec![];                
                if self.peek() != RSQUARE {
                    list.push(self.expr()?);
                    while self.optional(&COMMA) {
                        list.push(self.expr()?);
                    }
                }
                let rsquare_span = self.span();
                self.consume(&RSQUARE)?;
                Spanned::new(Expr::List(list), cs.extend(rsquare_span))
            },
            HASH => {
                self.consume(&LSQUARE)?;
                let mut pairs = vec![];
                if self.peek() != RSQUARE {
                    let key = self.expr()?;
                    self.consume(&COLON)?;
                    let value = self.expr()?;
                    pairs.push((key, value));
                    while self.optional(&COMMA) {
                        let key = self.expr()?;
                        self.consume(&COLON)?;
                        let value = self.expr()?;
                        pairs.push((key, value));
                    }
                }
                let rcurly_span = self.span();
                self.consume(&RSQUARE)?;
                Spanned::new(Expr::Map(pairs), cs.extend(rcurly_span))
            },
            BACKSLASH => {
                let closure = if self.peek() == LPAREN {
                    Some(self.args()?)
                } else {
                    None
                };
                let mut args = vec![];
                if self.peek() != DOT {
                    args.push(self.consume_symbol()?);
                    while self.optional(&COMMA) {
                        args.push(self.consume_symbol()?);
                    }
                }
                self.consume(&DOT)?;
                let mut body = vec![];
                while self.peek() != KEND {
                    body.push(self.statement()?)
                }
                let backslash_span = self.span();
                self.advance();
                Spanned::new(Expr::Function { args, body, closure }, cs.extend(backslash_span))
            }
            _ => {
                return simple_error(format!("Unexpected token `{ct}`."), cs)
            }
        };

        loop {
            match self.peek() {
                LPAREN => {
                    let exprs = self.paren_exprs()?;
                    let start_span = expr.span;
                    expr = Spanned::new(Expr::Application {
                        applied: Box::new(expr), 
                        exprs: exprs.data
                    }, start_span.extend(exprs.span)) 
                },
                LSQUARE => {
                    self.advance();
                    let start_span = expr.span;
                    let index_expr = Box::new(self.expr()?);
                    let rsquare_span = self.span();
                    self.advance();
                    expr = Spanned::new(Expr::Index {
                        from: Box::new(expr), 
                        index_expr
                    }, start_span.extend(rsquare_span))
                },
                DOT => {
                    self.advance();
                    let start_span = expr.span;
                    let member = self.consume_symbol()?;
                    let end_span = self.span();
                    expr = Spanned::new(Expr::Access {
                        from: Box::new(expr), 
                        member
                    }, start_span.extend(end_span))
                }
                _ => break
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.product()?;
        while let STAR | SLASH | KMOD = self.peek() {
            let op = self.peek().into();
            self.advance();
            let right = self.product()?;
            let span = left.span.extend(right.span);
            left = Spanned::new(Expr::Binary { 
                op, 
                left : Box::new(left), 
                right: Box::new(right) 
            }, span)
        } 
        Ok(left)
    }

    fn arith(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.term()?;        
        while let PLUS | MINUS = self.peek() {
            let op = self.peek().into();
            self.advance();
            let right = self.term()?;
            let span = left.span.extend(right.span);
            left = Spanned::new(Expr::Binary { 
                op, 
                left : Box::new(left), 
                right: Box::new(right) 
            }, span)
        } 
        Ok(left)
    }

    fn range(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.arith()?;
        if let TWODOT | TWODOTEQUAL = self.peek() {
            let op = self.peek().into();
            self.advance();
            let right = self.arith()?;
            let span = left.span.extend(right.span);
            left = Spanned::new(Expr::Binary {
                op,
                left : Box::new(left), 
                right: Box::new(right)
            }, span)
        }
        Ok(left)
    }

    fn comparison(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.range()?;
        if let LESS    | LESSEQUAL    |
               GREATER | GREATEREQUAL = self.peek() 
        {
            let op = self.peek().into();
            self.advance();    
            let right = self.range()?;
            let span = left.span.extend(right.span);
            left = Spanned::new(Expr::Binary { 
                op, 
                left : Box::new(left), 
                right: Box::new(right) 
            }, span)
        }
        Ok(left)
    }

    fn type_test(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.comparison()?;
        if let KIS = self.peek() {
            self.advance();
            let right = self.comparison()?;
            let span = left.span.extend(right.span);
            left = Spanned::new(Expr::TypeTest { 
                expr: Box::new(left), 
                ty: Box::new(right) 
            }, span)
        }
        Ok(left)
    }

    fn equality(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.type_test()?;
        while let DEQUAL  | BANGEQUAL = self.peek() {
            let op = self.peek().into();
            self.advance();    
            let right = self.type_test()?;
            let span = left.span.extend(right.span);
            left = Spanned::new(Expr::Binary { 
                op, 
                left : Box::new(left), 
                right: Box::new(right) 
            }, span)
        }
        Ok(left)
    }

    fn logic_and(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.equality()?;
        while let KAND = self.peek() {
            let op = self.peek().into();
            self.advance();    
            let right = self.equality()?;
            let span = left.span.extend(right.span);
            left = Spanned::new(Expr::Binary { 
                op, 
                left : Box::new(left), 
                right: Box::new(right) 
            }, span)
        }
        Ok(left)
    }

    fn logic_or(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.logic_and()?;
        while let KOR = self.peek() {
            let op = self.peek().into();
            self.advance();    
            let right = self.logic_and()?;
            let span = left.span.extend(right.span);
            left = Spanned::new(Expr::Binary { 
                op, 
                left : Box::new(left), 
                right: Box::new(right) 
            }, span)
        }
        Ok(left)
    }

    fn assignment(&mut self) -> Res<Spanned<Expr>> {
        let mut left = self.logic_or()?;
        if let EQUAL      | PLUSEQUAL | 
               MINUSEQUAL | STAREQUAL | 
               SLASHEQUAL = self.peek() {
            let op = self.peek().into();
            self.advance();
            let right = self.assignment()?;
            let span = left.span.extend(right.span);
            left = Spanned::new(Expr::Assignment { 
                op,
                assignee: Box::new(left), 
                expr    : Box::new(right) 
            }, span)
        }
        Ok(left)
    }

    fn expr(&mut self) -> Res<Spanned<Expr>> {
        self.assignment()
    }

    fn let_statement(&mut self) -> Res<Spanned<Stmt>> {
        // span of 'let'
        let let_span = self.span();
        // consume let and advance
        self.advance();
        let var = self.consume_symbol()?;
        self.consume(&EQUAL)?;
        let expr = self.expr()?;
        let span = let_span.extend(expr.span);
        Ok(Spanned::new(Stmt::Let { var, expr }, span))
    }

    fn fun_statement(&mut self) -> Res<Spanned<Stmt>> {
        self.in_fun += 1;
        // span of 'let'
        let fun_span = self.span();
        // consume let and advance
        self.advance();
        
        let closure = if self.peek() == LPAREN {
            Some(self.args()?)
        } else {
            None
        };
        
        let name = self.consume_symbol()?;
        let args = self.args()?;
        let mut body = vec![];
        while self.peek() != KEND {
            body.push(self.statement()?)
        }
        let end_span = self.span();
        self.consume(&KEND)?;
        self.in_fun -= 1;
        
        let fun = stmt::Function { name, args, body };
        Ok(Spanned::new(match closure {
            Some(closure) => Stmt::Closure { fun, closure },
            None => Stmt::Function(fun),
        }, fun_span.extend(end_span)))
    }

    fn return_statement(&mut self) -> Res<Spanned<Stmt>> {
        // span of 'return'
        let return_span = self.span();
        if self.in_fun == 0 {
            return simple_error("Return statement outside of a function definition.", return_span)
        }
        // consume return and advance
        self.advance();
        let expr = self.expr()?;
        let expr_span = expr.span;
        Ok(Spanned::new(Stmt::Return(expr), return_span.extend(expr_span)))
    }

    fn branch(&mut self) -> Res<(Spanned<Expr>, Vec<Spanned<Stmt>>)> {
        if let KIF | KEF = self.peek() {
            self.advance();
        }
        let cond = self.expr()?;
        self.consume(&KTHEN)?;
        let mut body = vec![];
        while ![KEF, KELSE, KEND].contains(&self.peek()) {
            body.push(self.statement()?);
        }
        Ok((cond, body))
    }

    fn if_statement(&mut self) -> Res<Spanned<Stmt>> {
        let if_span = self.span();
        let mut branches = vec![];
        while ![KELSE, KEND].contains(&self.peek()) {
            branches.push(self.branch()?);    
        }
        let (elss, end_span) = match self.peek() {
            KELSE => {
                self.advance();
                let mut body = vec![];
                while self.peek() != KEND {
                    body.push(self.statement()?);
                }
                (Some(body), self.span())
            }, 
            KEND  => (None, self.span()),
            _ => unreachable!()
        };
        // consume `end`
        self.advance();
        Ok(Spanned::new(Stmt::If { branches, elss }, if_span.extend(end_span)))
    }

    fn while_statement(&mut self) -> Res<Spanned<Stmt>> {
        self.in_loop += 1;
        let while_span = self.span();
        self.advance();
        let cond = self.expr()?;
        self.consume(&KTHEN)?;
        let mut body = vec![];
        while self.peek() != KEND {
            body.push(self.statement()?);
        }
        let end_span = self.span();
        self.advance();
        self.in_loop -= 1;
        Ok(Spanned::new(Stmt::While { cond, body }, while_span.extend(end_span)))
    }

    fn continue_statement(&mut self) -> Res<Spanned<Stmt>> {
        let cnt_span = self.span();
        if self.in_loop == 0 {
            return simple_error("Continue statement outside of a loop.", cnt_span)
        }
        self.advance();
        Ok(Spanned::new(Stmt::Continue, cnt_span))
    }
    
    fn break_statement(&mut self) -> Res<Spanned<Stmt>> {
        let break_span = self.span();
        if self.in_loop == 0 {
            return simple_error("Break statement outside of a loop.", break_span)
        }
        self.advance();
        Ok(Spanned::new(Stmt::Break, break_span))
    }

    fn for_statement(&mut self) -> Res<Spanned<Stmt>> {
        self.in_loop += 1;
        let for_span = self.span();
        self.advance();
        let var = self.consume_symbol()?;
        self.consume(&KIN)?;
        let iter = self.expr()?;
        self.consume(&KTHEN)?;
        let mut body = vec![];
        while self.peek() != KEND {
            body.push(self.statement()?);
        }
        let end_span = self.span();
        self.advance();
        self.in_loop -= 1;
        Ok(Spanned::new(Stmt::For { var, iter, body }, for_span.extend(end_span)))
    }

    fn def_statement(&mut self) -> Res<Spanned<Stmt>> {
        let def_span = self.span();
        self.advance();
        let name = self.consume_symbol()?;
        let mut members = vec![];
        while self.peek() != KEND && self.peek() != KIMPL {
            members.push(self.consume_symbol()?);
        }
        let methods = if self.optional(&KIMPL) {
            let mut methods = vec![];
            while self.peek() != KEND {
                methods.push(self.def_method()?);
            }   
            methods
        } else {
            vec![]
        };

        let end_span = self.span(); 
        self.consume(&KEND)?;
        Ok(Spanned::new(Stmt::Def { name, members, methods }, def_span.extend(end_span)))
    }

    fn def_method(&mut self) -> Res<Spanned<stmt::Function>> {
        self.in_fun += 1;
        let fun_span = self.span();
        self.advance();
        
        let name = self.consume_symbol()?;
        let args = self.args()?;
        let mut body = vec![];
        while self.peek() != KEND {
            body.push(self.statement()?)
        }
        let end_span = self.span();
        self.consume(&KEND)?;
        self.in_fun -= 1;
        
        Ok(Spanned::new(stmt::Function { name, args, body }, fun_span.extend(end_span)))
    }

    fn impl_statement(&mut self) -> Res<Spanned<Stmt>> {
        let impl_span = self.span();
        self.advance();
        
        if self.peek() == KFOR {
            self.advance();
            let ty = self.consume_symbol()?;
            
            let mut mets = vec![];
            while self.peek() != KEND {
                let met = self.def_method()?;
                mets.push(met);
            }
            let end_span = self.span();
            self.consume(&KEND)?;
            
            return Ok(Spanned::new(Stmt::ImplFor { ty, mets }, impl_span.extend(end_span)))
        }
        
        self.in_fun += 1;
        let name = self.consume_symbol()?;
        let args = self.args()?;
        let mut body = vec![];
        while self.peek() != KEND {
            body.push(self.statement()?)
        }
        let end_span = self.span();
        self.consume(&KEND)?;
        self.in_fun -= 1;
        
        Ok(Spanned::new(Stmt::Impl(stmt::Function { name, args, body }), impl_span.extend(end_span)))
    }

    fn import_statement(&mut self) -> Res<Spanned<Stmt>> {
        let import_span = self.span();
        self.advance();

        let mut strings = vec![self.consume_symbol()?];
        while let DOT = self.peek() {
            self.advance();
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
                let span = expr.span;
                Spanned::new(Stmt::Expr(expr), span)
            }
        };
        let _ = self.optional(&SEMICOLON);
        Ok(stmt)
    }
    
    fn parse(&mut self) -> Option<Res<Spanned<Stmt>>> {
        if self.peek() == END {
            return None
        }
        Some(self.statement())
    }
    
}