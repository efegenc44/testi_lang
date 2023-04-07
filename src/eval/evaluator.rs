use std::collections::HashMap;

use crate::{
    error::error::{ Error, Res, simple_error, handle },
    ast::{
        stmt::Stmt::{ self, * },
        expr::{
            Expr::{ self, * },
            BinaryOp,
            UnaryOp, AssignOp
        },
    },
    span::Spanned
};

use super::{
    value::{
        Value::{ self, * },
        ExprResult
    },
    std::get_global,
};

impl BinaryOp {
    fn eval(&self, left: &Value, right: &Value) -> ExprResult {
        match self {
            BinaryOp::Add => left + right,
            BinaryOp::Sub => left - right,
            BinaryOp::Mul => left * right,
            BinaryOp::Div => left / right,
            BinaryOp::Mod => left % right,
            BinaryOp::Eq  => Ok(BoolVal(left == right)),
            BinaryOp::Ne  => Ok(BoolVal(left != right)),
            BinaryOp::Lt  => left.less(right).map(BoolVal),
            BinaryOp::Gt  => left.greater(right).map(BoolVal),
            BinaryOp::Le  => left.less_or_equal(right).map(BoolVal),
            BinaryOp::Ge  => left.greater_or_equal(right).map(BoolVal),
            BinaryOp::And => left & right,
            BinaryOp::Or  => left | right,
            BinaryOp::Is  => Ok(BoolVal(left.ty() == right.as_type()?)),                
            BinaryOp::Xrn => Ok(RangeVal(left.as_integer()?, right.as_integer()?)),
            BinaryOp::Irn => Ok(RangeVal(left.as_integer()?, right.as_integer()?+1))   
        }
    }
}

impl UnaryOp {
    fn eval(&self, operand: &Value) -> ExprResult {
        match self {
            UnaryOp::Neg => -operand,
            UnaryOp::Not => !operand,
        }
    }
}

pub enum State {
    Return(Value),
    Continue,
    Break,
    Ok
}

type Context = HashMap<String, Value>;
pub struct Engine {
    ctx: Vec<Context>,
}

impl Engine {
    pub fn new() -> Self {
        Self { ctx: vec![get_global()] }
    }

    pub fn enter_scope(&mut self) {
        self.ctx.push(Context::new());
    }

    pub fn exit_scope(&mut self) {
        self.ctx.pop().unwrap();
    }

    pub fn define(&mut self, var: String, val: Value) {
        self.ctx.last_mut().unwrap().insert(var, val);
    }

    fn redefine(&mut self, var: &String, val: Value) -> Result<(), String> {
        let len = self.ctx.len();
        for index in 0..len {
            let scope = &mut self.ctx[(len-1) - index];
            match scope.get_mut(var) {
                Some(v) => return Ok(*v = val.clone()),
                None    => continue,
            }
        }
        Err(format!("Undefined symbol `{var}`."))
    }

    pub fn resolve(&self, var: &String) -> Result<Value, String> {
        let len = self.ctx.len();
        for index in 0..len {
            let scope = &self.ctx[(len-1) - index];
            match scope.get(var) {
                Some(val) => return Ok(val.clone()),
                None      => continue,
            }
        }
        Err(format!("Undefined symbol `{var}`."))
    }
    
    pub fn collect_definition(&mut self, stmts: &Vec<Spanned<Stmt>>) -> Res<()> {
        for stmt in stmts {
            if let FunStmt { name, args, body } = &stmt.data {
                self.define(name.to_string(), FunVal {
                    args: args.to_vec(), body: body.to_vec()
                });
            }
        }
        Ok(())
    }

    fn run_block(&mut self, stmts: &Vec<Spanned<Stmt>>) -> Res<State> {
        self.enter_scope();
        self.collect_definition(stmts)?;
        for stmt in stmts {
            let res = self.run(stmt)?;
            let State::Ok = res else {
                self.exit_scope();
                return Ok(res)
            };
        }
        self.exit_scope();
        return Ok(State::Ok)
    }

    fn assign(&mut self, assignee: &Spanned<Expr>, val: Value) -> Res<Value> {
        match &assignee.data {
            Variable(var) => match self.redefine(&var, val.clone()) {
                Ok(())   => Ok(val.clone()),
                Err(err) => simple_error(err, assignee.span),
            },
            IndexExpr { from, index_expr } => {
                handle(handle(self.eval(from)?.indexable(), from.span)?
                    .replace(self.eval(index_expr)?, val), index_expr.span)
            },
            AccessExpr { from, member } => {
                let (type_name, mut members) = handle(self.eval(from)?.as_instance(), from.span)?;
                match members.insert(member.clone(), val) {
                    Some(_) => Ok(InstanceVal { type_name, members }),
                    None    => simple_error(format!("`{type_name}` has no member called `{member}`"), from.span),
                }
            }
            _ => simple_error("Invalid assignment target.", assignee.span)
        }
    }
    
    pub fn run(&mut self, stmt: &Spanned<Stmt>) -> Res<State> {
        match &stmt.data { 
            DefStmt { name, mems, mets } => {
                self.define(name.to_owned(), DefVal { 
                    name: name.to_owned(), 
                    members: mems.to_owned(), 
                    methods: mets.to_owned(), 
                });
            },
            LetStmt { var, expr } => {
                let value = self.eval(expr)?;
                self.define(var.to_string(), value);
            },
            //   Ignore Function Definitions at `run` because we
            // already defined them at `collect_definitions`
            FunStmt {..}  => (),
            ClosureStmt { name, args, body, closure } => {
                let mut closure_map = HashMap::new();
                for var in closure {
                    closure_map.insert(var.to_string(), self.resolve(var).unwrap());
                }
                
                self.define(name.to_string(), ClosureVal {
                    args: args.to_vec(), body: body.to_vec(), closure: closure_map
                });
            },
            IfStmt { branches, elss } => {
                for (cond, branch) in branches {
                    if handle(self.eval(cond)?.as_bool(), cond.span)? {
                        return self.run_block(branch)
                    }
                }
                if let Some(branch) = elss {
                    return self.run_block(branch)
                } 
            },
            WhileStmt { cond, body } => {
                loop {
                    if handle(self.eval(cond)?.as_bool(), cond.span)? {
                        break;
                    }
                    let res = self.run_block(body)?;
                    match res {
                        State::Return(_) => return Ok(res),
                        State::Continue  => continue,
                        State::Break     => break,
                        State::Ok        => (),
                    }
                }
            },
            ForStmt { var, iter, body } => {
                for i in handle(self.eval(iter)?.iter(), iter.span)? {
                    self.enter_scope();
                    self.collect_definition(body)?;
                    self.define(var.to_string(), i);
                    for stmt in body {
                        let res = self.run(stmt)?;
                        match res {
                            State::Return(_) => {
                                self.exit_scope();
                                return Ok(res)
                            },
                            State::Continue  => continue,
                            State::Break     => break,
                            State::Ok        => (),
                        }
                    }
                    self.exit_scope();
                }
            },
            ExprStmt(e)   => {
                self.eval(e)?;
            }
            ReturnStmt(e) => return Ok(State::Return(self.eval(e)?)),
            ContinueStmt  => return Ok(State::Continue),
            BreakStmt     => return Ok(State::Break),
        };
        Ok(State::Ok)
    }
    
    pub fn eval(&mut self, expr: &Spanned<Expr>) -> Res<Value> {
        match &expr.data {
            AssignmentExpr { op, assignee, expr: e } => {
                let val = self.eval(e)?;
                let aval = self.eval(assignee)?;
                let val = handle(match op {
                    AssignOp::Normal => Ok(val),
                    AssignOp::Add    => BinaryOp::Add.eval(&aval, &val),
                    AssignOp::Sub    => BinaryOp::Sub.eval(&aval, &val),
                    AssignOp::Mul    => BinaryOp::Mul.eval(&aval, &val),
                    AssignOp::Div    => BinaryOp::Div.eval(&aval, &val),
                }, expr.span)?;
                self.assign(assignee, val.clone())
            },
            BinaryExpr { op, left, right } => handle(op.eval(&self.eval(left)?, &self.eval(right)?), expr.span),
            UnaryExpr  { op, operand }     => handle(op.eval(&self.eval(operand)?), expr.span),
            ApplicationExpr { applied, exprs } => {
                let mut values = vec![];
                for expr in exprs {
                    values.push(self.eval(expr)?)
                }
                self.eval(applied)?.apply(values, self).map_err(|(err, inner_err)| {
                    Error::new(err, expr.span, inner_err) 
                })
            },
            IndexExpr { from, index_expr } => {
                handle(handle(self.eval(from)?.indexable(), from.span)?
                    .index(self.eval(index_expr)?), index_expr.span)
            },
            AccessExpr { from, member } => {
                let value = self.eval(from)?;
                match value {
                    InstanceVal {..} => {
                        let (_, members) = handle(value.as_instance(), from.span)?;
                        match members.get(member) {
                            Some(val) => return Ok(val.clone()),
                            None      => (),
                        }
                    }
                    _ => ()
                }
                let ty = value.ty();
                if let DefVal { methods, .. } = self.resolve(&ty.to_string()).unwrap() {
                    return match methods.get(member) {
                        Some(met) => Ok(MethodVal {
                            value: Box::new(value), 
                            args: met.args.clone(), 
                            body: met.body.clone() 
                        }),
                        None => simple_error(format!("`{ty}` has no member, nor method called `{member}`"), from.span),
                    }
                };
                if let BuiltInDefVal { methods, .. } = self.resolve(&ty.to_string()).unwrap() {
                    return match methods.get(member) {
                        Some(met) => Ok(BuiltInMethodVal {
                            value: Box::new(value),
                            arity: met.arity,
                            fun: met.fun,
                        }),
                        None => simple_error(format!("`{ty}` has no member, nor method called `{member}`"), from.span),
                    }
                };
                simple_error(format!("`{ty}` has no member, nor method called `{member}`"), from.span)
            },
            FunctionExpr { args, body, closure } => Ok(if let Some(closure) = closure {
                let mut closure_map = HashMap::new();
                for var in closure {
                    closure_map.insert(var.to_string(), self.resolve(var).unwrap());
                }

                ClosureVal { args: args.to_owned(), body: body.to_owned(), closure: closure_map }
            } else {
                FunVal { args: args.to_owned(), body: body.to_owned() }
            }),
            ListExpr(exprs)  => {
                let mut list = vec![]; 
                for expr in exprs {
                    list.push(self.eval(expr)?);
                }
                Ok(ListVal(list))
            },
            MapExpr(pairs)   => {
                let mut map = HashMap::new();
                for (key, value) in pairs {
                    map.insert(handle(self.eval(key)?.try_into(), key.span)?, self.eval(value)?);
                } 
                Ok(MapVal(map))
            },
            NaturalExpr(nat) => Ok(IntegerVal(*nat as i32)),
            FloatExpr(float) => Ok(FloatVal(*float)),
            StringExpr(s)    => Ok(StringVal(s.to_string())),
            CharExpr(ch)     => Ok(CharVal(*ch)),
            BoolExpr(b)      => Ok(BoolVal(*b)),
            NothingExpr      => Ok(NothingVal),
            Variable(var)    => Ok(handle(self.resolve(var), expr.span)?),
        }
    }
}