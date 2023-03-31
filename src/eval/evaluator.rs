use std::collections::HashMap;

use crate::{
    error::error::{ Error, Res, simple_error },
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
    r#type::Type::*,
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
            BinaryOp::Eq => Ok(BoolVal(left == right)),
            BinaryOp::Ne => Ok(BoolVal(left != right)),
            BinaryOp::Lt => left.less(right).map(|r| {
                BoolVal(r)
            }),
            BinaryOp::Gt => left.greater(right).map(|r| {
                BoolVal(r)
            }),
            BinaryOp::Le => left.less_or_equal(right).map(|r| {
                BoolVal(r)
            }),
            BinaryOp::Ge => left.greater_or_equal(right).map(|r| {
                BoolVal(r)
            }),
            BinaryOp::And => left & right,
            BinaryOp::Or  => left | right,
            BinaryOp::Is => {
                let TypeVal(ty) = right else {
                    return Err(format!("Expected `Type` not {ty} at right side.", ty = right.ty()))
                };
                Ok(BoolVal(&left.ty() == ty))                
            },
            BinaryOp::Xrn |
            BinaryOp::Irn => {
                let IntegerVal(bot) = left else {
                    return Err(format!("Range bounds expected to be `Integer` not `{ty}`.", ty = left.ty()))
                };
                let IntegerVal(up) = right else {
                    return Err(format!("Range bounds expected to be `Integer` not `{ty}`.", ty = right.ty()))
                };
                
                Ok(if let BinaryOp::Irn = self {
                    RangeVal(*bot, *up+1)   
                } else {
                    RangeVal(*bot, *up)   
                })
            },
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

    fn resolve(&self, var: &String) -> Result<Value, String> {
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
                self.eval(from)?.indexable().map_err(|err| {
                    Error::new(err, from.span, None)
                })?.replace(self.eval(index_expr)?, val).map_err(|err| {
                   Error::new(err, index_expr.span, None)
                })            
            },
            AccessExpr { from, member } => {
                let from_val = self.eval(from)?;
                let InstanceVal { type_name, mut members } = from_val else {
                    return simple_error(format!("`{ty}` has no member.", ty = from_val.ty()), from.span)
                };
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
            DefStmt { name, mems } => {
                let ty = CustomTy { 
                    name: name.to_owned(),
                    mems: mems.to_owned()
                };
                self.define(name.to_owned(), TypeVal(ty));
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
                    let cond_val = self.eval(cond)?;
                    let BoolVal(b) = cond_val else {
                        return simple_error(format!("Expected `Bool` for if condition, not `{ty}`.", ty = cond_val.ty()), cond.span)
                    };
                    if b {
                        return self.run_block(branch)
                    }
                }
                if let Some(branch) = elss {
                    return self.run_block(branch)
                } 
            },
            WhileStmt { cond, body } => {
                loop {
                    let cond_val = self.eval(cond)?;
                    let BoolVal(b) = cond_val else {
                        return simple_error(format!("Expected `Bool` for while condition, not `{ty}`.", ty = cond_val.ty()), cond.span)
                    };
                    if !b {
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
                let iter = self.eval(iter)?.iter().map_err(|err| {
                    Error::new(err, iter.span, None)
                })?;
                for i in iter {
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
                let val = match op {
                    AssignOp::Normal => Ok(val),
                    AssignOp::Add    => BinaryOp::Add.eval(&aval, &val),
                    AssignOp::Sub    => BinaryOp::Sub.eval(&aval, &val),
                    AssignOp::Mul    => BinaryOp::Mul.eval(&aval, &val),
                    AssignOp::Div    => BinaryOp::Div.eval(&aval, &val),
                }.map_err(|err| {
                    Error::new(err, expr.span, None)
                })?;
                self.assign(assignee, val.clone())
            },
            BinaryExpr { op, left, right } => op.eval(&self.eval(left)?,
                                                      &self.eval(right)?).map_err(|err| {
                    Error::new(err, expr.span, None)
            }),
            UnaryExpr { op, operand } => op.eval(&self.eval(operand)?).map_err(|err| {
                Error::new(err, expr.span, None)
            }),
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
                self.eval(from)?.indexable().map_err(|err| {
                    Error::new(err, from.span, None)
                })?.index(self.eval(index_expr)?).map_err(|err| {
                    Error::new(err, index_expr.span, None)
                })
            },
            AccessExpr { from, member } => {
                let from_val = self.eval(from)?;
                let InstanceVal { type_name, members } = from_val else {
                    return simple_error(format!("`{ty}` has no member.", ty = from_val.ty()), from.span)
                };
                match members.get(member) {
                    Some(val) => Ok(val.clone()),
                    None      => simple_error(format!("`{type_name}` has no member called `{member}`"), from.span),
                }
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
                    let key = TryInto::try_into(self.eval(key)?).map_err(|err| {
                        Error::new(err, key.span, None)
                    })?;
                    map.insert(key, self.eval(value)?);
                } 
                Ok(MapVal(map))
            },
            NaturalExpr(nat) => Ok(IntegerVal(*nat as i32)),
            FloatExpr(float) => Ok(FloatVal(*float)),
            StringExpr(s)    => Ok(StringVal(s.to_string())),
            BoolExpr(b)      => Ok(BoolVal(*b)),
            NothingExpr      => Ok(NothingVal),
            Variable(var)    => Ok(self.resolve(var).map_err(|err| {
                    Error::new(err, expr.span, None)
            })?),
        }
    }
}