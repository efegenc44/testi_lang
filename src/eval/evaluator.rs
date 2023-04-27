use std::collections::{ HashMap, HashSet };

use crate::{
    error::error::{ Error, Res, simple_error },
    ast::{
        stmt::{ Stmt, self },
        expr::{ Expr, AssignOp },
        parser::Parser,
    },
    span::Spanned, lex::lexer::Lexer, handle
};

use super::{
    value::{ Value, self },
    std::{
        get_global, integer_type, 
        bool_type, string_type, function_type, range_type, list_type, nothing_type
    }, 
    r#type::*, table::Table,
};

pub enum State {
    Return(Value),
    Continue,
    Break,
    Ok
}

type Context  = HashMap<String, Value>;
type Impls    = HashMap<String, Vec<usize>>;
type List     = Vec<Value>;
type Map      = HashMap<value::KeyValue, Value>;
type Instance = HashMap<String, Value>;
pub struct Engine {
    ctx: Vec<Context>,
    
    pub types    : Table<Type>,
    pub modules  : Table<Context>,
    pub lists    : Table<List>,
    pub maps     : Table<Map>,
    pub instances: Table<Instance>,
    pub functions: Table<value::Function>,

    impls: Impls,
    impls_to_collect: Vec<HashSet<String>>,
}

impl Engine {
    pub fn new() -> Self {
        let mut engine = Self { 
            ctx: vec![get_global()], 

            types    : Table::new(),
            modules  : Table::new(),
            lists    : Table::new(),
            maps     : Table::new(),
            instances: Table::new(),
            functions: Table::new(),

            impls: Impls::new(),
            impls_to_collect: vec![HashSet::new()],
        };
        engine.init_builtin_types();
        engine.types.current_id = BuiltInType::CustomStartId as usize;
        engine
    }

    fn init_builtin_types(&mut self) {
        self.types.values.insert(BuiltInType::Function as usize, function_type());
        self.types.values.insert(BuiltInType::Integer as usize, integer_type());
        self.types.values.insert(BuiltInType::String as usize, string_type());
        self.types.values.insert(BuiltInType::Range as usize, range_type());
        self.types.values.insert(BuiltInType::Bool as usize, bool_type());
        self.types.values.insert(BuiltInType::List as usize, list_type());
        self.types.values.insert(BuiltInType::Nothing as usize, nothing_type());
    }

    pub fn mark_and_sweep(&mut self) {
        // mark
        let dont_try_this_at_home = self as *mut Engine;
        for scope in &self.ctx {
            for value in scope.values() {
                unsafe /* 😨 */ {
                    value.mark(dont_try_this_at_home)
                }
            }
        }

        // sweep
        self.types    .sweep();
        self.modules  .sweep();
        self.lists    .sweep();
        self.maps     .sweep();
        self.instances.sweep();
        self.functions.sweep();
    }

    pub fn enter_scope(&mut self) {
        self.ctx.push(Context::new());
        self.impls_to_collect.push(HashSet::new());
    }

    pub fn exit_scope(&mut self) {
        self.ctx.pop();
        
        for impl_name in self.impls_to_collect.pop().unwrap() {
            let list = self.impls.get_mut(&impl_name).unwrap();
            if list.len() <= 1 {
                self.impls.remove(&impl_name);
            } else {
                list.pop();
            }
        }
    }

    pub fn get_impl(&self, name: &str) -> Option<&usize> {
        self.impls.get(name)
            .and_then(|list| list.last())
            .and_then(|id| Some(id))
    }

    pub fn define(&mut self, var: String, val: Value) {
        self.ctx.last_mut().unwrap().insert(var, val);
    }

    fn redefine(&mut self, var: &str, val: Value) -> Result<(), String> {
        for scope in self.ctx.iter_mut().rev() {
            match scope.get_mut(var) {
                Some(v) => return Ok(*v = val.clone()),
                None    => continue,
            }
        }
        Err(format!("Undefined symbol `{var}`."))
    }

    pub fn resolve(&self, var: &str) -> Result<Value, String> {
        for scope in self.ctx.iter().rev() {
            match scope.get(var) {
                Some(val) => return Ok(val.clone()),
                None      => continue,
            }
        }
        Err(format!("Undefined symbol `{var}`."))
    }

    pub fn collect_definitions(&mut self, stmts: &Vec<Spanned<Stmt>>) -> Res<()> {
        for stmt in stmts {
            match &stmt.data {
                Stmt::Function(stmt::Function { name, args, body }) => {
                    let id = self.functions.make(value::Function { 
                        args: args.clone(), body: body.clone() 
                    });
                    self.define(name.to_string(), Value::Function {
                        id, value: None, closure: None,
                    })
                }

                Stmt::Definition { name, members, methods } => {
                    let id = self.types.make(Type { 
                        members: members.to_owned(), builtin_methods: None, methods: methods.iter().map(|method| {
                            (method.data.name.clone(), self.functions.make(method.into()))
                        }).collect() 
                    });
                    self.define(name.to_string(), Value::Type(id));
                }
                
                Stmt::Impl(stmt::Function { name, args, body }) => {
                    self.impls_to_collect.last_mut().unwrap().insert(name.clone());
                    self.impls.entry(name.to_string())
                        .or_insert(vec![])
                        .push(self.functions.make(value::Function { args: args.clone(), body: body.clone() }));
                }

                Stmt::ImplFor { ty, methods: mets } => {
                    let id = handle!(handle!(self.resolve(ty), stmt.span.clone())?.as_type(), stmt.span.clone())?;
                    self.types.get_mut(&id).methods.extend(
                        mets.iter()
                            .map(|method| (method.data.name.clone(), self.functions.make(method.into())))
                            .collect::<HashMap<_, _>>()
                    );
                },

                _ => (),
            }
        }
        Ok(())
    }

    fn run_block(&mut self, stmts: &Vec<Spanned<Stmt>>) -> Res<State> {
        self.enter_scope();
        self.collect_definitions(stmts)?;
        for stmt in stmts {
            match self.run(stmt)? {
                State::Ok => (),
                res => {
                    self.exit_scope();
                    return Ok(res)
                }
            }
        }
        self.exit_scope();
        Ok(State::Ok)
    }

    fn assign(&mut self, assignee: &Spanned<Expr>, val: Value) -> Res<Value> {
        match &assignee.data {
            Expr::Variable(var) => match self.redefine(&var, val.clone()) {
                Ok(())   => Ok(Value::Nothing),
                Err(err) => simple_error(err, assignee.span.clone()),
            },
            
            Expr::Index { from, index_expr } => 
                match self.eval(from)? {
                    Value::List(id) => {
                        let index = handle!(self.eval(index_expr)?.as_integer(), from.span.clone())?;
                        match self.lists.get_mut(&id).get_mut(index as usize) {
                            Some(element) => {
                                *element = val;
                                Ok(Value::Nothing)
                            },
                            None => simple_error("Index out of bounds.", index_expr.span.clone()),
                        }
                    },
                    Value::Map(id) => {
                        let index = self.eval(index_expr)?;
                        self.maps.get_mut(&id).insert(handle!(TryInto::try_into(index), index_expr.span.clone())?, val);
                        Ok(Value::Nothing)
                    },
                    _ => return simple_error("Only can mutate lists and maps with indexing.", from.span.clone()),
                }

            Expr::Access { from, member } => {
                let (_, id) = handle!(self.eval(from)?.as_instance(), from.span.clone())?;
                match self.instances.get_mut(&id).get_mut(member) {
                    Some(member) => {
                        *member = val;
                        Ok(Value::Nothing)
                    },
                    None => simple_error("Instance has no member like that.", from.span.clone()),
                }
            },
            
            _ => simple_error("Invalid assignment target.", assignee.span.clone())
        }
    }
    
    pub fn run_module(&mut self, stmts: &Vec<Spanned<Stmt>>) -> Res<()> {
        self.collect_definitions(&stmts)?;
        for stmt in stmts {
            self.run(&stmt)?;
        };
        Ok(())
    }

    pub fn run(&mut self, stmt: &Spanned<Stmt>) -> Res<State> {
        match &stmt.data { 
            //   Ignore Functions and Definitions at `run` because we
            // already defined them at `collect_definitions`
            Stmt::Definition {..} => (),
            Stmt::Function   {..} => (),
            Stmt::Impl       {..} => (),
            Stmt::ImplFor    {..} => (),
            
            Stmt::Import(strings) => {
                let module_name = strings.last().unwrap().clone();
                let mut path = strings.join("/");
                path.push_str(".testi");
                
                let lexer = Lexer::from_file(&path)
                    .map_err(|_| Error::new("Error Importing a file.", stmt.span.clone(), None))?;
                let tokens = lexer.collect::<Res<_>>()              
                    .map_err(|err| Error::new("Error Importing a file.", stmt.span.clone(), Some(err)))?;
                let stmts = Parser::new(tokens).collect::<Res<_>>()
                    .map_err(|err| Error::new("Error Importing a file.", stmt.span.clone(), Some(err)))?;
                let _ = self.run_module(&stmts)       
                    .map_err(|err| Error::new("Error Importing a file.", stmt.span.clone(), Some(err)))?;

                let id = self.modules.make(self.ctx.pop().unwrap());
                self.define(module_name, Value::Module(id))
            }
            
            Stmt::Let { var, expr } => {
                let value = self.eval(expr)?;
                self.define(var.to_string(), value);
            },
            
            Stmt::Closure { fun: stmt::Function { name, args, body }, closure } => {
                let mut closure_map = HashMap::with_capacity(closure.len());
                for var in closure {
                    closure_map.insert(var.to_string(), handle!(self.resolve(var), stmt.span.clone())?);
                }
                let id = self.functions.make(value::Function { 
                    args: args.clone(), body: body.clone() 
                });
                self.define(name.to_string(), Value::Function { 
                    id,
                    value: None, 
                    closure: Some(closure_map) 
                });
            },
            
            Stmt::If { branches, elss } => {
                for (cond, branch) in branches {
                    if handle!(self.eval(cond)?.as_bool(), cond.span.clone())? {
                        return self.run_block(branch)
                    }
                }
                if let Some(branch) = elss {
                    return self.run_block(branch)
                } 
            },
            
            Stmt::While { cond, body } =>
                while handle!(self.eval(cond)?.as_bool(), cond.span.clone())? {
                    match self.run_block(body)? {
                        res @ State::Return(_) => return Ok(res),
                        State::Continue => continue,
                        State::Break => break,
                        State::Ok => (),
                    }
                },
            
            Stmt::For { var, iter, body } => {
                for step in 0.. {
                    let i = handle!(self.eval(&iter)?.get_method("step", self), iter.span.clone())?
                                .apply(vec![Value::Integer(step)], self)
                                .map_err(|(err, inner_err)| Error::new(err, iter.span.clone(), inner_err))?;
                    let Value::List(list) = i else {
                        return simple_error("Step should return a 2-tuple", iter.span.clone())
                    };

                    let list = self.lists.get(&list);
                    if list.len() != 2 {
                        return simple_error("Step should return a 2-tuple", iter.span.clone())
                    }

                    let mut quit = false;
                    if let Value::Bool(false) = list[1] {
                        quit = true;
                    }
                    let iter_value = list[0].clone(); 

                    self.enter_scope();
                    self.collect_definitions(body)?;
                    self.define(var.to_string(), iter_value);
                    for stmt in body {
                        match self.run(stmt)? {
                            res @ State::Return(_) => {
                                self.exit_scope();
                                return Ok(res)
                            },
                            State::Continue => continue,
                            State::Break => break,
                            State::Ok => (),
                        }
                    }
                    self.exit_scope();
                    if quit {
                        break;
                    }
                }
            },
            
            Stmt::Expr(e) => {
                self.eval(e)?;
            }
            
            Stmt::Return(e) => return Ok(State::Return(self.eval(e)?)),
            Stmt::Continue  => return Ok(State::Continue),
            Stmt::Break     => return Ok(State::Break),
        };
        Ok(State::Ok)
    }
    
    pub fn eval(&mut self, expr: &Spanned<Expr>) -> Res<Value> {
        match &expr.data {
            Expr::Assignment { op, assignee, expr: e } => {
                let val = match op {
                    AssignOp::Normal => self.eval(e)?,
                    AssignOp::Composite(bop) => 
                        handle!(self.eval(assignee)?.get_method(&bop.get_method_name(), self), expr.span.clone())?
                            .apply(vec![self.eval(&e)?], self)
                            .map_err(|(err, inner_err)| Error::new(err, expr.span.clone(), inner_err))?
                };
                self.assign(assignee, val)
            },
            
            Expr::Binary { op, left, right } => 
                handle!(self.eval(&left)?.get_method(&op.get_method_name(), self), expr.span.clone())?
                    .apply(vec![self.eval(&right)?], self)
                    .map_err(|(err, inner_err)| Error::new(err, expr.span.clone(), inner_err)),
            
            Expr::Unary { op, operand } =>
                handle!(self.eval(&operand)?.get_method(&op.get_method_name(), self), expr.span.clone())?
                    .apply(vec![], self)
                    .map_err(|(err, inner_err)| Error::new(err, expr.span.clone(), inner_err)),
            
            Expr::Application { applied, exprs } => {
                let values: Result<_, _> = exprs
                    .iter()
                    .map(|expr| self.eval(expr))
                    .collect(); 

                self.eval(applied)?
                    .apply(values?, self)
                    .map_err(|(err, inner_err)| Error::new(err, expr.span.clone(), inner_err))
            },
            
            Expr::Index { from, index_expr } =>
                handle!(self.eval(from)?.get_method("index", self), from.span.clone())?
                    .apply(vec![self.eval(index_expr)?], self)
                    .map_err(|(err, inner_err)| Error::new(err, expr.span.clone(), inner_err)),
           
            Expr::Access { from, member } => {
                let value = self.eval(from)?;
                match &value {
                    Value::Instance { id, .. } =>
                        if let Some(val) = self.instances.get(id).get(member) {
                            return Ok(val.clone())
                        }
                    
                    Value::Module(id) => {
                        let Some(value) = self.modules.get(id).get(member) else {
                            return simple_error(format!("`Module` has no member, nor method called `{member}`"), from.span.clone())
                        };
                        return Ok(value.clone())
                    }
                    _ => (),
                }
                match value.get_method(&member, self) {
                    Ok(met) => Ok(met),
                    Err(_)  => simple_error(format!("`{ty}` has no member, nor method called `{member}`", ty = value.ty()), from.span.clone()),
                }
            },
            
            Expr::Function { args, body, closure } => {
                let closure_map = if let Some(closure) = closure {
                    let mut closure_map = HashMap::with_capacity(closure.len());
                    for var in closure {
                        closure_map.insert(var.to_string(), handle!(self.resolve(var), expr.span.clone())?);
                    }
                    Some(closure_map)
                } else {
                    None
                };
                
                Ok(Value::Function { 
                    id: self.functions.make(value::Function { 
                        args: args.to_owned(), body: body.to_owned()
                    }),
                    value: None,
                    closure: closure_map,
                })
            },
            
            Expr::TypeTest { expr: e, ty } => 
                Ok(Value::Bool(self.eval(e)?.ty() == handle!(self.eval(ty)?.as_type(), ty.span.clone())?)),
            
            Expr::List(exprs) => {
                let list: Result<_, _> = exprs
                    .iter()
                    .map(|e| self.eval(e))
                    .collect();
                Ok(Value::List(self.lists.make(list?)))
            },
            
            Expr::Map(pairs) => {
                let mut map = HashMap::new();
                for (key, value) in pairs {
                    map.insert(handle!(self.eval(key)?.try_into(), key.span.clone())?, self.eval(value)?);
                }
                Ok(Value::Map(self.maps.make(map)))
            },
            
            Expr::Natural(nat) => Ok(Value::Integer(*nat as i32)),
            Expr::Float(float) => Ok(Value::Float(*float)),
            Expr::String(s) => Ok(Value::String(s.to_string())),
            Expr::Char(ch) => Ok(Value::Char(*ch)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Nothing => Ok(Value::Nothing),
            Expr::Variable(var) => Ok(handle!(self.resolve(var), expr.span.clone())?),
            Expr::Copy(expr) =>
                Ok(match self.eval(expr)? {
                    // Only copy the mutable values.
                    Value::List(id) => Value::List(self.lists.copy(&id)),
                    Value::Map(id)  => Value::Map(self.maps.copy(&id)),
                    Value::Type(id) => Value::Type(self.types.copy(&id)),
                    Value::Instance { type_id, id } 
                        => Value::Instance { type_id, id: self.instances.copy(&id)},
                    // No need to copy immutable values.
                    immutable_value => immutable_value
                })
        }
    }
}