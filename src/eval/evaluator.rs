use std::collections::{ HashMap, HashSet };

use crate::{
    error::error::{ Error, Res, simple_error, handle },
    ast::{
        stmt::{Stmt, self},
        expr::{
            Expr,
            BinaryOp,
            UnaryOp, AssignOp
        }, parser::Parser,
    },
    span::Spanned, lex::lexer::Lexer
};

use super::{
    value::{ Value, self },
    std::{
        get_global, integer_type, 
        bool_type, string_type, function_type, range_type, list_type, nothing_type
    }, 
    r#type::*,
};

impl BinaryOp {
    fn get_method_name(&self) -> String {
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

impl UnaryOp {
    fn get_method_name(&self) -> String {
        match self {
            UnaryOp::Neg => String::from("neg"),
            UnaryOp::Not => String::from("not"),
        }
    }
}

pub enum State {
    Return(Value),
    Continue,
    Break,
    Ok
}

pub struct Table<T> {
    pub values: HashMap<usize, T>,
    pub current_id: usize,
    pub free_ids: Vec<usize>,
    pub marked: HashSet<usize>
}

impl<T> Table<T> {
    fn new() -> Self {
        Self { values: HashMap::new(), current_id: 0, free_ids: vec![], marked: HashSet::new() }
    }

    pub fn get_id(&mut self) -> usize {
        self.free_ids.last()
            .and_then(|id| Some(*id))
            .unwrap_or_else(|| {
                let id = self.current_id;
                self.current_id += 1;
                id
            })
    }

    pub fn get(&self, id: &usize) -> &T {
        self.values.get(id).unwrap()
    }

    pub fn get_mut(&mut self, id: &usize) -> &mut T {
        self.values.get_mut(id).unwrap()
    }

    pub fn make(&mut self, value: T) -> usize {
        let id = self.get_id();
        self.values.insert(id, value);
        id
    }

    fn sweep(&mut self) {
        let ptr_to_table = self as *mut Table<T>;  

        for id in self.values.keys() {
            if self.marked.contains(id) {
                self.marked.remove(id);
            } else {
                // Because we're removing the values we currently look,
                // it's safe to continue to iterate, we have no chance to
                // encounter them again (at least i think that is what happens)
                unsafe /* 😨 */ {
                    (*ptr_to_table).values.remove(id);
                }
            }
        }
    }

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
        engine.types.current_id = 12;
        engine
    }

    fn init_builtin_types(&mut self) {
        self.types.values.insert(FUNCTION_TYPE_ID, function_type());
        self.types.values.insert(INTEGER_TYPE_ID, integer_type());
        self.types.values.insert(STRING_TYPE_ID, string_type());
        self.types.values.insert(RANGE_TYPE_ID, range_type());
        self.types.values.insert(BOOL_TYPE_ID, bool_type());
        self.types.values.insert(LIST_TYPE_ID, list_type());
        self.types.values.insert(NOTHING_TYPE_ID, nothing_type());
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
                        id,
                        value: None,
                        closure: None,
                    })
                }

                Stmt::Def { name, members, methods } => {
                    let id = self.types.make(Type::Def { 
                        members: members.to_owned(), methods: methods.iter().map(|method| {
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
                Ok(())   => Ok(val),
                Err(err) => simple_error(err, assignee.span.clone()),
            },
            
            Expr::Index { from: _, index_expr: _ } =>
                unimplemented!(),

            Expr::Access { from: _, member: _ } => 
                unimplemented!(),
            
            _ => simple_error("Invalid assignment target.", assignee.span.clone())
        }
    }
    
    pub fn run(&mut self, stmt: &Spanned<Stmt>) -> Res<State> {
        match &stmt.data { 
            //   Ignore Functions and Definitions at `run` because we
            // already defined them at `collect_definitions`
            Stmt::Def {..} => (),
            Stmt::Function {..} => (),
            Stmt::Impl {..} => (),
            
            Stmt::Import(strings) => {
                let module_name = strings.last().unwrap().clone();
                let mut path = strings.join("/");
                path.push_str(".testi");
                
                let file = std::fs::read_to_string(&path).expect("Error reading a file.");
                
                let tokens = match Lexer::new(path, &file).collect() {
                    Ok(tokens) => tokens,
                    Err(err)   => return Err(Error::new("Error Importing file", stmt.span.clone(), Some(err))),
                };

                let stmts: Vec<_> = match Parser::new(tokens).collect() {
                    Ok(stmts) => stmts,
                    Err(err)  => return Err(Error::new("Error Importing file", stmt.span.clone(), Some(err))),
                };

                self.enter_scope();
                match self.collect_definitions(&stmts) {
                    Ok(_)    => (),
                    Err(err) => return Err(Error::new("Error Importing file", stmt.span.clone(), Some(err))),
                }
                for stmtt in &stmts {
                    match self.run(&stmtt) {
                        Ok(_)    => (),
                        Err(err) => return Err(Error::new("Error Importing file", stmt.span.clone(), Some(err))),
                    }
                };

                let id = self.modules.make(self.ctx.pop().unwrap());
                self.define(module_name, Value::Module(id))
            }

            Stmt::ImplFor { ty, mets } => {
                let id = handle(handle(self.resolve(ty), stmt.span.clone())?.as_type(), stmt.span.clone())?;
                match self.types.get_mut(&id) {
                    Type::Def        { methods, .. } |
                    Type::BuiltInDef { methods, .. } => {
                        let mets: HashMap<_, _> = mets.iter().map(|method| {
                            (method.data.name.clone(), self.functions.make(method.into()))
                        }).collect();
                        methods.extend(mets);
                    }
                }
            },
            
            Stmt::Let { var, expr } => {
                let value = self.eval(expr)?;
                self.define(var.to_string(), value);
            },
            
            Stmt::Closure { fun: stmt::Function { name, args, body }, closure } => {
                let mut closure_map = HashMap::with_capacity(closure.len());
                for var in closure {
                    closure_map.insert(var.to_string(), handle(self.resolve(var), stmt.span.clone())?);
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
                    if handle(self.eval(cond)?.as_bool(), cond.span.clone())? {
                        return self.run_block(branch)
                    }
                }
                if let Some(branch) = elss {
                    return self.run_block(branch)
                } 
            },
            
            Stmt::While { cond, body } =>
                while handle(self.eval(cond)?.as_bool(), cond.span.clone())? {
                    match self.run_block(body)? {
                        res @ State::Return(_) => return Ok(res),
                        State::Continue => continue,
                        State::Break => break,
                        State::Ok => (),
                    }
                },
            
            Stmt::For { var, iter, body } => {
                for step in 0.. {
                    let i = handle(self.eval(&iter)?.get_method("step", self), iter.span.clone())?
                                .apply(vec![Value::Integer(step)], self)
                                .map_err(|(err, inner_err)| Error::new(err, iter.span.clone(), inner_err))?;
                    if let Value::Nothing = i {
                        break
                    }
                    self.enter_scope();
                    self.collect_definitions(body)?;
                    self.define(var.to_string(), i);
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
                        handle(self.eval(assignee)?.get_method(&bop.get_method_name(), self), expr.span.clone())?
                            .apply(vec![self.eval(&e)?], self)
                            .map_err(|(err, inner_err)| Error::new(err, expr.span.clone(), inner_err))?
                };
                self.assign(assignee, val)
            },
            
            Expr::Binary { op, left, right } => 
                handle(self.eval(&left)?.get_method(&op.get_method_name(), self), expr.span.clone())?
                    .apply(vec![self.eval(&right)?], self)
                    .map_err(|(err, inner_err)| Error::new(err, expr.span.clone(), inner_err)),
            
            Expr::Unary { op, operand } => 
                handle(self.eval(&operand)?.get_method(&op.get_method_name(), self), expr.span.clone())?
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
                handle(self.eval(from)?.get_method("index", self), from.span.clone())?
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
                        closure_map.insert(var.to_string(), handle(self.resolve(var), expr.span.clone())?);
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
                Ok(Value::Bool(self.eval(e)?.ty() == handle(self.eval(ty)?.as_type(), ty.span.clone())?)),
            
            Expr::List(exprs) => {
                let list: Result<_, _> = exprs
                    .iter()
                    .map(|e| self.eval(e))
                    .collect();
                Ok(Value::List(self.lists.make(list?)))
            },
            
            Expr::Map(pairs) => {
                let mut map = HashMap::with_capacity(pairs.len());
                for (key, value) in pairs {
                    map.insert(handle(self.eval(key)?.try_into(), key.span.clone())?, self.eval(value)?);
                }
                Ok(Value::Map(self.maps.make(map)))
            },
            
            Expr::Natural(nat) => Ok(Value::Integer(*nat as i32)),
            Expr::Float(float) => Ok(Value::Float(*float)),
            Expr::String(s) => Ok(Value::String(s.to_string())),
            Expr::Char(ch) => Ok(Value::Char(*ch)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Nothing => Ok(Value::Nothing),
            Expr::Variable(var) => Ok(handle(self.resolve(var), expr.span.clone())?),
        }
    }
}