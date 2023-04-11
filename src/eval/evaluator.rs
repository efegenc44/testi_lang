use std::collections::{HashMap, HashSet};

use crate::{
    error::error::{ Error, Res, simple_error, handle },
    ast::{
        stmt::{Stmt::{ self, * }, Function},
        expr::{
            Expr::{ self, * },
            BinaryOp,
            UnaryOp, AssignOp
        },
    },
    span::Spanned
};

use super::{
    value::Value::{ self, * },
    std::{
        get_global, integer_type, 
        bool_type, string_type, function_type
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

type Context = HashMap<String, Value>;
type Types = HashMap<usize, Type>;
type Impls = HashMap<String, Vec<Function>>;
pub struct Engine {
    ctx: Vec<Context>,
    
    types: Types,
    type_id: usize,
    free_type_ids: Vec<usize>,

    impls: Impls,

    types_to_collect: Vec<HashSet<usize>>,
    impls_to_collect: Vec<HashSet<String>>
}

impl Engine {
    pub fn new() -> Self {
        let mut engine = Self { 
            ctx: vec![get_global()], 
            types: Types::new(),
            type_id: 12,
            free_type_ids: vec![],
            impls: Impls::new(),
            types_to_collect: vec![HashSet::new()],
            impls_to_collect: vec![HashSet::new()],
        };
        engine.init_builtin_types();
        engine
    }

    fn init_builtin_types(&mut self) {
        self.types.insert(FUNCTION_TYPE_ID, function_type());
        self.types.insert(INTEGER_TYPE_ID, integer_type());
        self.types.insert(STRING_TYPE_ID, string_type());
        self.types.insert(BOOL_TYPE_ID, bool_type());
    }

    pub fn enter_scope(&mut self) {
        self.ctx.push(Context::new());
        self.types_to_collect.push(HashSet::new());
        self.impls_to_collect.push(HashSet::new());
    }

    pub fn exit_scope(&mut self) {
        self.ctx.pop();
        
        for id in self.types_to_collect.pop().unwrap() {
            self.types.remove(&id);
            self.free_type_ids.push(id);
        }
        
        for impl_name in self.impls_to_collect.pop().unwrap() {
            let list = self.impls.get_mut(&impl_name).unwrap();
            if list.len() <= 1 {
                self.impls.remove(&impl_name);
            } else {
                list.pop();
            }
        }
    }

    pub fn get_id(&mut self) -> usize {
        if let Some(id) = self.free_type_ids.last() {
            *id
        } else {
            let id = self.type_id;
            self.type_id += 1;
            id
        }
    }

    pub fn get_type(&self, id: usize) -> &Type {
        self.types.get(&id).unwrap()
    }

    pub fn get_type_mut(&mut self, id: usize) -> &mut Type {
        self.types.get_mut(&id).unwrap()
    }

    pub fn get_impl(&self, name: &str) -> Option<Function> {
        self.impls.get(name)
            .and_then(|list| list.last())
            .and_then(|f| Some(f.clone()))
    }

    pub fn define(&mut self, var: String, val: Value) {
        self.ctx.last_mut().unwrap().insert(var, val);
    }

    fn redefine(&mut self, var: &String, val: Value) -> Result<(), String> {
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
                FunStmt { name, args, body } =>
                    self.define(name.to_string(), FunVal {
                        args: args.to_vec(), body: body.to_vec()
                    }),
                
                DefStmt { name, mems, mets } => {
                    let id = self.get_id();
                    self.types.insert(id, Type::Def { 
                        members: mems.to_owned(), methods: mets.to_owned() 
                    });
                    self.types_to_collect.last_mut().unwrap().insert(id);
                    self.define(name.to_string(), TypeVal(id));
                }
                
                ImplStmt { name, args, body } => {
                    self.impls_to_collect.last_mut().unwrap().insert(name.clone());
                    self.impls.entry(name.to_string())
                        .or_insert(vec![])
                        .push(Function { name: name.clone(), args: args.clone(), body: body.clone() });
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
            Variable(var) => match self.redefine(&var, val.clone()) {
                Ok(())   => Ok(val),
                Err(err) => simple_error(err, assignee.span),
            },
            
            IndexExpr { from, index_expr } =>
                handle(handle(self.eval(from)?.indexable(), from.span)?
                    .replace(self.eval(index_expr)?, val), index_expr.span),

            AccessExpr { from, member } => {
                let (type_name, mut members) = handle(self.eval(from)?.as_instance(), from.span)?;                
                match members.insert(member.clone(), val) {
                    Some(_) => Ok(InstanceVal { type_id: type_name, members }),
                    None    => simple_error(format!("`{type_name}` has no member called `{member}`"), from.span),
                }
            }
            _ => simple_error("Invalid assignment target.", assignee.span)
        }
    }
    
    pub fn run(&mut self, stmt: &Spanned<Stmt>) -> Res<State> {
        match &stmt.data { 
            //   Ignore Functions and Definitions at `run` because we
            // already defined them at `collect_definitions`
            DefStmt {..} => (),
            FunStmt {..} => (),
            ImplStmt {..} => (),
            
            ImplForStmt { ty, mets } => {
                let id = handle(handle(self.resolve(ty), stmt.span)?.as_type(), stmt.span)?;
                match self.get_type_mut(id) {
                    Type::Def        { methods, .. } |
                    Type::BuiltInDef { methods, .. } => {
                        methods.extend(mets.clone());
                    }
                }
            },
            
            LetStmt { var, expr } => {
                let value = self.eval(expr)?;
                self.define(var.to_string(), value);
            },
            
            ClosureStmt { name, args, body, closure } => {
                let mut closure_map = HashMap::with_capacity(closure.len());
                for var in closure {
                    closure_map.insert(var.to_string(), handle(self.resolve(var), stmt.span)?);
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
            
            WhileStmt { cond, body } =>
                while handle(self.eval(cond)?.as_bool(), cond.span)? {
                    match self.run_block(body)? {
                        res @ State::Return(_) => return Ok(res),
                        State::Continue => continue,
                        State::Break => break,
                        State::Ok => (),
                    }
                },
            
            ForStmt { var, iter, body } => {
                for i in handle(self.eval(iter)?.iter(), iter.span)? {
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
            
            ExprStmt(e) => {
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
                let val = match op {
                    AssignOp::Normal => self.eval(e)?,
                    AssignOp::Composite(bop) => 
                        handle(self.eval(assignee)?.get_method(&bop.get_method_name(), self), expr.span)?
                            .apply(vec![self.eval(&e)?], self)
                            .map_err(|(err, inner_err)| Error::new(err, expr.span, inner_err))?
                };
                self.assign(assignee, val)
            },
            
            BinaryExpr { op, left, right } => 
                handle(self.eval(&left)?.get_method(&op.get_method_name(), self), expr.span)?
                    .apply(vec![self.eval(&right)?], self)
                    .map_err(|(err, inner_err)| Error::new(err, expr.span, inner_err)),
            
            UnaryExpr { op, operand } => 
                handle(self.eval(&operand)?.get_method(&op.get_method_name(), self), expr.span)?
                    .apply(vec![], self)
                    .map_err(|(err, inner_err)| Error::new(err, expr.span, inner_err)),
            
            ApplicationExpr { applied, exprs } => {
                let values: Result<_, _> = exprs
                    .iter()
                    .map(|expr| self.eval(expr))
                    .collect(); 

                self.eval(applied)?
                    .apply(values?, self)
                    .map_err(|(err, inner_err)| Error::new(err, expr.span, inner_err))
            },
            
            IndexExpr { from, index_expr } =>
                handle(self.eval(from)?.get_method("index", self), from.span)?
                    .apply(vec![self.eval(index_expr)?], self)
                    .map_err(|(err, inner_err)| Error::new(err, expr.span, inner_err)),
           
            AccessExpr { from, member } => {
                let value = self.eval(from)?;
                if let InstanceVal { members, .. } = &value {
                    if let Some(val) = members.get(member) {
                        return Ok(val.clone())
                    }
                }
                match value.get_method(&member, self) {
                    Ok(met) => Ok(met),
                    Err(_)  => simple_error(format!("`{ty}` has no member, nor method called `{member}`", ty = value.ty()), from.span),
                }
            },
            
            FunctionExpr { args, body, closure } => Ok(
                if let Some(closure) = closure {
                    let mut closure_map = HashMap::with_capacity(closure.len());
                    for var in closure {
                        closure_map.insert(var.to_string(), handle(self.resolve(var), expr.span)?);
                    }
                    ClosureVal { args: args.to_owned(), body: body.to_owned(), closure: closure_map }
                } else {
                    FunVal { args: args.to_owned(), body: body.to_owned() }
                }
            ),
            
            TypeTestExpr { expr: e, ty } => 
                Ok(BoolVal(self.eval(e)?.ty() == handle(self.eval(ty)?.as_type(), ty.span)?)),
            
            ListExpr(exprs) => {
                let list: Result<_, _> = exprs
                    .iter()
                    .map(|e| self.eval(e))
                    .collect();
                Ok(ListVal(list?))
            },
            
            MapExpr(pairs) => {
                let mut map = HashMap::with_capacity(pairs.len());
                for (key, value) in pairs {
                    map.insert(handle(self.eval(key)?.try_into(), key.span)?, self.eval(value)?);
                }
                Ok(MapVal(map))
            },
            
            NaturalExpr(nat) => Ok(IntegerVal(*nat as i32)),
            FloatExpr(float) => Ok(FloatVal(*float)),
            StringExpr(s) => Ok(StringVal(s.to_string())),
            CharExpr(ch) => Ok(CharVal(*ch)),
            BoolExpr(b) => Ok(BoolVal(*b)),
            NothingExpr => Ok(NothingVal),
            Variable(var) => Ok(handle(self.resolve(var), expr.span)?),
        }
    }
}