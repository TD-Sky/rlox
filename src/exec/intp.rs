use std::mem;
use std::{collections::HashMap, rc::Rc};

use smol_str::{SmolStr, SmolStrBuilder};

use super::{
    call::{clock, LoxCallable},
    env::Env,
};
use crate::{
    exec::Value,
    parse::types::*,
    scan::{Lexeme, Token},
    span::{Span, Spanned},
    utils::RcCell,
};

thread_local! {
    static GLOBAL_ENV: RcCell<Env> = RcCell::new(Env::new(
        [(SmolStr::new("clock"), clock().into())]
    ));
}

#[derive(Debug)]
pub struct Interpreter {
    env: RcCell<Env>,
    loop_depth: usize,
    locals: HashMap<Expr, usize>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: GLOBAL_ENV.with(Clone::clone),
            loop_depth: 0,
            locals: Default::default(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), ExecError> {
        for stmt in stmts {
            self.execute(stmt)?;
        }

        Ok(())
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, ExecError> {
        match expr {
            Expr::Binary(expr) => self.binary(expr),
            Expr::Grouping(expr) => self.grouping(expr),
            Expr::Literal(expr) => Ok(self.literal(expr)),
            Expr::Unary(expr) => self.unary(expr),
            Expr::Variable(var) => self.lookup_var(&var.name, expr),
            Expr::Assign(assign) => self.assign(assign, expr),
            Expr::Conditional(expr) => self.conditional(expr),
            Expr::Call(expr) => self.call(expr),
            Expr::Lambda(expr) => Ok(LoxLambda::new(expr, &self.env).into()),
            Expr::Get(get) => self.get(get),
            Expr::Set(set) => self.set(set),
            Expr::This(this) => self.lookup_var(&this.keyword, expr),
            Expr::Super(_) => todo!(),
        }
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.insert(expr.clone(), depth);
    }
}

impl Interpreter {
    fn execute(&mut self, stmt: &Stmt) -> Result<StmtValue, ExecError> {
        match stmt {
            Stmt::Expr(expr) => self.eval(expr).map(|_| StmtValue::Finish),
            Stmt::Print(expr) => {
                let value = self.eval(expr)?;
                println!("{value}");
                Ok(StmtValue::Finish)
            }
            Stmt::Var(var) => self.declare_var(var).map(|_| StmtValue::Finish),
            Stmt::Block(block) => self.block(block),
            Stmt::If(stmt) => self.if_stmt(stmt),
            Stmt::While(stmt) => self.while_stmt(stmt),
            Stmt::For(stmt) => self.for_stmt(stmt),
            Stmt::Break(stmt) => self.break_stmt(stmt).map(|_| StmtValue::Break),
            Stmt::Fun(fun) => {
                self.declare_fun(fun);
                Ok(StmtValue::Finish)
            }
            Stmt::Return(rt) => self.return_stmt(rt).map(StmtValue::Return),
            Stmt::Class(class) => self.class(class).map(|_| StmtValue::Finish),
        }
    }

    fn declare_var(&mut self, var: &Var) -> Result<(), ExecError> {
        let value = match &var.init {
            Some(expr) => self.eval(expr)?,
            None => Value::Null,
        };
        self.env.borrow_mut().define(&var.name, value);

        Ok(())
    }

    fn declare_fun(&mut self, fun: &Function) {
        let lox_fun = LoxFunction::new(fun, &self.env, false);
        self.env.borrow_mut().define(&fun.name, lox_fun.into());
    }

    fn block(&mut self, block: &Block) -> Result<StmtValue, ExecError> {
        self.begin_scope();

        let res = || -> Result<StmtValue, ExecError> {
            for stmt in &block.stmts {
                match self.execute(stmt) {
                    Ok(StmtValue::Finish) => (),
                    Ok(StmtValue::Break) => break,
                    res => return res,
                }
            }

            Ok(StmtValue::Finish)
        }();

        self.end_scope();

        res
    }

    /// 自己准备好环境来调用
    fn fun_block(&mut self, block: &Block) -> Result<Value, ExecError> {
        for stmt in &block.stmts {
            if let StmtValue::Return(v) = self.execute(stmt)? {
                return Ok(v);
            }
        }

        Ok(Value::Null)
    }

    fn grouping(&mut self, expr: &Grouping) -> Result<Value, ExecError> {
        self.eval(&expr.expression)
    }

    fn literal(&mut self, expr: &Literal) -> Value {
        expr.clone().into()
    }

    fn unary(&mut self, expr: &Unary) -> Result<Value, ExecError> {
        let right = self.eval(&expr.right)?;

        let value = match expr.operator.token {
            Token::Bang => Value::Bool(!right.as_bool()),
            Token::Minus => Value::Number(-right.as_number().ok_or_else(|| ExecError {
                span: expr.operator.span.clone(),
                msg: "expected number as operand".into(),
            })?),
            _ => unreachable!(),
        };

        Ok(value)
    }

    fn binary(&mut self, expr: &Binary) -> Result<Value, ExecError> {
        match expr.operator.token {
            Token::Or => {
                let left = self.eval(&expr.left)?;
                if left.as_bool() {
                    return Ok(left);
                }
                let right = self.eval(&expr.right)?;
                if right.as_bool() {
                    return Ok(right);
                }
                return Ok(Value::Bool(false));
            }
            Token::And => {
                let left = self.eval(&expr.left)?;
                if !left.as_bool() {
                    return Ok(Value::Bool(false));
                }
                let right = self.eval(&expr.right)?;
                return Ok(Value::Bool(right.as_bool()));
            }
            _ => (),
        }

        let left = self.eval(&expr.left)?;
        let right = self.eval(&expr.right)?;

        let expect_num = || ExecError {
            span: expr.operator.span.clone(),
            msg: "expected number as operand".into(),
        };

        let value = match expr.operator.token {
            Token::EqualEqual => Value::Bool(left == right),
            Token::BangEqual => Value::Bool(left != right),
            Token::Greater => Value::Bool(
                left.as_number().ok_or_else(expect_num)?
                    > right.as_number().ok_or_else(expect_num)?,
            ),
            Token::GreaterEqual => Value::Bool(
                left.as_number().ok_or_else(expect_num)?
                    >= right.as_number().ok_or_else(expect_num)?,
            ),
            Token::Less => Value::Bool(
                left.as_number().ok_or_else(expect_num)?
                    < right.as_number().ok_or_else(expect_num)?,
            ),
            Token::LessEqual => Value::Bool(
                left.as_number().ok_or_else(expect_num)?
                    <= right.as_number().ok_or_else(expect_num)?,
            ),
            Token::Minus => Value::Number(
                left.as_number().ok_or_else(expect_num)?
                    - right.as_number().ok_or_else(expect_num)?,
            ),
            Token::Slash => {
                let dividend = left.as_number().ok_or_else(expect_num)?;
                let divisor = right.as_number().ok_or_else(expect_num)?;

                if divisor == 0.0 {
                    return Err(ExecError {
                        span: expr.operator.span.clone(),
                        msg: "division by zero".into(),
                    });
                }

                Value::Number(dividend / divisor)
            }
            Token::Star => Value::Number(
                left.as_number().ok_or_else(expect_num)?
                    * right.as_number().ok_or_else(expect_num)?,
            ),
            Token::Plus => match (left, right) {
                (Value::Number(lhs), rhs) => {
                    Value::Number(lhs + rhs.as_number().ok_or_else(expect_num)?)
                }
                (Value::String(lhs), rhs) => {
                    let mut sb = SmolStrBuilder::new();
                    sb.push_str(&lhs);
                    sb.push_str(rhs.as_str().ok_or_else(|| ExecError {
                        span: expr.operator.span.clone(),
                        msg: "expected string as operand".into(),
                    })?);
                    Value::String(sb.finish())
                }
                _ => {
                    return Err(ExecError {
                        span: expr.operator.span.clone(),
                        msg: "expected number/string as operands".into(),
                    })
                }
            },
            Token::Comma => right,
            _ => unreachable!(),
        };

        Ok(value)
    }

    fn conditional(&mut self, expr: &Conditional) -> Result<Value, ExecError> {
        let Conditional {
            cond,
            then,
            or_else,
        } = expr;

        let expr = if self.eval(cond)?.as_bool() {
            then
        } else {
            or_else
        };

        self.eval(expr)
    }

    fn call(&mut self, expr: &Call) -> Result<Value, ExecError> {
        let callee = self.eval(&expr.callee)?;

        let args = expr
            .arguments
            .iter()
            .map(|arg| self.eval(arg))
            .collect::<Result<Vec<_>, _>>()?;

        let Value::Callable(callee) = callee else {
            return Err(ExecError {
                span: expr.callee.span(),
                msg: "can only call functions and classes".into(),
            });
        };

        if callee.arity() != args.len() {
            return Err(ExecError {
                span: expr.span(),
                msg: format!(
                    "expected {} arguments but got {}",
                    callee.arity(),
                    args.len()
                ),
            });
        }

        Ok(callee.call(self, args))
    }

    fn assign(&mut self, expr: &Assign, shell: &Expr) -> Result<Value, ExecError> {
        let value = self.eval(&expr.value)?;

        {
            let value = value.clone();
            if let Some(&depth) = self.locals.get(shell) {
                Env::assign_at(self.env.clone(), depth, &expr.name.name, value);
            } else {
                GLOBAL_ENV.with(|env| env.borrow_mut().assign(&expr.name.name, value))?;
            }
        }

        Ok(value)
    }

    fn get(&mut self, get: &Get) -> Result<Value, ExecError> {
        if let Value::Instance(instance) = self.eval(&get.object)? {
            instance.get(&get.name)
        } else {
            Err(ExecError {
                span: get.span(),
                msg: "only instances have properties".into(),
            })
        }
    }

    fn set(&mut self, set: &Set) -> Result<Value, ExecError> {
        let Value::Instance(mut instance) = self.eval(&set.object)? else {
            return Err(ExecError {
                span: set.span(),
                msg: "only instances have fields".into(),
            });
        };

        let value = self.eval(&set.value)?;
        instance.set(&set.name, value.clone());

        Ok(value)
    }

    fn if_stmt(&mut self, stmt: &If) -> Result<StmtValue, ExecError> {
        let If {
            condition,
            then_branch,
            else_branch,
        } = stmt;

        if self.eval(condition)?.as_bool() {
            self.execute(then_branch)?;
        } else if let Some(stmt) = else_branch {
            self.execute(stmt)?;
        }

        Ok(StmtValue::Finish)
    }

    fn while_stmt(&mut self, stmt: &While) -> Result<StmtValue, ExecError> {
        let While { condition, body } = stmt;

        self.loop_depth += 1;

        let res = || -> Result<StmtValue, ExecError> {
            while self.eval(condition)?.as_bool() {
                match self.execute(body) {
                    Ok(StmtValue::Finish) => (),
                    Ok(StmtValue::Break) => break,
                    res => return res,
                }
            }

            Ok(StmtValue::Finish)
        }();

        self.loop_depth -= 1;

        res
    }

    fn for_stmt(&mut self, stmt: &For) -> Result<StmtValue, ExecError> {
        let For {
            init,
            condition,
            change,
            body,
        } = stmt;

        let mut new_env = false;
        match init.as_deref() {
            Some(Stmt::Var(var)) => {
                self.begin_scope();
                self.declare_var(var).inspect_err(|_| self.end_scope())?;
                new_env = true;
            }
            Some(Stmt::Expr(expr)) => {
                self.eval(expr)?;
            }
            _ => (),
        }

        self.loop_depth += 1;

        let res = || -> Result<StmtValue, ExecError> {
            match (condition, change) {
                (Some(condition), Some(change)) => {
                    while self.eval(condition)?.as_bool() {
                        match self.execute(body) {
                            Ok(StmtValue::Finish) => (),
                            Ok(StmtValue::Break) => break,
                            res => return res,
                        }
                        self.eval(change)?;
                    }
                }
                (Some(condition), None) => {
                    while self.eval(condition)?.as_bool() {
                        match self.execute(body) {
                            Ok(StmtValue::Finish) => (),
                            Ok(StmtValue::Break) => break,
                            res => return res,
                        }
                    }
                }
                (None, Some(change)) => loop {
                    match self.execute(body) {
                        Ok(StmtValue::Finish) => (),
                        Ok(StmtValue::Break) => break,
                        res => return res,
                    }
                    self.eval(change)?;
                },
                (None, None) => loop {
                    match self.execute(body) {
                        Ok(StmtValue::Finish) => (),
                        Ok(StmtValue::Break) => break,
                        res => return res,
                    }
                },
            }

            Ok(StmtValue::Finish)
        }();

        self.loop_depth -= 1;
        if new_env {
            self.end_scope();
        }

        res
    }

    fn break_stmt(&mut self, stmt: &Break) -> Result<(), ExecError> {
        if self.loop_depth > 0 {
            Ok(())
        } else {
            Err(ExecError {
                span: stmt.token.span.clone(),
                msg: "`break` should be used inside looping".into(),
            })
        }
    }

    fn return_stmt(&mut self, stmt: &Return) -> Result<Value, ExecError> {
        stmt.expr
            .as_ref()
            .map(|expr| self.eval(expr))
            .unwrap_or(Ok(Value::Null))
    }

    fn class(&mut self, class: &Class) -> Result<(), ExecError> {
        let name = &class.name;
        // 先占位，以便类方法可以访问到类本身
        self.env.borrow_mut().define(name, Value::Null);

        let methods = class.methods.iter().map(|method| {
            let mname = method.name.ident();

            (mname, LoxFunction::new(method, &self.env, mname == "init"))
        });
        let class = LoxClass::new(name.ident(), methods);

        self.env.borrow_mut().define(name, class.into());

        Ok(())
    }

    fn begin_scope(&mut self) {
        let env = Env::from(&self.env);
        self.env = env.into();
    }

    fn end_scope(&mut self) {
        let env = self
            .env
            .borrow_mut()
            .enclose
            .take()
            .expect("there must be enclosing environment");
        self.env = env;
    }

    fn lookup_var(&self, name: &Lexeme, expr: &Expr) -> Result<Value, ExecError> {
        if let Some(&depth) = self.locals.get(expr) {
            Ok(Env::get_at(self.env.clone(), depth, name.ident()))
        } else {
            GLOBAL_ENV.with(|env| env.borrow().get(name))
        }
    }
}

#[derive(Debug)]
pub struct ExecError {
    pub span: Span,
    pub msg: String,
}

#[derive(Debug)]
pub enum StmtValue {
    Finish,
    Break,
    Return(Value),
}

#[derive(Debug)]
pub struct LoxFunction {
    declare: Function,
    /// 函数声明时所在的上下文，调用时以此为env.enclose
    closure: RcCell<Env>,
    is_init: bool,
}

impl LoxFunction {
    fn new(declare: &Function, closure: &RcCell<Env>, is_init: bool) -> Self {
        Self {
            declare: declare.clone(),
            closure: closure.clone(),
            is_init,
        }
    }

    /// 创建一个存在`this`实例的新环境的`LoxFunction`
    fn bind(&self, this: &LoxInstance) -> Self {
        let mut env = Env::from(&self.closure);
        env.insert("this", this.clone().into());
        Self {
            declare: self.declare.clone(),
            closure: RcCell::new(env),
            is_init: self.is_init,
        }
    }
}

impl std::fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.declare.name.token)
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.declare.params.len()
    }

    fn call(&self, intp: &mut Interpreter, args: Vec<Value>) -> Value {
        let env: RcCell<_> = Env::from(&self.closure).into();

        for (param, arg) in self.declare.params.iter().zip(args) {
            env.borrow_mut().define(param, arg);
        }

        let env = mem::replace(&mut intp.env, env);
        let res = intp.fun_block(&self.declare.body);
        intp.env = env;

        if self.is_init {
            // 调用初始化方法，就把实例返回去
            Env::get_at(self.closure.clone(), 0, "this")
        } else {
            res.unwrap_or(Value::Null)
        }
    }
}

#[derive(Debug)]
pub struct LoxLambda {
    declare: Lambda,
    closure: RcCell<Env>,
}

impl LoxLambda {
    fn new(declare: &Lambda, closure: &RcCell<Env>) -> Self {
        Self {
            declare: declare.clone(),
            closure: closure.clone(),
        }
    }
}

impl std::fmt::Display for LoxLambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<fn>")
    }
}

impl LoxCallable for LoxLambda {
    fn arity(&self) -> usize {
        self.declare.params.len()
    }

    fn call(&self, intp: &mut Interpreter, args: Vec<Value>) -> Value {
        let env: RcCell<_> = Env::from(&self.closure).into();

        for (param, arg) in self.declare.params.iter().zip(args) {
            env.borrow_mut().define(param, arg);
        }

        let env = mem::replace(&mut intp.env, env);
        let res = intp.fun_block(&self.declare.body);
        intp.env = env;

        res.unwrap_or(Value::Null)
    }
}

#[derive(Debug, Clone)]
pub struct LoxClass {
    name: SmolStr,
    methods: HashMap<SmolStr, Rc<LoxFunction>>,
}

impl LoxClass {
    pub fn new<'a>(name: &str, methods: impl Iterator<Item = (&'a str, LoxFunction)>) -> Self {
        Self {
            name: name.into(),
            methods: methods.map(|(s, f)| (s.into(), Rc::new(f))).collect(),
        }
    }
}

impl std::fmt::Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        self.methods
            .get("init")
            .map(|f| f.arity())
            .unwrap_or_default()
    }

    fn call(&self, intp: &mut Interpreter, args: Vec<Value>) -> Value {
        let instance = LoxInstance {
            class: self.clone(),
            fields: Default::default(),
        };

        if let Some(init) = self.methods.get("init") {
            init.bind(&instance).call(intp, args);
        }

        instance.into()
    }
}

/// 实例就是具备状态的类
#[derive(Debug, Clone)]
pub struct LoxInstance {
    class: LoxClass,
    fields: HashMap<SmolStr, Value>,
}

impl LoxInstance {
    pub fn get(&self, field: &Lexeme) -> Result<Value, ExecError> {
        let name = field.ident();

        self.fields
            .get(name)
            .cloned()
            .or_else(|| self.class.methods.get(name).map(|v| v.bind(self).into()))
            .ok_or_else(|| ExecError {
                span: field.span.clone(),
                msg: format!("undefined property `{name}`"),
            })
    }

    pub fn set(&mut self, field: &Lexeme, value: Value) {
        self.fields.insert(field.ident().into(), value);
    }
}

impl std::fmt::Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class)
    }
}
