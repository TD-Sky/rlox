use std::collections::HashMap;
use std::mem;

use smol_str::{SmolStr, SmolStrBuilder};

use super::call::{clock, LoxCallable};
use crate::{
    exec::Value,
    parse::{expr::*, stmt::*},
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
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            env: GLOBAL_ENV.with(Clone::clone),
            loop_depth: 0,
        }
    }
}

impl Interpreter {
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
            Expr::Variable(var) => self.env.borrow().get(&var.name),
            Expr::Assign(expr) => {
                let value = self.eval(&expr.value)?;
                self.env.borrow_mut().assign(&expr.name, value.clone())?;
                Ok(value)
            }
            Expr::Conditional(expr) => self.conditional(expr),
            Expr::Call(expr) => self.call(expr),
            Expr::Lambda(expr) => Ok(LoxLambda::new(expr, &self.env).into()),
            _ => unreachable!(),
        }
    }
}

impl Interpreter {
    fn execute(&mut self, stmt: &Stmt) -> Result<StmtValue, ExecError> {
        Ok(match stmt {
            Stmt::Expr(expr) => return self.eval(expr).map(|_| StmtValue::Finish),
            Stmt::Print(expr) => {
                let value = self.eval(expr)?;
                println!("{value}");
                StmtValue::Finish
            }
            Stmt::Var(var) => return self.declare(var).map(|_| StmtValue::Finish),
            Stmt::Block(block) => self.block(block)?,
            Stmt::If(stmt) => self.if_stmt(stmt)?,
            Stmt::While(stmt) => self.while_stmt(stmt)?,
            Stmt::For(stmt) => self.for_stmt(stmt)?,
            Stmt::Break(stmt) => return self.break_stmt(stmt).map(|_| StmtValue::Break),
            Stmt::Fun(fun) => {
                self.declare_fun(fun);
                StmtValue::Finish
            }
            Stmt::Return(rt) => StmtValue::Return(self.return_stmt(rt)?),
        })
    }

    fn declare(&mut self, var: &Var) -> Result<(), ExecError> {
        let value = match &var.init {
            Some(expr) => self.eval(expr)?,
            None => Value::Null,
        };
        self.env.borrow_mut().define(&var.name, value);

        Ok(())
    }

    fn declare_fun(&mut self, fun: &Function) {
        let lox_fun = LoxFunction::new(fun, &self.env);
        self.env.borrow_mut().define(&fun.name, lox_fun.into());
    }

    fn block(&mut self, block: &Block) -> Result<StmtValue, ExecError> {
        self.enter_env();

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

        self.leave_env();

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
        match init {
            Some(Stmt::Var(var)) => {
                self.enter_env();
                self.declare(var).inspect_err(|_| self.leave_env())?;
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
            self.leave_env();
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

    fn enter_env(&mut self) {
        let env = Env::from(&self.env);
        self.env = env.into();
    }

    fn leave_env(&mut self) {
        let env = self
            .env
            .borrow_mut()
            .enclose
            .take()
            .expect("there must be enclosing environment");
        self.env = env;
    }
}

#[derive(Debug)]
pub struct ExecError {
    pub span: Span,
    pub msg: String,
}

#[derive(Debug, Default)]
struct Env {
    values: HashMap<SmolStr, Value>,
    enclose: Option<RcCell<Self>>,
}

impl From<&RcCell<Self>> for Env {
    fn from(enclose: &RcCell<Self>) -> Self {
        Self {
            values: HashMap::default(),
            enclose: Some(enclose.clone()),
        }
    }
}

impl Env {
    fn new<I>(values: I) -> Self
    where
        I: IntoIterator<Item = (SmolStr, Value)>,
    {
        Self {
            values: HashMap::from_iter(values),
            enclose: None,
        }
    }

    fn define(&mut self, name: &Lexeme, value: Value) {
        let Token::Identifier(s) = &name.token else {
            panic!("expected Identifier");
        };

        self.values.insert(s.clone(), value);
    }

    fn assign(&mut self, name: &Lexeme, value: Value) -> Result<(), ExecError> {
        let Token::Identifier(s) = &name.token else {
            panic!("expected Identifier");
        };

        self.assign_rec(s, value).ok_or_else(|| ExecError {
            span: name.span.clone(),
            msg: format!("undefined variable {s}"),
        })
    }

    fn assign_rec(&mut self, name: &str, value: Value) -> Option<()> {
        if let Some(v) = self.values.get_mut(name) {
            *v = value;
            Some(())
        } else {
            self.enclose.as_mut()?.borrow_mut().assign_rec(name, value)
        }
    }

    fn get(&self, name: &Lexeme) -> Result<Value, ExecError> {
        let Token::Identifier(s) = &name.token else {
            panic!("expected Identifier");
        };

        self.get_rec(s).ok_or_else(|| ExecError {
            span: name.span.clone(),
            msg: format!("undefined variable {s}"),
        })
    }

    fn get_rec(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.values.get(name).cloned() {
            Some(v)
        } else {
            self.enclose.as_ref()?.borrow().get_rec(name)
        }
    }
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
}

impl LoxFunction {
    fn new(declare: &Function, closure: &RcCell<Env>) -> Self {
        Self {
            declare: declare.clone(),
            closure: closure.clone(),
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

        res.unwrap_or(Value::Null)
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
