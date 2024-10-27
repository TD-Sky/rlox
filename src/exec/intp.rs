use std::collections::HashMap;
use std::mem;
use std::ops::Range;

use smol_str::{SmolStr, SmolStrBuilder};

use crate::{
    exec::Value,
    parse::{expr::*, stmt::*},
    scan::{Lexeme, Token},
};

#[derive(Debug, Default)]
pub struct Interpreter {
    env: Box<Env>,
    ctrls: Vec<ControlFlow>,
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), ExecError> {
        for stmt in stmts {
            self.execute(stmt)?;
            if let Some(ControlFlow::Break) = self.ctrls.last() {
                break;
            }
        }

        Ok(())
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, ExecError> {
        match expr {
            Expr::Binary(expr) => self.binary(expr),
            Expr::Grouping(expr) => self.grouping(expr),
            Expr::Literal(expr) => Ok(self.literal(expr)),
            Expr::Unary(expr) => self.unary(expr),
            Expr::Variable(var) => self.env.get(&var.name),
            Expr::Assign(expr) => {
                let value = self.eval(&expr.value)?;
                self.env.assign(&expr.name, value.clone())?;
                Ok(value)
            }
            Expr::Conditional(expr) => self.conditional(expr),
            _ => unreachable!(),
        }
    }

    fn break_stmt(&mut self, stmt: &Break) -> Result<(), ExecError> {
        match self.ctrls.last_mut() {
            Some(status) if *status == ControlFlow::Looping => {
                *status = ControlFlow::Break;
                Ok(())
            }
            None => Err(ExecError {
                span: stmt.token.span.range.clone(),
                msg: "`break` should be used inside looping".into(),
            }),
            _ => unreachable!(),
        }
    }
}

impl Interpreter {
    fn execute(&mut self, stmt: &Stmt) -> Result<(), ExecError> {
        match stmt {
            Stmt::Expr(expr) => {
                self.eval(expr)?;
            }
            Stmt::Print(expr) => {
                let value = self.eval(expr)?;
                println!("{value}");
            }
            Stmt::Var(var) => self.declare(var)?,
            Stmt::Block(block) => self.block(block)?,
            Stmt::If(stmt) => self.if_stmt(stmt)?,
            Stmt::While(stmt) => self.while_stmt(stmt)?,
            Stmt::For(stmt) => self.for_stmt(stmt)?,
            Stmt::Break(stmt) => self.break_stmt(stmt)?,
        };

        Ok(())
    }

    fn declare(&mut self, var: &Var) -> Result<(), ExecError> {
        let value = match &var.init {
            Some(expr) => self.eval(expr)?,
            None => Value::Null,
        };
        self.env.define(&var.name, value);

        Ok(())
    }

    fn block(&mut self, block: &Block) -> Result<(), ExecError> {
        self.enter_env();
        let res = self.interpret(&block.stmts);
        self.leave_env();
        res
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
                span: expr.operator.span.range.clone(),
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
            span: expr.operator.span.range.clone(),
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
                        span: expr.operator.span.range.clone(),
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
                        span: expr.operator.span.range.clone(),
                        msg: "expected string as operand".into(),
                    })?);
                    Value::String(sb.finish())
                }
                _ => {
                    return Err(ExecError {
                        span: expr.operator.span.range.clone(),
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

    fn if_stmt(&mut self, stmt: &If) -> Result<(), ExecError> {
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

        Ok(())
    }

    fn while_stmt(&mut self, stmt: &While) -> Result<(), ExecError> {
        let While { condition, body } = stmt;

        self.ctrls.push(ControlFlow::Looping);

        let res = || -> Result<(), ExecError> {
            while self.eval(condition)?.as_bool() {
                self.execute(body)?;
                if let Some(ControlFlow::Break) = self.ctrls.last() {
                    break;
                }
            }

            Ok(())
        }();

        self.ctrls.pop();

        res
    }

    fn for_stmt(&mut self, stmt: &For) -> Result<(), ExecError> {
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

        self.ctrls.push(ControlFlow::Looping);

        let res = || -> Result<(), ExecError> {
            match (condition, change) {
                (Some(condition), Some(change)) => {
                    while self.eval(condition)?.as_bool() {
                        self.execute(body)?;
                        if let Some(ControlFlow::Break) = self.ctrls.last() {
                            break;
                        }
                        self.eval(change)?;
                    }
                }
                (Some(condition), None) => {
                    while self.eval(condition)?.as_bool() {
                        self.execute(body)?;
                        if let Some(ControlFlow::Break) = self.ctrls.last() {
                            break;
                        }
                    }
                }
                (None, Some(change)) => loop {
                    self.execute(body)?;
                    if let Some(ControlFlow::Break) = self.ctrls.last() {
                        break;
                    }
                    self.eval(change)?;
                },
                (None, None) => loop {
                    self.execute(body)?;
                    if let Some(ControlFlow::Break) = self.ctrls.last() {
                        break;
                    }
                },
            }

            Ok(())
        }();

        self.ctrls.pop();
        if new_env {
            self.leave_env();
        }

        res
    }

    fn enter_env(&mut self) {
        let scope = Box::new(Env::default());
        let previous = mem::replace(&mut self.env, scope);
        self.env.enclosing = Some(previous);
    }

    fn leave_env(&mut self) {
        self.env = self
            .env
            .enclosing
            .take()
            .expect("there must be enclosing environment");
    }
}

#[derive(Debug)]
pub struct ExecError {
    pub span: Range<usize>,
    pub msg: String,
}

#[derive(Debug, Default)]
struct Env {
    values: HashMap<SmolStr, Value>,
    enclosing: Option<Box<Self>>,
}

impl Env {
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

        if let Some(v) = self.values.get_mut(s) {
            *v = value;
            Ok(())
        } else if let Some(env) = &mut self.enclosing {
            env.assign(name, value)
        } else {
            Err(ExecError {
                span: name.span.range.clone(),
                msg: format!("undefined variable {s}"),
            })
        }
    }

    fn get(&self, name: &Lexeme) -> Result<Value, ExecError> {
        let Token::Identifier(s) = &name.token else {
            panic!("expected Identifier");
        };

        if let Some(v) = self.values.get(s).cloned() {
            Ok(v)
        } else if let Some(env) = &self.enclosing {
            env.get(name)
        } else {
            Err(ExecError {
                span: name.span.range.clone(),
                msg: format!("undefined variable {s}"),
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ControlFlow {
    Break,
    Looping,
}
