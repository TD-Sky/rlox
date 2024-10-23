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
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), ExecError> {
        for stmt in stmts {
            match stmt {
                Stmt::Expr(expr) => {
                    self.eval(expr)?;
                }
                Stmt::Print(expr) => {
                    let value = self.eval(expr)?;
                    println!("{value}");
                }
                Stmt::Var(var) => {
                    let value = match &var.init {
                        Some(expr) => self.eval(expr)?,
                        None => Value::Null,
                    };
                    self.env.define(&var.name, value);
                }
                Stmt::Block(block) => self.block(block)?,
            }
        }

        Ok(())
    }

    fn block(&mut self, block: &Block) -> Result<(), ExecError> {
        let scope = Box::new(Env::default());
        let previous = mem::replace(&mut self.env, scope);
        self.env.enclosing = Some(previous);

        self.interpret(&block.stmts)?;

        self.env = self
            .env
            .enclosing
            .take()
            .expect("there must be enclosing environment");
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

    fn grouping(&mut self, expr: &Grouping) -> Result<Value, ExecError> {
        self.eval(&expr.expression)
    }

    fn literal(&mut self, expr: &Literal) -> Value {
        expr.clone().into()
    }

    fn unary(&mut self, expr: &Unary) -> Result<Value, ExecError> {
        let right = self.eval(&expr.right)?;

        let value = match expr.operator.token {
            Token::Bang => Value::Bool(!right.as_bool().ok_or_else(|| ExecError {
                span: expr.operator.span.range.clone(),
                msg: "expected boolean/null as operand".into(),
            })?),
            Token::Minus => Value::Number(-right.as_number().ok_or_else(|| ExecError {
                span: expr.operator.span.range.clone(),
                msg: "expected number as operand".into(),
            })?),
            _ => unreachable!(),
        };

        Ok(value)
    }

    fn binary(&mut self, expr: &Binary) -> Result<Value, ExecError> {
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
            question,
            then,
            colon: _,
            or_else,
        } = expr;

        let Some(bvalue) = self.eval(cond)?.as_bool() else {
            return Err(ExecError {
                span: question.span.range.clone(),
                msg: "expected bool expression".into(),
            });
        };

        let expr = if bvalue { then } else { or_else };

        self.eval(expr)
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

        let var = self.values.get_mut(s).ok_or_else(|| ExecError {
            span: name.span.range.clone(),
            msg: format!("undefined variable {s}"),
        })?;
        *var = value;

        Ok(())
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
