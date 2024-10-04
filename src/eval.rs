use std::ops::Range;

use smol_str::SmolStrBuilder;

use crate::{expr::*, scan::Token, Value};

pub fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Binary(expr) => binary(expr),
        Expr::Grouping(expr) => grouping(expr),
        Expr::Literal(expr) => Ok(literal(expr)),
        Expr::Unary(expr) => unary(expr),
        _ => todo!(),
    }
}

fn grouping(expr: &Grouping) -> Result<Value, EvalError> {
    eval(&expr.expression)
}

fn literal(expr: &Literal) -> Value {
    expr.clone().into()
}

fn unary(expr: &Unary) -> Result<Value, EvalError> {
    let right = eval(&expr.right)?;

    let value = match expr.operator.token {
        Token::Bang => Value::Bool(!right.as_bool().ok_or_else(|| EvalError {
            span: expr.operator.span.range.clone(),
            msg: "expected boolean/null as operand".into(),
        })?),
        Token::Minus => Value::Number(-right.as_number().ok_or_else(|| EvalError {
            span: expr.operator.span.range.clone(),
            msg: "expected number as operand".into(),
        })?),
        _ => unreachable!(),
    };

    Ok(value)
}

fn binary(expr: &Binary) -> Result<Value, EvalError> {
    let left = eval(&expr.left)?;
    let right = eval(&expr.right)?;

    let expect_num = || EvalError {
        span: expr.operator.span.range.clone(),
        msg: "expected number as operand".into(),
    };

    let value = match expr.operator.token {
        Token::Equal => Value::Bool(left == right),
        Token::BangEqual => Value::Bool(left != right),
        Token::Greater => Value::Bool(
            left.as_number().ok_or_else(expect_num)? > right.as_number().ok_or_else(expect_num)?,
        ),
        Token::GreaterEqual => Value::Bool(
            left.as_number().ok_or_else(expect_num)? >= right.as_number().ok_or_else(expect_num)?,
        ),
        Token::Less => Value::Bool(
            left.as_number().ok_or_else(expect_num)? < right.as_number().ok_or_else(expect_num)?,
        ),
        Token::LessEqual => Value::Bool(
            left.as_number().ok_or_else(expect_num)? <= right.as_number().ok_or_else(expect_num)?,
        ),
        Token::Minus => Value::Number(
            left.as_number().ok_or_else(expect_num)? - right.as_number().ok_or_else(expect_num)?,
        ),
        Token::Slash => {
            let dividend = left.as_number().ok_or_else(expect_num)?;
            let divisor = right.as_number().ok_or_else(expect_num)?;

            if divisor == 0.0 {
                return Err(EvalError {
                    span: expr.operator.span.range.clone(),
                    msg: "division by zero".into(),
                });
            }

            Value::Number(dividend / divisor)
        }
        Token::Star => Value::Number(
            left.as_number().ok_or_else(expect_num)? * right.as_number().ok_or_else(expect_num)?,
        ),
        Token::Plus => match (left, right) {
            (Value::Number(lhs), rhs) => {
                Value::Number(lhs + rhs.as_number().ok_or_else(expect_num)?)
            }
            (Value::String(lhs), rhs) => {
                let mut sb = SmolStrBuilder::new();
                sb.push_str(&lhs);
                sb.push_str(rhs.as_str().ok_or_else(|| EvalError {
                    span: expr.operator.span.range.clone(),
                    msg: "expected string as operand".into(),
                })?);
                Value::String(sb.finish())
            }
            _ => {
                return Err(EvalError {
                    span: expr.operator.span.range.clone(),
                    msg: "expected number/string as operands".into(),
                })
            }
        },
        _ => unreachable!(),
    };

    Ok(value)
}

#[derive(Debug)]
pub struct EvalError {
    pub span: Range<usize>,
    pub msg: String,
}
