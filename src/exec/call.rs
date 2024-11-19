use std::time::SystemTime;

use super::{Interpreter, Value};

pub type LoxCall = dyn Fn(&mut Interpreter, Vec<Value>) -> Value;

pub trait LoxCallable: std::fmt::Debug + std::fmt::Display {
    fn arity(&self) -> usize;

    fn call(&self, intp: &mut Interpreter, args: Vec<Value>) -> Value;
}

pub struct NativeFn {
    pub arity: usize,
    pub call: Box<LoxCall>,
}

impl std::fmt::Display for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<native fn>")
    }
}

impl std::fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFn")
            .field("arity", &self.arity)
            .field("call", &"<lox call>")
            .finish()
    }
}

impl LoxCallable for NativeFn {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, intp: &mut Interpreter, args: Vec<Value>) -> Value {
        (self.call)(intp, args)
    }
}

pub fn clock() -> NativeFn {
    NativeFn {
        arity: 0,
        call: Box::new(|_, _| {
            Value::Number(
                SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap()
                    .as_secs_f64(),
            )
        }),
    }
}
