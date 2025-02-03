use std::collections::HashMap;

use smol_str::SmolStr;

use super::ExecError;
use crate::{exec::Value, scan::Lexeme, utils::RcCell};

#[derive(Debug, Default)]
pub struct Env {
    values: HashMap<SmolStr, Value>,
    pub(super) enclose: Option<RcCell<Self>>,
}

impl From<&RcCell<Env>> for Env {
    fn from(enclose: &RcCell<Env>) -> Env {
        Self {
            values: HashMap::default(),
            enclose: Some(enclose.clone()),
        }
    }
}

impl Env {
    pub fn new<I>(values: I) -> Self
    where
        I: IntoIterator<Item = (SmolStr, Value)>,
    {
        Self {
            values: HashMap::from_iter(values),
            enclose: None,
        }
    }

    pub fn define(&mut self, name: &Lexeme, value: Value) {
        self.insert(name.ident(), value);
    }

    pub fn insert(&mut self, name: &str, value: Value) {
        self.values.insert(name.into(), value);
    }

    pub fn get(&self, name: &Lexeme) -> Result<Value, ExecError> {
        let s = name.ident();

        self.get_rec(s).ok_or_else(|| ExecError {
            span: name.span.clone(),
            msg: format!("undefined variable {s}"),
        })
    }

    pub fn get_at(this: RcCell<Self>, depth: usize, name: &str) -> Value {
        let mut env = this;
        for _ in 0..depth {
            let enclose = env.borrow().enclose.as_ref().unwrap().clone();
            env = enclose;
        }
        let env = env.borrow();
        env.values.get(name).cloned().unwrap()
    }

    pub fn assign(&mut self, name: &Lexeme, value: Value) -> Result<(), ExecError> {
        let s = name.ident();

        self.assign_rec(s, value).ok_or_else(|| ExecError {
            span: name.span.clone(),
            msg: format!("undefined variable {s}"),
        })
    }

    pub fn assign_at(this: RcCell<Self>, depth: usize, name: &Lexeme, value: Value) {
        let mut env = this;
        for _ in 0..depth {
            let enclose = env.borrow_mut().enclose.as_ref().unwrap().clone();
            env = enclose;
        }
        let mut env = env.borrow_mut();
        env.define(name, value);
    }
}

impl Env {
    fn assign_rec(&mut self, name: &str, value: Value) -> Option<()> {
        if let Some(v) = self.values.get_mut(name) {
            *v = value;
            Some(())
        } else {
            self.enclose.as_mut()?.borrow_mut().assign_rec(name, value)
        }
    }

    fn get_rec(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.values.get(name).cloned() {
            Some(v)
        } else {
            self.enclose.as_ref()?.borrow().get_rec(name)
        }
    }
}
