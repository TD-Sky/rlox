use std::collections::HashMap;
use std::mem;

use smol_str::SmolStr;

use crate::{
    parse::types::*,
    span::{Span, Spanned},
    Interpreter,
};

/// 将执行器的变量寻址方式固定下来，伴随着时间复杂度的大幅降低。
///
/// 解析器无法解决**对全局环境变量的先引用后声明**的问题，
/// 也就是全局作用域的item无论声明顺序先后都能被观测到。
#[derive(Debug)]
pub struct Resolver<'i> {
    /// 所有作用域叠成的栈，会动态变化；
    /// 映射：变量名到是否初始化
    scopes: Vec<HashMap<SmolStr, bool>>,
    current_ct: Option<ClassType>,
    current_ft: Option<FunctionType>,
    interpreter: &'i mut Interpreter,
}

impl<'i> Resolver<'i> {
    pub fn new(interpreter: &'i mut Interpreter) -> Self {
        Self {
            interpreter,
            current_ct: None,
            current_ft: None,
            scopes: vec![],
        }
    }

    pub fn resolve_stmts(&mut self, stmts: &[Stmt]) -> Result<(), ResolveError> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    pub fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolveError> {
        match expr {
            Expr::Assign(assign) => self.assign(assign, expr),
            Expr::Binary(binary) => self.binary(binary),
            Expr::Call(call) => self.call(call),
            Expr::Grouping(grouping) => self.resolve_expr(&grouping.expression),
            Expr::Literal(_) => Ok(()),
            Expr::Unary(unary) => self.unary(unary),
            Expr::Variable(variable) => self.variable(variable, expr),
            Expr::Conditional(conditional) => self.conditional(conditional),
            Expr::Lambda(lambda) => self.lambda(lambda),
            Expr::Get(get) => self.resolve_expr(&get.object),
            Expr::Set(set) => self.set(set),
            Expr::This(this) => self.this(this, expr),
            Expr::Super(sp) => self.super_expr(sp, expr),
        }
    }

    pub fn interpreter(&mut self) -> &mut Interpreter {
        self.interpreter
    }
}

impl Resolver<'_> {
    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(), ResolveError> {
        match stmt {
            Stmt::Expr(expr) | Stmt::Print(expr) => self.resolve_expr(expr),
            Stmt::Var(var) => self.var_stmt(var),
            Stmt::Block(block) => self.block(block),
            Stmt::If(stmt) => self.if_stmt(stmt),
            Stmt::While(stmt) => self.while_stmt(stmt),
            Stmt::Fun(function) => self.func_stmt(function),
            Stmt::Return(rt) => self.return_stmt(rt),
            Stmt::For(stmt) => self.for_stmt(stmt),
            Stmt::Class(class) => self.class(class),
            Stmt::Break(_) => Ok(()),
        }
    }

    fn resolve_func(&mut self, func: &Function, ft: FunctionType) -> Result<(), ResolveError> {
        let enclose_ft = mem::replace(&mut self.current_ft, Some(ft));

        self.begin_scope();
        for param in &func.params {
            let name = param.ident();
            self.declare(name);
            self.define(name);
        }
        let res = self.resolve_stmts(&func.body.stmts);
        self.end_scope();

        self.current_ft = enclose_ft;

        res
    }

    fn resolve_local(&mut self, expr: &Expr, name: &str) -> Result<(), ResolveError> {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                self.interpreter.resolve(expr, depth);
                return Ok(());
            }
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.into(), false);
        };
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.into(), true);
        };
    }

    fn block(&mut self, block: &Block) -> Result<(), ResolveError> {
        self.begin_scope();
        let res = self.resolve_stmts(&block.stmts);
        self.end_scope();

        res
    }

    fn var_stmt(&mut self, stmt: &Var) -> Result<(), ResolveError> {
        let name = stmt.name.ident();

        // 注意，不同于执行时，此处先声明了一个未初始化的变量，
        // 再解析其初始化表达式，与[`Self::variable`]的处理形成配合
        self.declare(name);
        if let Some(init) = &stmt.init {
            self.resolve_expr(init)?;
        }
        self.define(name);

        Ok(())
    }

    fn variable(&mut self, var: &Variable, shell: &Expr) -> Result<(), ResolveError> {
        let name = var.name.ident();

        // 拒绝诸如
        // ```
        // var x = x + 1;
        // ```
        // 的情况（x未定义）
        if self
            .scopes
            .last()
            .map(|scope| scope.get(name) == Some(&false))
            .unwrap_or_default()
        {
            return Err(ResolveError {
                span: var.span(),
                msg: "cannot read local variable in its own initializer".into(),
            });
        }

        self.resolve_local(shell, name)?;
        Ok(())
    }

    fn conditional(&mut self, expr: &Conditional) -> Result<(), ResolveError> {
        self.resolve_expr(&expr.cond)?;
        self.resolve_expr(&expr.then)?;
        self.resolve_expr(&expr.or_else)?;

        Ok(())
    }

    fn assign(&mut self, expr: &Assign, shell: &Expr) -> Result<(), ResolveError> {
        self.resolve_expr(&expr.value)?;
        self.resolve_local(shell, expr.name.name.ident())
    }

    fn func_stmt(&mut self, func: &Function) -> Result<(), ResolveError> {
        let name = func.name.ident();
        self.declare(name);
        self.define(name);

        self.resolve_func(func, FunctionType::Function)
    }

    fn if_stmt(&mut self, stmt: &If) -> Result<(), ResolveError> {
        self.resolve_expr(&stmt.condition)?;
        self.resolve_stmt(&stmt.then_branch)?;
        if let Some(stmt) = &stmt.else_branch {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    fn return_stmt(&mut self, stmt: &Return) -> Result<(), ResolveError> {
        match &self.current_ft {
            Some(FunctionType::Init) => {
                return Err(ResolveError {
                    span: stmt.span(),
                    msg: "can't return a value from an initializer".into(),
                });
            }
            None => {
                return Err(ResolveError {
                    span: stmt.span(),
                    msg: "can't return from top-level code".into(),
                });
            }
            _ => (),
        }

        if let Some(value) = &stmt.expr {
            self.resolve_expr(value)?;
        }

        Ok(())
    }

    fn while_stmt(&mut self, stmt: &While) -> Result<(), ResolveError> {
        self.resolve_expr(&stmt.condition)?;
        self.resolve_stmt(&stmt.body)?;
        Ok(())
    }

    fn for_stmt(&mut self, stmt: &For) -> Result<(), ResolveError> {
        let For {
            init,
            condition,
            change,
            body,
        } = stmt;

        let mut new_env = false;
        if let Some(Stmt::Var(var)) = init.as_deref() {
            self.begin_scope();
            let name = var.name.ident();
            self.declare(name);
            if let Some(expr) = &var.init {
                self.resolve_expr(expr).inspect_err(|_| self.end_scope())?;
            }
            self.define(name);
            new_env = true;
        }

        let res = || -> Result<(), ResolveError> {
            if let Some(expr) = condition {
                self.resolve_expr(expr)?;
            }

            self.resolve_stmt(body)?;

            if let Some(expr) = change {
                self.resolve_expr(expr)?;
            }

            Ok(())
        }();

        if new_env {
            self.end_scope();
        }

        res
    }

    fn class(&mut self, class: &Class) -> Result<(), ResolveError> {
        let enclose_ct = self.current_ct.replace(ClassType::Class);

        let name = class.name.ident();
        self.declare(name);
        self.define(name);

        if let Some(super_class) = &class.super_class {
            if name != super_class.name.ident() {
                self.current_ct = Some(ClassType::SubClass);
                self.variable(super_class, &Expr::Variable(super_class.clone()))?;
                // 此环境包裹着`this`的环境，使用`super`指向基类
                self.begin_scope();
                self.scopes.last_mut().unwrap().insert("super".into(), true);
            } else {
                return Err(ResolveError {
                    span: class.span(),
                    msg: "a class can't inherit from itself".into(),
                });
            }
        }

        // 类层面的作用域对应于翻译时的实例绑定
        self.begin_scope();
        let res = || -> Result<(), ResolveError> {
            self.scopes.last_mut().unwrap().insert("this".into(), true);
            for method in &class.methods {
                let ft = if method.name.ident() == "init" {
                    FunctionType::Init
                } else {
                    FunctionType::Method
                };

                self.resolve_func(method, ft)?;
            }

            Ok(())
        }();
        self.end_scope();

        if class.super_class.is_some() {
            self.end_scope();
        }

        res?;

        for method in &class.class_methods {
            self.resolve_func(method, FunctionType::Method)
                .inspect_err(|_| {
                    self.current_ct = enclose_ct;
                })?;
        }

        self.current_ct = enclose_ct;

        Ok(())
    }

    fn binary(&mut self, expr: &Binary) -> Result<(), ResolveError> {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)?;
        Ok(())
    }

    fn unary(&mut self, expr: &Unary) -> Result<(), ResolveError> {
        self.resolve_expr(&expr.right)?;
        Ok(())
    }

    fn call(&mut self, expr: &Call) -> Result<(), ResolveError> {
        self.resolve_expr(&expr.callee)?;
        for arg in &expr.arguments {
            self.resolve_expr(arg)?;
        }

        Ok(())
    }

    fn lambda(&mut self, lambda: &Lambda) -> Result<(), ResolveError> {
        self.begin_scope();
        for param in &lambda.params {
            let name = param.ident();
            self.declare(name);
            self.define(name);
        }
        let res = self.block(&lambda.body);
        self.end_scope();
        res
    }

    fn set(&mut self, set: &Set) -> Result<(), ResolveError> {
        self.resolve_expr(&set.object)?;
        self.resolve_expr(&set.value)?;
        Ok(())
    }

    fn this(&mut self, this: &This, shell: &Expr) -> Result<(), ResolveError> {
        if self.current_ct.is_some() {
            self.resolve_local(shell, this.keyword.ident())
        } else {
            Err(ResolveError {
                span: this.span(),
                msg: "can't use `this` outside of a class".into(),
            })
        }
    }

    fn super_expr(&mut self, sp: &Super, shell: &Expr) -> Result<(), ResolveError> {
        if let Some(ClassType::SubClass) = &self.current_ct {
            self.resolve_local(shell, sp.keyword.ident())
        } else {
            Err(ResolveError {
                span: sp.span(),
                msg: "`super` is only allowed used in subclass".into(),
            })
        }
    }
}

#[derive(Debug)]
pub struct ResolveError {
    pub span: Span,
    pub msg: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum FunctionType {
    Function,
    Init,
    Method,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ClassType {
    Class,
    SubClass,
}
