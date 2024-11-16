use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use super::{
    ast::{self, AstVisitor, Expr, Ident},
    lexer::{Token, TokenValue},
};

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Object(Object),
    Nil,
    Void,
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{s}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Object(o) => write!(f, "{o}"),
            Value::Nil => write!(f, "nil"),
            Value::Void => Ok(()),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Object {}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[Object]")
    }
}

#[derive(Debug)]
pub struct RuntimeError<'a> {
    token: Token<'a>,
    msg: &'static str,
    kind: RErrorKind,
}

impl<'a> RuntimeError<'a> {
    pub fn undefined(ident: Ident<'a>) -> Self {
        Self {
            token: ident.ident,
            msg: "Undefined reference to variable",
            kind: RErrorKind::Undefined,
        }
    }
}

pub struct RichRuntimeError<'a, 'src> {
    error: &'a RuntimeError<'src>,
    source: &'src str,
}

impl Display for RichRuntimeError<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (row, col) = self.error.token.lexeme.find_in_source(self.source);
        write!(
            f,
            "{}: {} '{}' at {}:{}",
            self.error.kind, self.error.msg, self.error.token.lexeme.payload, row, col
        )
    }
}

impl<'src> RuntimeError<'src> {
    pub fn new(token: Token<'src>, msg: &'static str, kind: RErrorKind) -> Self {
        Self { token, msg, kind }
    }

    pub fn display<'a>(&'a self, src: &'src str) -> impl Display + 'a {
        RichRuntimeError {
            error: self,
            source: src,
        }
    }
}

#[derive(Debug)]
pub enum RErrorKind {
    TypeError,
    Undefined,
}

impl Display for RErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RErrorKind::TypeError => write!(f, "TypeError"),
            RErrorKind::Undefined => write!(f, "Undefined"),
        }
    }
}

#[derive(Default)]
struct Scope {
    vars: HashMap<String, Value>,
}

impl Scope {
    pub fn declare(&mut self, var: String, value: Value) {
        self.vars.insert(var, value);
    }

    pub fn get<'a>(&'a self, var: &str) -> Option<&'a Value> {
        self.vars.get(var)
    }

    pub fn get_mut<'a>(&'a mut self, var: &str) -> Option<&'a mut Value> {
        self.vars.get_mut(var)
    }
}

pub struct Env {
    scopes: Vec<Scope>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    pub fn declare(&mut self, var: String, value: Value) {
        self.scopes.last_mut().unwrap().declare(var, value);
    }

    pub fn with_scope<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce() -> T,
    {
        self.enter_scope();
        let out = f();
        self.exit_scope();
        out
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("Tried to exit from global scope");
        }
    }

    pub fn get<'a>(&'a self, var: &str) -> Option<&'a Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(var) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_mut<'a>(&'a mut self, var: &str) -> Option<&'a mut Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_mut(var) {
                return Some(value);
            }
        }
        None
    }
}

pub struct Interpreter {
    environment: Env,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Env::new(),
        }
    }

    pub fn evaluate<'a>(&mut self, ast: &ast::LoxAst<'a>) -> Result<Value, RuntimeError<'a>> {
        self.visit_ast(ast)
    }
}

impl<'a> AstVisitor<'a> for Interpreter {
    type Output = Result<Value, RuntimeError<'a>>;

    fn visit_ast(&mut self, ast: &ast::LoxAst<'a>) -> Self::Output {
        for stmt in &ast.statements {
            self.visit_stmt(stmt)?;
        }
        Ok(Value::Void)
    }

    fn visit_expr_stmt(&mut self, stmt: &ast::ExprStmt<'a>) -> Self::Output {
        self.visit_expr(&stmt.expr)?;
        Ok(Value::Void)
    }

    fn visit_print_stmt(&mut self, stmt: &ast::PrintStmt<'a>) -> Self::Output {
        let value = self.visit_expr(&stmt.expr)?;
        println!("{value}");
        Ok(Value::Void)
    }

    fn visit_stmt(&mut self, stmt: &ast::Stmt<'a>) -> Self::Output {
        stmt.walk(self)
    }

    fn visit_expr(&mut self, expr: &Expr<'a>) -> Self::Output {
        expr.walk(self)
    }

    fn visit_binary_expr(&mut self, binary_expr: &ast::BinaryExpr<'a>) -> Self::Output {
        let lhs = self.visit_expr(&binary_expr.lhs)?;
        let rhs = self.visit_expr(&binary_expr.rhs)?;
        match &binary_expr.op.value {
            TokenValue::Plus => match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
                (Value::String(lhs), Value::String(rhs)) => Ok(Value::String(lhs + &rhs)),
                (_lhs, _rhs) => Err(RuntimeError::new(
                    binary_expr.op.clone(),
                    "Invalid operands for binary expression",
                    RErrorKind::TypeError,
                )),
            },
            TokenValue::Minus => match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
                (_lhs, _rhs) => Err(RuntimeError::new(
                    binary_expr.op.clone(),
                    "Invalid operands for binary expression",
                    RErrorKind::TypeError,
                )),
            },
            TokenValue::Star => match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
                (_lhs, _rhs) => Err(RuntimeError::new(
                    binary_expr.op.clone(),
                    "Invalid operands for binary expression",
                    RErrorKind::TypeError,
                )),
            },
            TokenValue::Slash => match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
                (_lhs, _rhs) => Err(RuntimeError::new(
                    binary_expr.op.clone(),
                    "Invalid operands for binary expression",
                    RErrorKind::TypeError,
                )),
            },
            TokenValue::Greater => match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs > rhs)),
                (_lhs, _rhs) => Err(RuntimeError::new(
                    binary_expr.op.clone(),
                    "Invalid operands for binary expression",
                    RErrorKind::TypeError,
                )),
            },
            TokenValue::GreaterEqual => match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs >= rhs)),
                (_lhs, _rhs) => Err(RuntimeError::new(
                    binary_expr.op.clone(),
                    "Invalid operands for binary expression",
                    RErrorKind::TypeError,
                )),
            },
            TokenValue::Less => match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs < rhs)),
                (_lhs, _rhs) => Err(RuntimeError::new(
                    binary_expr.op.clone(),
                    "Invalid operands for binary expression",
                    RErrorKind::TypeError,
                )),
            },
            TokenValue::LessEqual => match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs <= rhs)),
                (_lhs, _rhs) => Err(RuntimeError::new(
                    binary_expr.op.clone(),
                    "Invalid operands for binary expression",
                    RErrorKind::TypeError,
                )),
            },
            TokenValue::EqualEqual => Ok(Value::Bool(equals(&lhs, &rhs))),
            TokenValue::BangEqual => Ok(Value::Bool(!equals(&lhs, &rhs))),
            other => unreachable!("Unexpected token for binary expression: {other:?}"),
        }
    }

    fn visit_unary_expr(&mut self, unary_expr: &ast::UnaryExpr<'a>) -> Self::Output {
        let rhs = self.visit_expr(&unary_expr.rhs)?;
        match &unary_expr.op.value {
            TokenValue::Minus => match rhs {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err(RuntimeError::new(
                    unary_expr.op.clone(),
                    "Invalid operand for unary expression",
                    RErrorKind::TypeError,
                )),
            },
            TokenValue::Bang => Ok(Value::Bool(!truthy(&rhs))),
            other => unreachable!("Unexpected token for unary expression: {other:?}"),
        }
    }

    fn visit_literal(&mut self, literal_expr: &ast::Literal<'a>) -> Self::Output {
        match &literal_expr.value.value {
            TokenValue::String(s) => Ok(Value::String(s.clone())),
            TokenValue::Number(n) => Ok(Value::Number(*n)),
            TokenValue::True => Ok(Value::Bool(true)),
            TokenValue::False => Ok(Value::Bool(false)),
            TokenValue::Nil => Ok(Value::Nil),
            other => unreachable!("Unexpected literal token: {other:?}"),
        }
    }

    fn visit_group_expr(&mut self, group_expr: &ast::Grouping<'a>) -> Self::Output {
        self.visit_expr(&group_expr.expr)
    }

    fn visit_var_decl(&mut self, decl: &ast::VarDecl<'a>) -> Self::Output {
        let rhs = if let Some(expr) = decl.initializer() {
            self.visit_expr(expr)?
        } else {
            Value::Nil
        };
        self.environment.declare(decl.name(), rhs);
        Ok(Value::Void)
    }

    fn visit_ident(&mut self, ident: &ast::Ident<'a>) -> Self::Output {
        match self.environment.get(&ident.name()) {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError::undefined(ident.clone())),
        }
    }

    fn visit_assign(&mut self, assign: &ast::Assignment<'a>) -> Self::Output {
        let rhs = self.visit_expr(&assign.rhs)?;
        let Some(place) = self.environment.get_mut(&assign.lhs.name()) else {
            return Err(RuntimeError::undefined(assign.lhs.clone()));
        };
        *place = rhs.clone();
        Ok(rhs)
    }

    fn visit_block(&mut self, block: &ast::Block<'a>) -> Self::Output {
        self.with_scope(|this| {
            for stmt in &block.stmts {
                this.visit_stmt(stmt)?;
            }
            Ok(Value::Void)
        })
    }
}

impl Interpreter {
    fn with_scope<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.environment.enter_scope();
        let out = f(self);
        self.environment.exit_scope();
        out
    }
}

fn equals(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
        (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
        (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
        (Value::Object(lhs), Value::Object(rhs)) => lhs == rhs,
        (Value::Nil, Value::Nil) => true,
        (_, Value::Nil) => false,
        (Value::Nil, _) => false,
        _ => panic!("Type error: Invalid use of '=='"),
    }
}

fn truthy(value: &Value) -> bool {
    match value {
        Value::Bool(b) => *b,
        Value::Nil => false,
        _ => true,
    }
}
