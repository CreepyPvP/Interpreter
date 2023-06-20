use std::rc::Rc;

use crate::{
    environment::Environment,
    parser::{Ast, Expression, Ident, InfOp, PrefOp, Statement},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Function(Vec<Ident>, Statement, Environment),
}

pub fn eval(ast: Ast, env: &mut Environment) -> Object {
    eval_program(ast.statements, env)
}

fn eval_program(stmts: Vec<Statement>, env: &mut Environment) -> Object {
    let mut result = Object::Null;
    for stmt in stmts {
        result = eval_statement(stmt, env);

        if let Object::Return(ret) = result {
            return *ret;
        }
    }

    result
}

fn eval_statement(stmt: Statement, env: &mut Environment) -> Object {
    match stmt {
        Statement::Expression(expr) => eval_expression(expr, env),
        Statement::Block(exprs) => eval_block_statement(exprs, env),
        Statement::Return(expr) => eval_return_statement(expr, env),
        Statement::Let(ident, expr) => eval_let_statement(ident, expr, env),
    }
}

fn eval_let_statement(ident: Ident, expr: Expression, env: &mut Environment) -> Object {
    let Ident(ident) = ident;
    let value = eval_expression(expr, env);
    env.set(ident, value);
    Object::Null
}

fn eval_block_statement(stmts: Vec<Statement>, env: &mut Environment) -> Object {
    let mut result = Object::Null;
    for stmt in stmts {
        result = eval_statement(stmt, env);

        if matches!(result, Object::Return(_)) {
            return result;
        }
    }

    result
}

fn eval_expression(expr: Expression, env: &mut Environment) -> Object {
    match expr {
        Expression::IntLiteral(value) => Object::Integer(value),
        Expression::Boolean(value) => Object::Boolean(value),
        Expression::Prefix(op, expr) => eval_pref_expression(op, *expr, env),
        Expression::Infix(left, op, right) => eval_inf_expression(*left, op, *right, env),
        Expression::If(expr, stmt0, stmt1) => eval_if_expression(*expr, *stmt0, stmt1.map(|v| *v), env),
        Expression::Identifier(Ident(ident)) => eval_ident_expression(ident, env),
        Expression::FunctionLiteral(params, stmt) => eval_fn_literal_expression(params, *stmt, env),
        Expression::Call(expr, params) => eval_fn_call(*expr, params, env),
    }
}

fn eval_fn_call(expr: Expression, param_exprs: Vec<Expression>, env: &mut Environment) -> Object {
    let (params, stmt, mut fn_env) = match eval_expression(expr, env) {
        Object::Function(params, stmt, env) => (params, stmt, env),
        _ => panic!("Expected callable expression"),
    };

    for (i, param) in params.iter().enumerate() {
        let value = param_exprs.get(i).map(|expr| eval_expression(expr.to_owned(), env)).unwrap_or(Object::Null);
        let Ident(key) = param;
        fn_env.set(key.to_owned(), value);
    }

    eval_program(vec!(stmt), &mut fn_env)
}

fn eval_fn_literal_expression(params: Vec<Ident>, stmt: Statement, env: &mut Environment) -> Object {
    Object::Function(params, stmt, env.clone())
}

fn eval_ident_expression(ident: String, env: &mut Environment) -> Object {
    match env.get(&ident) {
        Some(value) => value,
        None => panic!("Unknown identifier: {}", ident),
    }
}

fn eval_return_statement(expr: Expression, env: &mut Environment) -> Object {
    Object::Return(Box::new(eval_expression(expr, env)))
}

fn eval_if_expression(
    expr: Expression,
    stmt0: Statement,
    stmt1: Option<Statement>,
    env: &mut Environment,
) -> Object {
    match expect_bool(expr, env) {
        true => eval_statement(stmt0, env),
        false if stmt1.is_some() => eval_statement(stmt1.unwrap(), env),
        _ => Object::Null,
    }
}

fn eval_inf_expression(
    left: Expression,
    op: InfOp,
    right: Expression,
    env: &mut Environment,
) -> Object {
    match op {
        InfOp::Mul => Object::Integer(expect_int(left, env) * expect_int(right, env)),
        InfOp::Div => Object::Integer(expect_int(left, env) / expect_int(right, env)),
        InfOp::Plus => Object::Integer(expect_int(left, env) + expect_int(right, env)),
        InfOp::Minus => Object::Integer(expect_int(left, env) - expect_int(right, env)),

        InfOp::Eq => Object::Boolean(eval_expression(left, env) == eval_expression(right, env)),
        InfOp::NotEq => Object::Boolean(eval_expression(left, env) != eval_expression(right, env)),
        InfOp::GreaterThan => Object::Boolean(expect_int(left, env) > expect_int(right, env)),
        InfOp::LessThan => Object::Boolean(expect_int(left, env) < expect_int(right, env)),
    }
}

fn eval_pref_expression(op: PrefOp, expr: Expression, env: &mut Environment) -> Object {
    match op {
        PrefOp::Not => Object::Boolean(!expect_bool(expr, env)),
        PrefOp::Minus => Object::Integer(-expect_int(expr, env)),
    }
}

fn expect_int(expr: Expression, env: &mut Environment) -> i64 {
    match eval_expression(expr, env) {
        Object::Integer(value) => value,
        _ => panic!("Expected number expression"),
    }
}

fn expect_bool(expr: Expression, env: &mut Environment) -> bool {
    match eval_expression(expr, env) {
        Object::Boolean(value) => value,
        Object::Integer(value) => value != 0,
        _ => panic!("Expected boolean"),
    }
}
