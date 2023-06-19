use crate::parser::{Statement, Ast, Expression, PrefOp, InfOp};

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}


pub fn eval(ast: Ast) -> Object {
    eval_statements(ast.statements)
}

fn eval_statements(stmts: Vec<Statement>) -> Object {
    let mut result = Object::Null;
    for stmt in stmts {
        result = eval_statement(stmt);
    }

    result
}

fn eval_statement(stmt: Statement) -> Object {
    match stmt {
        Statement::Expression(expr) => eval_expression(expr),
        Statement::Block(exprs) => eval_statements(exprs),
        _ => Object::Null,
    }
}

fn eval_expression(expr: Expression) -> Object {
    match expr {
        Expression::IntLiteral(value) => Object::Integer(value),
        Expression::Boolean(value) => Object::Boolean(value),
        Expression::Prefix(op, expr) => eval_pref_expression(op, *expr),
        Expression::Infix(left, op, right) => eval_inf_expression(*left, op, *right),
        Expression::If(expr, stmt0, stmt1) => eval_if_expression(*expr, *stmt0, stmt1.map(|v| *v)),
        _ => Object::Null,
    }
}

fn eval_if_expression(expr: Expression, stmt0: Statement, stmt1: Option<Statement>) -> Object {
    match expect_bool(expr) {
        true => eval_statement(stmt0),
        false if stmt1.is_some() => eval_statement(stmt1.unwrap()),
        _ => Object::Null,
    }
}

fn eval_inf_expression(left: Expression, op: InfOp, right: Expression) -> Object {
    match op {
        InfOp::Mul => Object::Integer(expect_int(left) * expect_int(right)),
        InfOp::Div => Object::Integer(expect_int(left) / expect_int(right)),
        InfOp::Plus => Object::Integer(expect_int(left) + expect_int(right)),
        InfOp::Minus => Object::Integer(expect_int(left) - expect_int(right)),

        InfOp::Eq => Object::Boolean(eval_expression(left) == eval_expression(right)),
        InfOp::NotEq => Object::Boolean(eval_expression(left) != eval_expression(right)),
        InfOp::GreaterThan => Object::Boolean(expect_int(left) > expect_int(right)),
        InfOp::LessThan => Object::Boolean(expect_int(left) < expect_int(right)),
    }
}

fn eval_pref_expression(op: PrefOp, expr: Expression) -> Object {
    match op {
        PrefOp::Not => Object::Boolean(!expect_bool(expr)),
        PrefOp::Minus => Object::Integer(-expect_int(expr)),
    }
}

fn expect_int(expr: Expression) -> i64 {
    match eval_expression(expr) {
        Object::Integer(value) => value,
        _ => panic!("Expected number expression"),
    }
}

fn expect_bool(expr: Expression) -> bool {
    match eval_expression(expr) {
        Object::Boolean(value) => value,
        Object::Integer(value) => value != 0,
        _ => panic!("Expected boolean"), 
    }
}
