use crate::parser::{Statement, Ast, Expression, PrefOp};

#[derive(Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}


pub fn eval(ast: Ast) -> Object {
    let mut result = Object::Null;
    for stmt in ast.statements {
        result = eval_statement(stmt);
    }

    result
}

fn eval_statement(stmt: Statement) -> Object {
    match stmt {
        Statement::Expression(expr) => eval_expression(expr),
        _ => Object::Null,
    }
}

fn eval_expression(expr: Expression) -> Object {
    match expr {
        Expression::IntLiteral(value) => Object::Integer(value),
        Expression::Boolean(value) => Object::Boolean(value),
        Expression::Prefix(op, expr) => eval_pref_expression(op, *expr),
        _ => Object::Null,
    }
}

fn eval_pref_expression(op: PrefOp, expr: Expression) -> Object {
    match op {
        PrefOp::Not => {
            let value = match eval_expression(expr) {
                Object::Boolean(value) => value,
                Object::Integer(value) => value != 0,
                _ => panic!("Expected boolean"), 
            };

            Object::Boolean(!value)
        },
        PrefOp::Minus => {
            let value = match eval_expression(expr) {
                Object::Integer(value) => value,
                _ => panic!("Expected number"),
            };

            Object::Integer(-value)
        },
    }
}
