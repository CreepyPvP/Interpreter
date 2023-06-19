use crate::{
    error::AppError,
    lexer::{Lexer, Token},
};

#[derive(Debug)]
pub struct Ast {
    pub statements: Vec<Statement>,
}

impl Ast {
    fn new() -> Self {
        Ast { statements: vec![] }
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Statements: {:?}", self.statements)
    }
}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Debug)]
pub enum PrefOp {
    Not,
    Minus,
}

#[derive(Debug)]
enum InfOp {
    Plus,
    Minus,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Eq,
    NotEq,
}

impl Token {
    fn precedence(&self) -> Precedence {
        match self {
            Self::Eq => Precedence::Equals,
            Self::NotEq => Precedence::Equals,
            Self::LessThan => Precedence::LessGreater,
            Self::GreaterThan => Precedence::LessGreater,
            Self::Plus => Precedence::Sum,
            Self::Minus => Precedence::Sum,
            Self::Slash => Precedence::Product,
            Self::Asterisk => Precedence::Product,
            Self::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(Ident, Expression),
    Return(Expression),
    Expression(Expression),
    Block(Vec<Statement>),
}

#[derive(Debug)]
pub struct Ident(String);

#[derive(Debug)]
pub enum Expression {
    Identifier(Ident),
    IntLiteral(i64),
    Prefix(PrefOp, Box<Expression>),
    Infix(Box<Expression>, InfOp, Box<Expression>),
    Boolean(bool),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    FunctionLiteral(Vec<Ident>, Box<Statement>),
    Call(Box<Expression>, Vec<Expression>),
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<AppError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            errors: vec![],
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.cur_token = std::mem::replace(
            &mut self.peek_token,
            self.lexer.next().unwrap_or(Token::EOF),
        );
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token != token {
            self.peek_error(Token::Ident("Something".to_string()));
            return false;
        }
        self.next_token();
        true
    }
    fn expect_peek_ident(&mut self) -> Option<String> {
        match &self.peek_token {
            Token::Ident(value) => {
                let value = value.to_owned();
                self.next_token();
                Some(value)
            }
            _ => {
                self.peek_error(Token::Ident("Something".to_string()));
                None
            }
        }
    }

    fn peek_error(&mut self, expected: Token) {
        self.errors.push(AppError::ParserError(format!(
            "expected token {:?}, got token {:?}",
            expected, self.peek_token
        )));
    }

    fn peek_precedence(&self) -> Precedence {
        self.peek_token.precedence()
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left = self.parse_prefix();
        if left.is_none() {
            self.errors.push(AppError::ParserError(format!(
                "no prefix parse function for {:?}",
                self.cur_token
            )));
            return None;
        }

        while self.peek_token != Token::Semicolon && precedence < self.peek_precedence() {
            self.next_token();
            left = self.parse_infix(left.unwrap());
            if left.is_none() {
                self.errors.push(AppError::ParserError(format!(
                    "no infix parse function for {:?}",
                    self.cur_token
                )));
                return None;
            }
        }

        left
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::Ident(value) => Self::parse_identifier(value.to_owned()),
            Token::Int(value) => Self::parse_int_literal(value.to_owned()),
            Token::True => Self::parse_bool(true),
            Token::False => Self::parse_bool(false),
            Token::Bang => self.parse_prefix_expression(PrefOp::Not),
            Token::Minus => self.parse_prefix_expression(PrefOp::Minus),
            Token::Lparen => self.parse_grouped(),
            Token::If => self.parse_if(),
            Token::Function => self.parse_fn_literal(),
            _ => None,
        }
    }

    fn parse_infix(&mut self, left: Expression) -> Option<Expression> {
        match &self.cur_token {
            Token::Plus => self.parse_infix_expression(InfOp::Plus, left),
            Token::Minus => self.parse_infix_expression(InfOp::Minus, left),
            Token::Slash => self.parse_infix_expression(InfOp::Div, left),
            Token::Asterisk => self.parse_infix_expression(InfOp::Mul, left),
            Token::Eq => self.parse_infix_expression(InfOp::Eq, left),
            Token::NotEq => self.parse_infix_expression(InfOp::NotEq, left),
            Token::LessThan => self.parse_infix_expression(InfOp::LessThan, left),
            Token::GreaterThan => self.parse_infix_expression(InfOp::GreaterThan, left),
            Token::Lparen => self.parse_call_expression(left),
            _ => None,
        }
    }

    fn parse_call_expression(&mut self, left: Expression) -> Option<Expression> {
        match self.parse_call_arguments() {
            Some(args) => Some(Expression::Call(Box::new(left), args)),
            None => None,
        }
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        self.next_token();
        let mut expressions = vec!();
        if self.cur_token == Token::Rparen {
            return Some(expressions);
        }

        expressions.push(match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        });

        while self.peek_token == Token::Comma {
            self.next_token(); 
            self.next_token(); 

            expressions.push(match self.parse_expression(Precedence::Lowest) {
                Some(expr) => expr,
                None => return None,
            });
        }

        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        Some(expressions)
    } 

    fn parse_identifier(value: String) -> Option<Expression> {
        Some(Expression::Identifier(Ident(value)))
    }

    fn parse_int_literal(value: i64) -> Option<Expression> {
        Some(Expression::IntLiteral(value))
    }

    fn parse_bool(value: bool) -> Option<Expression> {
        Some(Expression::Boolean(value))
    }

    fn parse_grouped(&mut self) -> Option<Expression> {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(Token::Rparen) {
            return None;
        }
        expr
    }

    fn parse_if(&mut self) -> Option<Expression> {
        if !self.expect_peek(Token::Lparen) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(Token::Rparen) {
            return None;
        }
        
        if !self.expect_peek(Token::Lbrace) {
            return None;
        }

        let statement = self.parse_block_statement();
        if statement.is_none() {
            return None;
        }

        let mut else_stmt: Option<Box<Statement>> = None;
        if self.peek_token == Token::Else {
            self.next_token();
            if !self.expect_peek(Token::Lbrace) {
                return None;
            }
            else_stmt = self.parse_block_statement().map(Box::new);
        }

        Some(Expression::If(Box::new(condition.unwrap()), Box::new(statement.unwrap()), else_stmt))
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        let mut statements = vec!();
        self.next_token();

        while self.cur_token != Token::Rbrace && self.cur_token !=  Token::EOF {
            let statement = self.parse_statement();
            if statement.is_some() {
                statements.push(statement.unwrap());
            }
            self.next_token();
        }
        Some(Statement::Block(statements))
    }

    fn parse_fn_literal(&mut self) -> Option<Expression> {
        if  !self.expect_peek(Token::Lparen) {
            return None;
        }

        let params = match self.parse_fn_params() {
            Some(params) => params,
            None => return None,
        };
        
        if !self.expect_peek(Token::Lbrace) {
            return None;
        }

        let body = match self.parse_block_statement() {
            Some(stmt) => Box::new(stmt),
            None => return None,
        };

        Some(Expression::FunctionLiteral(params, body))
    }

    fn parse_fn_params(&mut self) -> Option<Vec<Ident>> {
        self.next_token();

        let mut idents = vec!();
        
        match &self.cur_token {
            Token::Ident(value) => idents.push(Ident(value.clone())),
            Token::Rparen => return Some(idents),
            _ => return None,
        }

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            
            match &self.cur_token {
                Token::Ident(value) => idents.push(Ident(value.clone())),
                _ => return None,
            }
        }

        self.expect_peek(Token::Rparen);

        Some(idents)
    }

    fn parse_infix_expression(&mut self, op: InfOp, left: Expression) -> Option<Expression> {
        let precedence = self.cur_token.precedence();
        self.next_token();
        if let Some(right) = self.parse_expression(precedence) {
            return Some(Expression::Infix(Box::new(left), op, Box::new(right)));
        }
        None
    }

    fn parse_prefix_expression(&mut self, op: PrefOp) -> Option<Expression> {
        self.next_token();
        if let Some(right) = self.parse_expression(Precedence::Prefix) {
            return Some(Expression::Prefix(op, Box::new(right)));
        }
        None
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        Some(Statement::Expression(expr))
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let ident = match self.expect_peek_ident() {
            Some(ident) => ident,
            None => {
                return None;
            }
        };

        if self.peek_token != Token::Assign {
            return None;
        }

        self.next_token();
        self.next_token();
        if let Some(expr) = self.parse_expression(Precedence::Lowest) {
            self.next_token();
            return Some(Statement::Let(Ident(ident), expr));
        }

        None
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };
        self.expect_peek(Token::Semicolon);

        Some(Statement::Return(expr))
    }

    pub fn parse(&mut self) -> Ast {
        let mut program = Ast::new();

        while self.cur_token != Token::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }
}
