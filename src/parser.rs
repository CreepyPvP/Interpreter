use crate::{
    error::AppError,
    lexer::{Lexer, Token},
};

#[derive(Debug)]
pub struct Ast {
    statements: Vec<Statement>,
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

#[derive(Debug)]
enum Statement {
    Let(Ident, Expression),
}

#[derive(Debug)]
struct Ident(String);

#[derive(Debug)]
enum Expression {
    Identifier(Ident),
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

    fn expect_peek<V, T>(&mut self, validator: V) -> Option<T>
    where
        V: FnOnce(&Token) -> Option<T>,
    {
        let result = validator(&self.peek_token);
        if result.is_some() {
            self.next_token();
        }
        result
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn peek_error(&mut self, expected: Token) {
        self.errors.push(AppError::ParserError(format!(
            "expected token {:?}, got token {:?}",
            expected, self.peek_token
        )));
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let ident = self.expect_peek(|tok| match tok {
            Token::Ident(ident) => Some(ident.to_owned()),
            _ => None,
        });

        let ident = match ident {
            Some(ident) => ident,
            None => {
                self.peek_error(Token::Ident("something".to_string()));
                return None;
            }
        };

        if self.peek_token != Token::Assign {
            return None;
        }

        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Let(
            Ident(ident),
            Expression::Identifier(Ident("hello world".to_string())),
        ))
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
