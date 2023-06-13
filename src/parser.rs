use crate::lexer::{Token, Lexer};

enum Statement {
    Let(Identifier, Expression)
}

struct  Identifier {
    value: String,
}

enum Expression {
    Identifier(Identifier)
}


struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Parser { lexer, cur_token: Token::EOF, peek_token: Token::EOF };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next().unwrap_or(Token::EOF));
    }
}

