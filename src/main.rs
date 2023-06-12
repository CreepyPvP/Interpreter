struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}



#[derive(Debug, PartialEq)]
enum TokenType {
    Illegal,
    EOF,
    Ident,
    Int,
    Assign,
    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Function,
    Let,
}

impl TokenType {
    fn from_ident(ident: &str) -> Self {
        match ident {
            "fn" => Self::Function,
            "let" => Self::Let,
            _ => Self::Ident,
        }
    }
}

struct Token {
    token: TokenType,
    literal: String,
}

impl Lexer {
    fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: char::default(),
        };

        lexer.read_char();
        lexer
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic()
    }

    fn is_whitespace(ch: char) -> bool {
        ch.is_whitespace()
    }

    fn is_digit(ch: char) -> bool {
        ch.is_digit(10)
    }

    fn read_char(&mut self) {
        self.ch = match (self.read_position >= self.input.len()) {
            true => char::default(),
            false => self.input[self.read_position],
        };

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while Self::is_letter(self.ch) {
            self.read_char();
        }
        
        let slice = &self.input[start..self.position];
        slice.iter().collect()
    }

    fn read_number(&mut self) -> Result<usize,  {
        let start = self.position;
        while Self::is_digit(self.ch) {
            self.read_char();
        }

        let slice = &self.input[start..self.position];
        let str: String = slice.iter().collect();
        str.parse()
    }

    fn skip_whitespaces(&mut self) {
        while Self::is_whitespace(self.ch) {
            self.read_char();
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let (token, literal) = match self.ch {
            '=' => (TokenType::Assign, self.ch.to_string()),
            ';' => (TokenType::Semicolon, self.ch.to_string()),
            '(' => (TokenType::Lparen, self.ch.to_string()),
            ')' => (TokenType::Rparen, self.ch.to_string()),
            ',' => (TokenType::Comma, self.ch.to_string()),
            '+' => (TokenType::Plus, self.ch.to_string()),
            '{' => (TokenType::Rbrace, self.ch.to_string()),
            '}' => (TokenType::Lbrace, self.ch.to_string()),
            '\0' => (TokenType::EOF, "".to_string()),

            _ => {
                if Lexer::is_letter(self.ch) {
                    let ident = self.read_identifier();
                    return Token {token: TokenType::from_ident(&ident), literal: ident};
                } else {
                    (TokenType::Illegal, self.ch.to_string())
                }
            }
        };

        self.read_char();

        Token {token, literal}
    }

}

fn main() {
    let input = "
        let five = 5;\n
        let ten = 10;\n
        let add = fn(x, y) {\n
            x + y;\n
        };\n
        let result = add(five, ten);
    ".to_string();

    let mut lexer = Lexer::new(input);
    let mut token = lexer.next_token();
    loop {
        println!("{:?}, {}", token.token, token.literal);
        if token.token == TokenType::EOF {
            break;
        }
        token = lexer.next_token();
    }
}
