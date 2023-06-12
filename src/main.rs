struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}



#[derive(Debug, PartialEq)]
enum Token {
    Illegal(String),
    EOF,
    Ident(String),
    Int(usize),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Comma,
    Eq,
    NotEq,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
}

impl Token {
    fn from_ident(ident: String) -> Self {
        match ident.as_str() {
            "fn" => Self::Function,
            "let" => Self::Let,
            "true" => Self::True,
            "false" => Self::False,
            "if" => Self::If,
            "else" => Self::Else,
            "return" => Self::Return,
            _ => Self::Ident(ident),
        }
    }
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

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            char::default()
        } else {
            self.input[self.read_position]
        }
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while Self::is_letter(self.ch) {
            self.read_char();
        }
        
        let slice = &self.input[start..self.position];
        slice.iter().collect()
    }

    fn read_number(&mut self) -> usize  {
        let start = self.position;
        while Self::is_digit(self.ch) {
            self.read_char();
        }

        let slice = &self.input[start..self.position];
        let str: String = slice.iter().collect();
        str.parse().unwrap()
    }


    fn skip_whitespaces(&mut self) {
        while Self::is_whitespace(self.ch) {
            self.read_char();
        }
    }


    fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let token = match self.ch {
            '=' if self.peek_char() == '=' => {
                self.read_char();
                Token::Eq
            }
            '='  => Token::Assign,
            '-' => Token::Minus,
            '!' if self.peek_char() == '=' => {
                self.read_char();
                Token::NotEq
            }
            '!' => Token::Bang,
            '*' => Token::Asterisk,
            '/' => Token::Slash, 
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '\0' => Token::EOF,

            _ => {
                if Lexer::is_letter(self.ch) {
                    let ident = self.read_identifier();
                    return Token::from_ident(ident);
                } else if Lexer::is_digit(self.ch) {
                    let num = self.read_number();
                    return Token::Int(num);
                } else {
                    Token::Illegal(self.ch.to_string())
                }
            }
        };

        self.read_char();

        token
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

        !-/*5;
        5< 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
    ".to_string();

    let mut lexer = Lexer::new(input);
    let mut token = lexer.next_token();
    loop {
        println!("{:?}", token);
        if token == Token::EOF {
            break;
        }
        token = lexer.next_token();
    }
}
