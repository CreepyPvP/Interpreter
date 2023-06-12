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

impl Token {
    fn from_ident(ident: String) -> Self {
        match ident.as_str() {
            "fn" => Self::Function,
            "let" => Self::Let,
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
            '=' => Token::Assign,
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
