use environment::Environment;
use error::AppError;

mod error;
mod lexer;
mod parser;
mod environment;
mod evaluator;

use evaluator::{eval, Object};
use lexer::Lexer;
use parser::Parser;

fn start_interactive() -> Result<(), AppError> {
    let mut env = Environment::new();
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();

        if parser.errors.len() > 0 {
            println!("Errors: {:?}", parser.errors);
        }

        match eval(program, &mut env) {
            Object::Null => (),
            result => println!("{:?}", result),
        }
    }
}

fn main() -> Result<(), AppError> {
    // let input = "
    //     let five = 5;\n
    //     let ten = 10;\n
    //     let add = fn(x, y) {\n
    //         x + y;\n
    //     };\n
    //     let result = add(five, ten);
    //
    //     !-/*5;
    //     5< 10 > 5;
    //
    //     if (5 < 10) {
    //         return true;
    //     } else {
    //         return false;
    //     }
    //
    //     10 == 10;
    //     10 != 9;
    // ".to_string();
    //
    // let mut lexer = Lexer::new(input);
    // let mut token = lexer.next_token();
    // loop {
    //     println!("{:?}", token);
    //     if token == Token::EOF {
    //         break;
    //     }
    //     token = lexer.next_token();
    // }

    start_interactive()
}
