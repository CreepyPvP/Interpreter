use error::AppError;

mod error;
mod lexer;
mod parser;

use lexer::Lexer;


fn start_interactive() -> Result<(), AppError> {
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input)?;

        let lexer = Lexer::new(input);
        for token in lexer {
            println!("{:?}", token);
        }
    }
}

fn main() -> Result<(), AppError>{
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
