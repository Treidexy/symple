mod lexer;

use lexer::{ Lexer };

fn main() {
    let mut lexer = Lexer::new("let x = 1;".to_owned());
    let mut tokens = vec![];
    lexer.lex(&mut tokens);
    for token in tokens {
        println!("{:?}", token);
    }
}
