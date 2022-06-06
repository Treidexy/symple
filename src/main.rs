mod lex;

use lex::{ Lexer, };

fn main() {
    let src = std::fs::read_to_string("samples/test.sy").unwrap();
    let tokens = Lexer::lex(src.clone());
    for token in tokens {
        println!("{:?}", token);
    }
}