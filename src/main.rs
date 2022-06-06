mod lex;
mod parse;
mod compiler;

use lex::{ Lexer, };
use parse::{ Parser, };
use compiler::{ FileId, };

fn main() {
    let file_id: FileId = 0;
    let src = std::fs::read_to_string("samples/test.sy").unwrap();

    let tokens = Lexer::lex(file_id, src.clone());
    for token in &tokens {
        println!("{:?}", token);
    }

    println!("");

    let exprs = Parser::parse(file_id, tokens);
    for expr in &exprs {
        println!("{:?}", expr);
    }
}