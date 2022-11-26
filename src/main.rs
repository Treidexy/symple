mod lex;
mod debug;

use std::fs;

use lex::{lex, TokenKind};

fn main() {
    let src = fs::read_to_string("sy/test.sy").unwrap();
    let chars = src.chars();
    let lex_result = lex(chars);
    for token in lex_result.tokens {
        if token.kind() == TokenKind::Unknown {
            panic!("unkown token");
        }
    }
}
