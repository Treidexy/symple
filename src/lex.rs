mod lexer;
mod lex_result;
mod token;
mod token_kind;

use std::str::Chars;

use lexer::Lexer;
pub use token::Token;
pub use token_kind::TokenKind;
pub use lex_result::LexResult;

#[allow(dead_code)]
pub fn lex<'a>(chars: Chars<'a>) -> LexResult {
	let mut lexer = Lexer::new(chars);
	lexer.lex()
}