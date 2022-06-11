use crate::lex::{ Token, TokenKind, };

pub type FileId = usize;

#[derive(Debug, Copy, Clone)]
pub struct Span {
	pub file_id: FileId,
	pub start: usize,
	pub end: usize,
}

#[derive(Debug)]
pub enum ParseError<'a> {
	WrongToken(&'static TokenKind, &'a Token),
	ExpectedIdentifier(&'a Token),
	ExpectedExpr(&'a Token),
}