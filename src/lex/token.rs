use super::TokenKind;

pub struct Token {
	kind: TokenKind,
	len: usize,
}

impl Token {
	pub fn new(kind: TokenKind, len: usize) -> Self {
		Self { kind, len }
	}

	pub fn kind(&self) -> TokenKind {
		self.kind
	}

	pub fn len(&self) -> usize {
		self.len
	}
}