use crate::compiler::{ FileId };

#[derive(Debug, Clone)]
pub struct Span {
	pub file_id: FileId,
	pub start: usize,
	pub end: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct TokenFlags {
	first_of_line: bool,
	leading_space: bool,
}

impl TokenFlags {
	pub fn new() -> Self {
		TokenFlags {
			first_of_line: false,
			leading_space: false,
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
	Unknown,
	Eof,

	Identifier(String),
	Int(i64),

	Plus,
	Minus,
	Star,
	Slash,
	Percent,
}

#[derive(Debug)]
pub struct Token {
	pub flags: TokenFlags,
	pub kind: TokenKind,
	pub span: Span,
}

pub struct Lexer {
	file_id: FileId,
	src: String,
	idx: usize,
	flags: TokenFlags,
}

impl Lexer {
	pub fn lex(file_id: FileId, src: String) -> Vec<Token> {
		let mut lexer = Lexer {
			file_id,
			src,
			idx: 0,
			flags: TokenFlags::new(),
		};

		let mut tokens = vec![];
		while !lexer.at_eof() {
			lexer.skip_whitespace();
			tokens.push(lexer.lex_token());
		}

		tokens.push(lexer.lex_token());

		tokens
	}

	fn skip_whitespace(&mut self) {
		while !self.at_eof() && self.peek(0).is_ascii_whitespace() {
			self.next();
		}
	}

	fn lex_token(&mut self) -> Token {
		match self.peek(0) {
			b'+' => {
				self.make_little_token(TokenKind::Plus)
			},
			b'-' => {
				self.make_little_token(TokenKind::Minus)
			},
			b'*' => {
				self.make_little_token(TokenKind::Star)
			},
			b'/' => {
				self.make_little_token(TokenKind::Slash)
			},
			b'%' => {
				self.make_little_token(TokenKind::Percent)
			},
			b'\0' => {
				self.make_little_token(TokenKind::Eof)
			},
			_ => {
				if self.peek(0).is_ascii_digit() {
					self.lex_number()
				} else if self.peek(0).is_ascii_alphabetic() {
					self.lex_identifier()
				} else {
					self.make_little_token(TokenKind::Unknown)
				}
			},
		}
	}

	fn lex_number(&mut self) -> Token {
		let start = self.idx;

		while !self.at_eof() && self.peek(0).is_ascii_digit() {
			self.next();
		}

		let end = self.idx;
		let span = Span { file_id: self.file_id, start, end };
		let number = self.src[start..end].parse::<i64>().unwrap();
		self.make_token(TokenKind::Int(number), span)
	}

	fn lex_identifier(&mut self) -> Token {
		let start = self.idx;

		while !self.at_eof() && self.peek(0).is_ascii_alphanumeric() {
			self.next();
		}

		let end = self.idx;
		let span = Span { file_id: self.file_id, start, end };
		let identifier = self.src[start..end].to_owned();
		self.make_token(TokenKind::Identifier(identifier), span)
	}

	fn at_eof(&self) -> bool {
		self.idx >= self.src.len()
	}

	fn next(&mut self) -> u8 {
		let c = self.peek(0);

		if c == b'\n' {
			self.flags.first_of_line = true;
		} else if c.is_ascii_whitespace() {
			self.flags.leading_space = true;
		}

		self.idx += 1;
		c
	}

	fn peek(&self, offset: isize) -> u8 {
		let pos = self.idx as isize + offset;
		if pos < 0 || pos >= self.src.len() as isize {
			return 0;
		}

		self.src.as_bytes()[pos as usize]
	}

	fn make_token(&mut self, kind: TokenKind, span: Span) -> Token {
		let token = Token {
			kind,
			flags: self.flags,
			span,
		};

		self.flags = TokenFlags::new();
		token
	}

	// makes a token of len 1
	fn make_little_token(&mut self, kind: TokenKind) -> Token {
		let span = Span {
			file_id: self.file_id,
			start: self.idx,
			end: self.idx + 1,
		};

		self.next();
		self.make_token(kind, span)
	}
}