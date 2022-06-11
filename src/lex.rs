use crate::compiler::{ FileId, Span };

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
	Float(f64),

	Plus,
	Minus,
	Star,
	Slash,
	Percent,

	LParen,
	RParen,
	LBrace,
	RBrace,

	Semicolon,
}

#[derive(Debug)]
pub struct Token {
	pub flags: TokenFlags,
	pub kind: TokenKind,
	pub span: Span,
}

pub struct Lexer<'a> {
	file_id: FileId,
	src: &'a String,
	idx: usize,
	flags: TokenFlags,
}

impl<'a> Lexer<'a> {
	pub fn lex(file_id: FileId, src: &String) -> Vec<Token> {
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
			b'(' => {
				self.make_little_token(TokenKind::LParen)
			},
			b')' => {
				self.make_little_token(TokenKind::RParen)
			},
			b'{' => {
				self.make_little_token(TokenKind::LBrace)
			},
			b'}' => {
				self.make_little_token(TokenKind::RBrace)
			},
			b';' => {
				self.make_little_token(TokenKind::Semicolon)
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

		if self.peek(0) == b'-' {
			self.next();
		}

		while !self.at_eof() && self.peek(0).is_ascii_digit() {
			self.next();
		}

		if self.peek(0) == b'.' {
			self.next();

			while !self.at_eof() && self.peek(0).is_ascii_digit() {
				self.next();
			}

			let end = self.idx;
			let span = Span { file_id: self.file_id, start, end };
			let number = self.src[start..end].parse::<f64>().unwrap();
			self.make_token(TokenKind::Float(number), span)
		} else {
			let end = self.idx;
			let span = Span { file_id: self.file_id, start, end };
			let number = self.src[start..end].parse::<i64>().unwrap();
			self.make_token(TokenKind::Int(number), span)
		}
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

impl TokenKind {
	pub fn to_error_string(&self) -> String {
		match *self {
			TokenKind::Unknown => "unknown".to_owned(),
			TokenKind::Plus => "`+`".to_owned(),
			TokenKind::Minus => "`-`".to_owned(),
			TokenKind::Star => "`*`".to_owned(),
			TokenKind::Slash => "`/`".to_owned(),
			TokenKind::Percent => "`%`".to_owned(),
			TokenKind::Eof => "EOF".to_owned(),
			TokenKind::LParen => "`(`".to_owned(),
			TokenKind::RParen => "`)`".to_owned(),
			TokenKind::LBrace => "`{`".to_owned(),
			TokenKind::RBrace => "`}`".to_owned(),
			TokenKind::Semicolon => "`;`".to_owned(),
			TokenKind::Int(x) => format!("integer `{}`", x),
			TokenKind::Float(x) => format!("float `{}`", x),
			TokenKind::Identifier(ref s) => format!("identifier `{}`", s),
		}
	}
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(self.to_error_string().as_str())
    }
}