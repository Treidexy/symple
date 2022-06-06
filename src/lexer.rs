#[derive(Debug)]
pub struct Span {
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

#[derive(Debug)]
pub enum TokenKind {
	Unknown,

	Identifier(String),
	Int(i64),

	Equal,

	Semicolon,
}

#[derive(Debug)]
pub struct Token {
	pub flags: TokenFlags,
	pub kind: TokenKind,
	pub span: Span,
}

pub struct Lexer {
	src: String,
	idx: usize,
	flags: TokenFlags,
}

impl Lexer{
	pub fn new(src: String) -> Self {
		let mut flags = TokenFlags::new();
		flags.first_of_line = true;

		Self {
			src,
			idx: 0,
			flags,
		}
	}

	pub fn lex(&mut self, tokens: &mut Vec<Token>) {
		while !self.at_eof() {
			if self.peek(0).is_ascii_whitespace() {
				self.next();
				continue;
			}

			match self.peek(0) {
				b'=' => {
					tokens.push(self.make_little_token(TokenKind::Equal));
				},
				b';' => {
					tokens.push(self.make_little_token(TokenKind::Semicolon));
				},
				_ => {

					if self.peek(0).is_ascii_digit() {
						self.lex_number(tokens);
						continue;
					}
		
					if self.peek(0).is_ascii_alphabetic() {
						self.lex_identifier(tokens);
						continue;
					}
		
					tokens.push(self.make_little_token(TokenKind::Unknown));
					continue;
				},
			}
		}
	}

	fn lex_number(&mut self, tokens: &mut Vec<Token>) {
		let start = self.idx;

		while !self.at_eof() && self.peek(0).is_ascii_digit() {
			self.next();
		}

		let end = self.idx;
		let span = Span { start, end };
		let number = self.src[start..end].parse::<i64>().unwrap();
		tokens.push(self.make_token(TokenKind::Int(number), span));
	}

	fn lex_identifier(&mut self, tokens: &mut Vec<Token>) {
		let start = self.idx;

		while !self.at_eof() && self.peek(0).is_ascii_alphanumeric() {
			self.next();
		}

		let end = self.idx;
		let span = Span { start, end };
		let identifier = self.src[start..end].to_owned();
		tokens.push(self.make_token(TokenKind::Identifier(identifier), span));
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
			start: self.idx,
			end: self.idx + 1,
		};

		self.next();
		self.make_token(kind, span)
	}
}