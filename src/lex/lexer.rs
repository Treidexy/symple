use std::{str::Chars, hash::Hash};

use super::{Token, TokenKind, LexResult};

pub struct Lexer<'a> {
	chars: Chars<'a>,
	curr: char, // current character
	pos: usize,
	toks: Vec<Token>,
}

impl<'a> Lexer<'a> {
	pub fn new(mut chars: Chars<'a>) -> Self {
		let curr = chars.next().unwrap_or('\0');

		Self {
			chars: chars,
			curr: curr,
			pos: 0,
			toks: Vec::new(),
		}
	}

	pub fn lex(mut self) -> LexResult {
		while !self.at_eof() {
			self.lex_tok();
		}

		self.toks.push(Token::new(TokenKind::Eof, 0));
		LexResult { tokens: self.toks, }
	}

	fn lex_tok(&mut self) {
		self.pos = 0;

		if self.curr.is_whitespace() {
			while self.curr.is_whitespace() {
				self.next();
			}

			self.toks.push(Token::new(TokenKind::Whitespace, self.pos));
		} else {
			let c = self.next();
			match c {
				'a'..='z' | 'A'..='Z' | '_' => self.lex_name(c),

				// Literal
				'0'..='9' => self.lex_number(),
				'"' => self.lex_string(),
				'\'' => self.lex_char(),

				// Arithmetic
				'+' => self.push(TokenKind::Plus),
				'-' => {
					if self.curr == '>' {
						self.next();
						self.push(TokenKind::RArrow);
					} else {
						self.push(TokenKind::Minus);
					}
				}
				'*' => self.push(TokenKind::Star),
				'/' => {
					if self.curr == '/' {
						self.next();
						self.lex_line_comment();
					} else if self.curr == '*' {
						self.next();
						self.lex_block_comment();
					} else {
						self.push(TokenKind::Slash);
					}
				},
				'%' => self.push(TokenKind::Percent),

				// Logic
				'^' => self.push(TokenKind::Carot),
				'|' => self.push(TokenKind::Pipe),
				'&' => self.push(TokenKind::Ampersand),
				'=' => self.push(TokenKind::Equal),
				'!' => self.push(TokenKind::Bang),

				// Comparison or Grouping
				'<' => {
					if self.curr == '=' {
						self.next();
						self.push(TokenKind::LChevronEqual);
					} else if self.curr == '-' {
						self.next();
						self.push(TokenKind::LArrow);
					} else {
						self.push(TokenKind::LChevron);
					}
				}
				'>' => {
					if self.curr == '=' {
						self.next();
						self.push(TokenKind::RChevronEqual);
					} else {
						self.push(TokenKind::RChevron);
					}
				}

				// Grouping
				'(' => self.push(TokenKind::LParen),
				')' => self.push(TokenKind::RParen),
				'{' => self.push(TokenKind::LBrace),
				'}' => self.push(TokenKind::RBrace),
				'[' => self.push(TokenKind::LBracket),
				']' => self.push(TokenKind::RBracket),

				// Punctuation
				',' => self.push(TokenKind::Comma),
				':' => {
					if self.curr == ':' {
						self.next();
						self.push(TokenKind::ColonColon);
					} else {
						self.push(TokenKind::Colon);
					}
				}
				';' => self.push(TokenKind::Semicolon),
				'.' => self.push(TokenKind::Dot),

				// Misc
				'$' => self.push(TokenKind::Dollar),

				_ => self.push(TokenKind::Unknown),
			}
		}
	}

	fn lex_name(&mut self, c: char) {
		let mut name = String::new();
		name.push(c);

		while self.curr.is_alphanumeric() || self.curr == '_' {
			name.push(self.next());
		}

		let kind = match name.as_str() {
			// Literal
			"true" => TokenKind::TrueKw,
			"false" => TokenKind::FalseKw,

			// Types
			"public" => TokenKind::PublicKw,
			"mut" => TokenKind::MutKw,
			"let" => TokenKind::LetKw,
			"const" => TokenKind::ConstKw,
			"fn" => TokenKind::FnKw,
			"sys" => TokenKind::SysKw,

			// Control Flow
			"if" => TokenKind::IfKw,
			"else" => TokenKind::ElseKw,
			"loop" => TokenKind::LoopKw,
			"for" => TokenKind::ForKw,

			// Contract
			"type" => TokenKind::TypeKw,
			"trait" => TokenKind::TraitKw,

			// Implementation
			"struct" => TokenKind::StructKw,
			"enum" => TokenKind::EnumKw,
			"impl" => TokenKind::ImplKw,
			_ => TokenKind::Name,
		};

		self.push(kind);
	}

	fn lex_number(&mut self) {
		let mut dot_count = 0;

		while self.curr.is_numeric() || self.curr == '.' {
			if self.curr == '.' {
				dot_count += 1;
			}

			self.next();
		}

		let kind = match dot_count {
			0 => TokenKind::Int,
			1 => TokenKind::Float,
			_ => TokenKind::Plex,
		};

		self.push(kind);
	}

	fn lex_string(&mut self) {
		let mut was_escape = self.curr == '\\';
		while self.curr != '"' && !was_escape {
			if self.at_eof() {
				println!("Unterminated string");
				break;
			}

			self.next();
			was_escape = !was_escape && self.curr == '\\';
		}

		self.push(TokenKind::String);
	}

	fn lex_char(&mut self) {
		let mut was_escape = self.curr == '\\';
		while self.curr != '\'' && !was_escape {
			if self.at_eof() {
				println!("Unterminated char");
				break;
			}

			self.next();
			was_escape = !was_escape && self.curr == '\\';
		}

		self.push(TokenKind::Char);
	}

	fn lex_line_comment(&mut self) {
		while self.curr != '\n' && !self.at_eof() {
			self.next();
		}

		self.push(TokenKind::LineComment);
	}

	fn lex_block_comment(&mut self) {
		let mut depth = 1usize;

		while depth > 0 && !self.at_eof() {
			if self.curr == '/' && self.next() == '*' {
				depth += 1;
			} else if self.curr == '*' && self.next() == '/' {
				depth -= 1;
			}

			self.next();
		}

		self.push(TokenKind::BlockComment);
	}

	fn at_eof(&self) -> bool {
		self.curr == '\0'
	}

	fn push(&mut self, kind: TokenKind) {
		self.toks.push(Token::new(kind, self.pos));
	}

	fn next(&mut self) -> char {
		let c = self.curr;
		self.curr = self.chars.next().unwrap_or('\0');
		self.pos += 1;
		c
	}
}