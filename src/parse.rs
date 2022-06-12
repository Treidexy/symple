use crate::lex::{ Token, TokenKind, };
use crate::compiler::*;

#[derive(Debug)]
pub struct ModuleST {
	pub funcs: Vec<FuncST>,
}

#[derive(Debug, Copy, Clone)]
pub enum BinOpST {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
}

#[derive(Debug)]
pub enum TypeSTKind {
	Error,
	None,
	Name(String),
}

#[derive(Debug)]
pub struct TypeST {
	pub kind: TypeSTKind,
	pub span: Span,
}

#[derive(Debug)]
pub enum ExprSTKind {
	Error,
	BinOp(BinOpST, Box<ExprST>, Box<ExprST>),
	Int(i64),
	Float(f64),
}

#[derive(Debug)]
pub struct ExprST {
	pub kind: ExprSTKind,
	pub span: Span,
}

#[derive(Debug)]
pub struct FuncST {
	pub ty: TypeST,
	pub name: String,
	pub stmts: Vec<StmtST>,
	pub span: Span,
}

#[derive(Debug)]
pub enum StmtST {
	Error(Span),
	Expr(ExprST),
	Func(FuncST),
}

pub struct Parser<'a> {
	file_id: FileId,
	tokens: &'a Vec<Token>,
	idx: usize,

	errors: Vec<Error>,
}

impl<'a> Parser<'a> {
	pub fn parse(file_id: FileId, tokens: &Vec<Token>) -> Result<ModuleST, Vec<Error>> {
		let mut parser = Parser {
			file_id,
			tokens,
			idx: 0,

			errors: vec![],
		};

		let mut funcs = vec![]; 
		while !parser.at_eof() {
			let func = parser.parse_func();
			if func.is_some() {
				funcs.push(func.unwrap());
			}
		}

		if parser.errors.is_empty() {
			Ok(ModuleST { funcs })
		} else {
			Err(parser.errors)
		}
	}

	fn parse_type(&mut self) -> TypeST {
		let span = self.peek(0).span;
		let name_token = self.expect_identifier();
		if name_token.is_none() {
			return TypeST {
				kind: TypeSTKind::Error,
				span,
			};
		}

		let name = name_token.unwrap().kind.text().unwrap();

		TypeST { kind: TypeSTKind::Name(name.clone()), span }
	}

	fn parse_func(&mut self) -> Option<FuncST> {
		let ty = if matches!(self.peek(1).kind, TokenKind::Identifier(_)) {
			self.parse_type()
		} else {
			TypeST {
				kind: TypeSTKind::None,
				span: Span::new(self.file_id),
			}
		};

		let name_token = self.expect_identifier();
		if name_token.is_none() {
			return None;
		}

		let name_token = name_token.unwrap();
		let name_span = name_token.span;
		let name = name_token.kind.text().unwrap();
		
		if self.expect(&TokenKind::LParen).is_none() { return None; }
		if self.expect(&TokenKind::RParen).is_none() { return None; }
		if self.expect(&TokenKind::LBrace).is_none() { return None; }
		
		let mut stmts = vec![];
		while !self.at_eof() && self.peek(0).kind != TokenKind::RBrace {
			stmts.push(self.parse_stmt());
		}

		let rbrace_span = self.next().span;

		let span = Span {
			file_id: self.file_id,
			start: name_span.start,
			end: rbrace_span.end,
		};
		
		Some(FuncST { ty, name: name.clone(), stmts, span, })
	}

	fn parse_stmt(&mut self) -> StmtST {
		let token = self.peek(0);
		match token {
			Token { kind: TokenKind::Identifier(_), .. } => {
				let func = self.parse_func();
				if func.is_some() {
					StmtST::Func(func.unwrap())
				} else {
					StmtST::Error(Span { file_id: self.file_id, start: token.span.start, end: self.peek(0).span.end, })
				}
			},
			_ => {
				let expr = self.parse_expr();
				if self.peek(0).kind != TokenKind::RBrace {
					if self.expect(&TokenKind::Semicolon).is_none() {
						self.prev();
					}
				}
				StmtST::Expr(expr)
			},
		}
	}

	fn parse_expr(&mut self) -> ExprST {
		self.parse_bin_expr(0)
	}

	fn parse_bin_expr(&mut self, parent_precedence: u8) -> ExprST {
		let mut left = self.parse_unary_expr();

		while !self.at_eof() {
			let op_token = self.peek(0);
			let precedence = op_token.kind.bin_precedence();
			if precedence <= parent_precedence {
				break;
			}

			let op = op_token.kind.to_bin_op();
			self.next();

			let right = self.parse_bin_expr(precedence);
			let span = Span {
				file_id: self.file_id,
				start: left.span.start,
				end: right.span.end,
			};
			left = ExprST {
				kind: ExprSTKind::BinOp(op, Box::new(left), Box::new(right)),
				span: span,
			};
		}

		left
	}

	fn parse_unary_expr(&mut self) -> ExprST {
		self.parse_primary_expr()
	}

	fn parse_primary_expr(&mut self) -> ExprST {
		let token = self.next();
		match token.kind {
			TokenKind::Int(i) => ExprST {
				kind: ExprSTKind::Int(i),
				span: token.span.clone(),
			},
			TokenKind::Float(x) => ExprST {
				kind: ExprSTKind::Float(x),
				span: token.span.clone(),
			},
			_ => self.report_expected_expr(token),
		}
	}

	fn report_wrong_token(&mut self, expected: &[&'static TokenKind], actual: &'a Token) {
		let expected_str = expected.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
		self.errors.push(Error(format!("expected {}, found {}", expected_str, actual.kind), actual.span));
	}

	fn report_expected_identifier(&mut self, actual: &'a Token) {
		self.errors.push(Error(format!("expected identifier, found {}", actual.kind), actual.span));
	}

	fn report_expected_identifier_or(&mut self, expected: &[&'static TokenKind], actual: &'a Token) {
		let expected_str = expected.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
		self.errors.push(Error(format!("expected identifier, {}, found {}", expected_str, actual.kind), actual.span));
	}

	fn report_expected_expr(&mut self, actual: &'a Token) -> ExprST {
		self.errors.push(Error(format!("expected expression, found {}", actual.kind), actual.span));
		ExprST {
			kind: ExprSTKind::Error,
			span: actual.span,
		}
	}

	fn at_eof(&self) -> bool {
		self.idx >= self.tokens.len() - 1
	}

	fn expect_identifier(&mut self) -> Option<&'a Token> {
		let token = self.next();
		if matches!(token, Token { kind: TokenKind::Identifier(_), .. }) {
			Some(token)
		} else {
			self.report_expected_identifier(token);
			None
		}
	}

	fn expect_identifier_or(&mut self, kinds: &[&'static TokenKind]) -> Option<&'a Token> {
		let token = self.next();
		if matches!(token, Token { kind: TokenKind::Identifier(_), .. }) {
			Some(token)
		} else {
			let token = self.next();
			for kind in kinds {
				if token.kind == **kind {
					return Some(token);
				}
			}

			self.report_expected_identifier_or(kinds, token);
			None
		}
	}

	fn expect(&mut self, kind: &'static TokenKind) -> Option<&'a Token> {
		let token = self.next();
		if token.kind != *kind {
			self.report_wrong_token(&[ kind ], token);
			None
		} else {
			Some(token)
		}
	}

	fn expect_one_of(&mut self, kinds: &[&'static TokenKind]) -> Option<&'a Token> {
		let token = self.next();
		for kind in kinds {
			if token.kind == **kind {
				return Some(token);
			}
		}

		self.report_wrong_token(kinds, token);
		None
	}

	fn prev(&mut self) -> &'a Token {
		self.idx -= 1;
		self.peek(1)
	}

	fn next(&mut self) -> &'a Token {
		self.idx += 1;
		self.peek(-1)
	}

	fn peek(&self, offset: isize) -> &'a Token {
		let pos = (self.idx as isize + offset) as usize;
		if pos >= self.tokens.len() {
			return &self.tokens[self.tokens.len() - 1];
		}

		&self.tokens[pos]
	}
}

impl TokenKind {
	pub fn bin_precedence(&self) -> u8 {
		match self {
			TokenKind::Plus => 1,
			TokenKind::Minus => 1,
			TokenKind::Star => 2,
			TokenKind::Slash => 2,
			TokenKind::Percent => 2,
			_ => 0,
		}
	}

	pub fn to_bin_op(&self) -> BinOpST {
		match self {
			TokenKind::Plus => BinOpST::Add,
			TokenKind::Minus => BinOpST::Sub,
			TokenKind::Star => BinOpST::Mul,
			TokenKind::Slash => BinOpST::Div,
			TokenKind::Percent => BinOpST::Mod,
			_ => unreachable!(),
		}
	}

	fn text(&self) -> Option<&String> {
		match self {
			TokenKind::Identifier(name) => Some(name),
			_ => None,
		}
	}
}