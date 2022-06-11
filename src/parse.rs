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
	pub name: String,
	pub stmts: Vec<StmtST>,
	pub span: Span,
}

#[derive(Debug)]
pub enum StmtST {
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
			funcs.push(parser.parse_func());
		}

		if parser.errors.is_empty() {
			Ok(ModuleST { funcs })
		} else {
			Err(parser.errors)
		}
	}

	fn parse_func(&mut self) -> FuncST {
		let name_token = self.next();
		let name_span = name_token.span;
		let name = if let TokenKind::Identifier(name) = &name_token.kind {
			name.to_string()
		} else {
			self.report_expected_identifier(name_token);
			String::new()
		};
		
		self.expect(&TokenKind::LParen);
		self.expect(&TokenKind::RParen);
		self.expect(&TokenKind::LBrace);
		
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
		
		FuncST { name, stmts, span, }
	}

	fn parse_stmt(&mut self) -> StmtST {
		let expr = self.parse_expr();
		self.expect(&TokenKind::Semicolon);
		StmtST::Expr(expr)
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

	fn at_eof(&self) -> bool {
		self.idx >= self.tokens.len() - 1
	}

	fn expect(&mut self, kind: &'static TokenKind) -> &'a Token {
		let token = self.next();
		if token.kind != *kind {
			self.report_wrong_token(kind, token);
		}

		token
	}

	fn report_wrong_token(&mut self, expected: &'static TokenKind, actual: &'a Token) {
		self.errors.push(Error(format!("expected {}, found {}", expected, actual.kind), actual.span));
	}

	fn report_expected_identifier(&mut self, token: &'a Token) {
		self.errors.push(Error(format!("expected identifier, found {}", token.kind), token.span));
	}

	fn report_expected_expr(&mut self, token: &'a Token) -> ExprST {
		self.errors.push(Error(format!("expected expression, found {}", token.kind), token.span));
		ExprST {
			kind: ExprSTKind::Error,
			span: token.span,
		}
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
}