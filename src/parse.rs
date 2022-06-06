use crate::lex::{ Token, TokenKind, Span, };
use crate::compiler::{ FileId };

#[derive(Debug)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
}

#[derive(Debug)]
pub enum ExprKind {
	BinOp(BinOp, Box<Expr>, Box<Expr>),
	Int(i64),
}

#[derive(Debug)]
pub struct Expr {
	pub kind: ExprKind,
	pub span: Span,
}

pub struct Parser {
	file_id: FileId,
	tokens: Vec<Token>,
	idx: usize,
}

impl Parser {
	pub fn parse(file_id: FileId, tokens: Vec<Token>) -> Vec<Expr> {
		let mut parser = Parser {
			file_id,
			tokens,
			idx: 0,
		};

		let mut exprs = vec![]; 
		while !parser.at_eof() {
			exprs.push(parser.parse_expr());
		}

		exprs
	}

	fn parse_expr(&mut self) -> Expr {
		self.parse_bin_expr(0)
	}

	fn parse_bin_expr(&mut self, parent_precedence: u8) -> Expr {
		let mut left = self.parse_primary_expr();

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
			left = Expr {
				kind: ExprKind::BinOp(op, Box::new(left), Box::new(right)),
				span: span,
			};
		}

		left
	}

	fn parse_primary_expr(&mut self) -> Expr {
		let token = self.next();
		match token.kind {
			TokenKind::Int(i) => Expr {
				kind: ExprKind::Int(i),
				span: token.span.clone(),
			},
			_ => panic!(),
		}
	}

	fn at_eof(&self) -> bool {
		self.idx >= self.tokens.len() - 1
	}

	fn expect(&mut self, kind: TokenKind) -> &Token {
		if self.peek(0).kind != kind {
			panic!();
		}

		self.next()
	}

	fn next(&mut self) -> &Token {
		self.idx += 1;
		self.peek(-1)
	}

	fn peek(&self, offset: isize) -> &Token {
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

	pub fn to_bin_op(&self) -> BinOp {
		match self {
			TokenKind::Plus => BinOp::Add,
			TokenKind::Minus => BinOp::Sub,
			TokenKind::Star => BinOp::Mul,
			TokenKind::Slash => BinOp::Div,
			TokenKind::Percent => BinOp::Mod,
			_ => panic!("bad match"),
		}
	}
}