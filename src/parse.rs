use crate::lex::{ Token, TokenKind, };
use crate::compiler::{ FileId, Span, };

#[derive(Debug)]
pub struct ModuleST {
	pub stmts: Vec<StmtST>,
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
pub enum ExprSTKind {
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
pub enum StmtST {
	Expr(ExprST),
}

pub struct Parser<'a> {
	file_id: FileId,
	tokens: &'a Vec<Token>,
	idx: usize,
}

impl<'a> Parser<'a> {
	pub fn parse(file_id: FileId, tokens: &Vec<Token>) -> ModuleST {
		let mut parser = Parser {
			file_id,
			tokens,
			idx: 0,
		};

		let mut stmts = vec![]; 
		while !parser.at_eof() {
			stmts.push(parser.parse_stmt());
		}

		ModuleST { stmts }
	}

	fn parse_stmt(&mut self) -> StmtST {
		let expr = self.parse_expr();
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
			_ => panic!(),
		}
	}

	fn at_eof(&self) -> bool {
		self.idx >= self.tokens.len() - 1
	}

	#[allow(dead_code)]
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

	pub fn to_bin_op(&self) -> BinOpST {
		match self {
			TokenKind::Plus => BinOpST::Add,
			TokenKind::Minus => BinOpST::Sub,
			TokenKind::Star => BinOpST::Mul,
			TokenKind::Slash => BinOpST::Div,
			TokenKind::Percent => BinOpST::Mod,
			_ => panic!("bad match"),
		}
	}
}