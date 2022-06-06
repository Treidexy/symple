use crate::lex::{ Token, Span, };

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
	Null,

	BinOp(BinOp, Box<Expr>, Box<Expr>),
	Int(i64),
}

#[derive(Debug)]
pub struct Expr {
	pub kind: ExprKind,
	pub span: Span,
}

pub struct Parser {
	pub tokens: Vec<Token>,
}