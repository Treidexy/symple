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

	messages: Vec<Message>,
	has_error: bool,
	has_mistake: bool,

	// mistake helping
}

pub struct ParseResult {
	pub module: ModuleST,
	pub messages: Vec<Message>,
	pub has_error: bool,
	pub has_mistake: bool,
}

impl<'a> Parser<'a> {
	pub fn parse(file_id: FileId, tokens: &Vec<Token>) -> ParseResult {
		let mut parser = Parser {
			file_id,
			tokens,
			idx: 0,

			messages: vec![],
			has_error: false,
			has_mistake: false,
		};

		let mut funcs = vec![]; 
		while !parser.at_eof() {
			let func = parser.parse_func();
			if func.is_some() {
				funcs.push(func.unwrap());
			} else {
				parser.next();
			}
		}

		ParseResult {
			module: ModuleST { funcs },
			messages: parser.messages,
			has_error: parser.has_error,
			has_mistake: parser.has_mistake,
		}
	}

	fn parse_type(&mut self) -> TypeST {
		let span = self.peek(0).span;
		let name_token = self.peek(0);
		if !matches!(name_token.kind, TokenKind::Identifier(_)) {
			self.report_error(format!("expected identifier for type name"), span);
			return TypeST {
				kind: TypeSTKind::Error,
				span,
			};
		} else {
			self.next();
		}

		let name = name_token.kind.text().unwrap();

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

		let name_token = self.peek(0);
		if !matches!(name_token.kind, TokenKind::Identifier(_)) {
			self.report_error( format!("expected identifier for function name"), name_token.span);
			return None;
		} else {
			self.next();
		}

		let name_span = name_token.span;
		let name = name_token.kind.text().unwrap();
		
		if self.peek(0).kind != TokenKind::LParen {
			self.report_error( format!("expected `(` for function arguments"), self.peek(-1).span.gap(self.peek(0).span));
			return None;
		} else {
			self.next();
		}

		if self.peek(0).kind != TokenKind::RParen {
			if self.peek(0).kind == TokenKind::LBrace {
				println!("{}, {}, {}", self.peek(-1).kind, self.peek(0).kind, self.peek(1).kind);
				self.report_mistake(format!("expected `)` to close function arguments"), format!("adding them for you"), self.peek(-1).span.gap(self.peek(0).span));
				self.next();
			} else {
				self.report_error( format!("expected `)` to close function arguments"), self.peek(-1).span.gap(self.peek(0).span));
				return None;
			}
		} else {
			self.next();
			if self.peek(0).kind != TokenKind::LBrace {
				self.report_error(format!("expected `{{` to open function"), self.peek(-1).span.gap(self.peek(0).span));
			} else {
				self.next();
			}
		}
		
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
					if self.peek(0).kind != TokenKind::Semicolon {
						self.report_mistake(format!("expected `;`"), format!("ignoring"), self.peek(-1).span);
					} else {
						self.next();
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
			_ => {
				self.report_mistake(format!("expected expression"), format!("using `null` instead"), token.span);

				ExprST {
					kind: ExprSTKind::Error,
					span: token.span,
				}
			},
		}
	}

	fn at_eof(&self) -> bool {
		self.idx >= self.tokens.len() - 1
	}

	fn report_error(&mut self, msg: String, span: Span) {
		self.messages.push(Message {
			kind: MessageKind::Error,
			span: span,
			text: msg,
		});

		self.has_error = true;
	}

	fn report_mistake(&mut self, err: String, fix: String, span: Span) {
		self.messages.push(Message {
			kind: MessageKind::Mistake,
			span: span,
			text: format!("{}\u{001b}[93m; {}", err, fix),
		});

		self.has_mistake = true;
	}

	fn report_warning(&mut self, msg: String, span: Span) {
		self.messages.push(Message {
			kind: MessageKind::Warning,
			span: span,
			text: msg,
		});
	}
	
	fn report_note(&mut self, msg: String, span: Span) {
		self.messages.push(Message {
			kind: MessageKind::Note,
			span: span,
			text: msg,
		});
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