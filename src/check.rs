use crate::compiler::{ Span };
use crate::parse::{ ModuleST, StmtST, ExprST, ExprSTKind, BinOpST };

pub type TypeId = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum BuiltinType {
	Int(u32),
	UInt(u32),
	Float(u32),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
	Builtin(BuiltinType),
}

#[derive(Debug)]
pub struct Module {
	pub types: Vec<Type>,
	pub stmts: Vec<Stmt>,
}

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	SDiv,
	SMod,
	UDiv,
	UMod,
	FAdd,
	FSub,
	FMul,
	FDiv,
	FMod,
}

#[derive(Debug)]
pub enum ExprKind {
	BinOp(BinOp, Box<Expr>, Box<Expr>),
	Int(i64),
	Float(f64),
}


#[derive(Debug)]
pub struct Expr {
	pub kind: ExprKind,
	pub type_id: TypeId,
	pub span: Span,
}

#[derive(Debug)]
pub enum Stmt {
	Expr(Expr),
}

pub struct Checker {
	module: Module,
}

impl Checker {
	pub fn check(module_st: &ModuleST) -> Module {
		let mut checker = Checker {
			module: Module {
				types: vec![],
				stmts: vec![],
			},
		};

		for stmt_st in &module_st.stmts {
			let checked = checker.check_stmt(stmt_st);
			checker.module.stmts.push(checked);
		}

		checker.module
	}

	fn check_stmt(&mut self, ast: &StmtST) -> Stmt {
		match ast {
			StmtST::Expr(expr_st) => {
				let checked = self.check_expr(expr_st);
				Stmt::Expr(checked)
			}
		}
	}

	fn check_expr(&mut self, ast: &ExprST) -> Expr {
		match ast.kind {
			ExprSTKind::BinOp(op, ref left, ref right) => {
				let left = self.check_expr(left);
				let right = self.check_expr(right);
				let span = ast.span;
				let type_id = self.resolve_bin_op_type(op, left.type_id, right.type_id);
				let op = self.bin_op_st_to_bin_op(op, type_id);

				Expr {
					kind: ExprKind::BinOp(op, Box::new(left), Box::new(right)),
					type_id,
					span,
				}
			},
			ExprSTKind::Int(value) => {
				let span = ast.span;
				let _type = Type::Builtin(BuiltinType::Int(64));
				let type_id = self.module.find_or_add_type(_type);

				Expr {
					kind: ExprKind::Int(value),
					type_id,
					span,
				}
			},
			ExprSTKind::Float(value) => {
				let span = ast.span;
				let _type = Type::Builtin(BuiltinType::Float(64));
				let type_id = self.module.find_or_add_type(_type);

				Expr {
					kind: ExprKind::Float(value),
					type_id,
					span,
				}
			},
		}
	}

	fn resolve_bin_op_type(&mut self, _op: BinOpST, left_id: TypeId, right_id: TypeId) -> TypeId {
		let left = &self.module.types[left_id];
		let right = &self.module.types[right_id];

		if left.is_float() {
			left_id
		} else if right.is_float() {
			right_id
		} else {
			left_id
		}
	}

	fn bin_op_st_to_bin_op(&self, op: BinOpST, type_id: TypeId) -> BinOp {
		let ty = &self.module.types[type_id];
		
		if ty.is_float() {
			match op {
				BinOpST::Add => BinOp::FAdd,
				BinOpST::Sub => BinOp::FSub,
				BinOpST::Mul => BinOp::FMul,
				BinOpST::Div => BinOp::FDiv,
				BinOpST::Mod => BinOp::FMod,
			}
		} else {
			match op {
				BinOpST::Add => BinOp::Add,
				BinOpST::Sub => BinOp::Sub,
				BinOpST::Mul => BinOp::Mul,
				BinOpST::Div => if ty.is_signed_int() { BinOp::SDiv } else { BinOp::UDiv },
				BinOpST::Mod => if ty.is_signed_int() { BinOp::SMod } else { BinOp::UMod },
			}
		}
	}
}

impl Module {
	fn find_or_add_type(&mut self, _type: Type) -> TypeId {
		for (i, t) in self.types.iter().enumerate() {
			if *t == _type {
				return i;
			}
		}

		let type_id = self.types.len();
		self.types.push(_type);
		type_id
	}
}

impl Type {
	pub fn is_signed_int(&self) -> bool {
		match self {
			Type::Builtin(BuiltinType::Int(_)) => true,
			_ => false,
		}
	}

	pub fn is_int(&self) -> bool {
		match self {
			Type::Builtin(BuiltinType::Int(_)) => true,
			Type::Builtin(BuiltinType::UInt(_)) => true,
			_ => false,
		}
	}

	pub fn is_float(&self) -> bool {
		match self {
			Type::Builtin(BuiltinType::Float(_)) => true,
			_ => false,
		}
	}
}