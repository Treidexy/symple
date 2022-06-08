use crate::compiler::{ Span };
use crate::parse::{ ModuleST, ExprST, ExprSTKind, BinOpST };

pub type TypeId = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum BuiltinType {
	Int(usize),
	UInt(usize),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
	Builtin(BuiltinType),
}

#[derive(Debug)]
pub struct Module {
	pub types: Vec<Type>,
	pub exprs: Vec<Expr>,
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
}


#[derive(Debug)]
pub struct Expr {
	pub kind: ExprKind,
	pub type_id: TypeId,
	pub span: Span,
}

pub struct Checker {
	module: Module,
}

impl Checker {
	pub fn check(module_st: &ModuleST) -> Module {
		let mut checker = Checker {
			module: Module {
				types: vec![],
				exprs: vec![],
			},
		};

		for expr_st in &module_st.exprs {
			let checked = checker.check_expr(expr_st);
			checker.module.exprs.push(checked);
		}

		checker.module
	}

	fn check_expr(&mut self, ast: &ExprST) -> Expr {
		match ast.kind {
			ExprSTKind::BinOp(op, ref left, ref right) => {
				let left = self.check_expr(left);
				let right = self.check_expr(right);
				let span = ast.span;
				let op = op.to_bin_op();
				let type_id = self.resolve_bin_op_type(op, left.type_id, right.type_id);

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
		}
	}

	fn resolve_bin_op_type(&mut self, _op: BinOp, left: TypeId, _right: TypeId) -> TypeId {
		left
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

impl BinOpST {
	pub fn to_bin_op(&self) -> BinOp {
		match self {
			BinOpST::Add => BinOp::Add,
			BinOpST::Sub => BinOp::Sub,
			BinOpST::Mul => BinOp::Mul,
			BinOpST::Div => BinOp::SDiv,
			BinOpST::Mod => BinOp::SMod,
		}
	}
}